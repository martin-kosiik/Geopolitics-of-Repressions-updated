Regression tables
================
Martin Kosík
April 21, 2019

``` r
knitr::opts_chunk$set(echo = TRUE)
library(stargazer)
library(tidyverse)
library(broom)
library(stargazer)
library(here)
library(readxl)
library(knitr)
library(kableExtra)
library(estimatr)
library(lubridate)
library(clubSandwich)
```

Import the data

``` r
ethnicity_controls <- read_excel(here::here("data/ethnicity_info.xlsx")) %>%
  mutate(ethnicity_id = as.numeric(1:n()),
         urb_rate_pct = urb_rate * 100)
  
min_by_year <-  read_csv(here::here("data/min_by_year_preds.csv")) %>% 
  mutate(log_n = log(1 + label ),
         log_n_pred_full = log(1 + label + pred_adj_full_scaled),
         log_n_pred = log(1 + label + prediction),
         log_n_imp_date = log(1 + label_imp_date),
         log_n_pred_full_imp_date = log(1 + label_imp_date + pred_adj_full_scaled_imp_date),
         n_pred_full_imp_date =  label_imp_date + pred_adj_full_scaled_imp_date,
         log_n_pred_full_imp_date_rehab = log(1 + label_rehab + pred_adj_full_scaled_rehab),
         log_n_pred_pars_imp_date = log(1 + label_imp_date + pred_adj_scaled_imp_date),
         log_n_pred_imp_date = log(1 + label_imp_date + prediction_imp_date)) %>% 
  left_join(ethnicity_controls, by = "ethnicity") %>% 
  mutate(german = ifelse(ethnicity == "German", 1, 0),
         post_german = german * ifelse(YEAR >= 1933, 1, 0),
         ethnicity = as.factor(ethnicity),
         YEAR_sq = YEAR^2, 
         pre_treatment = ifelse(YEAR >= 1922 & YEAR < 1933, 1, 0),
         hostility = ifelse(YEAR >= 1933 & YEAR <= 1939, 1, 0), 
         pact = ifelse(YEAR > 1939 & YEAR < 1941, 1, 0), 
         war = ifelse(YEAR >= 1941 & YEAR < 1945, 1, 0),
         post_war = ifelse(YEAR >= 1945, 1, 0)) 
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   ethnicity = col_character(),
    ##   log_n = col_double(),
    ##   log_n_imp_date = col_double(),
    ##   log_n_rehab = col_double()
    ## )

    ## See spec(...) for full column specifications.

Geopolitical controls - definition

``` r
min_by_year <- min_by_year %>% 
  mutate(geopol_finnish_war = as.numeric(YEAR >= 1939 & YEAR < 1945 & (ethnicity == "Finnish")),
         geopol_finnish_postwar = as.numeric(YEAR >=  1945 & (ethnicity == "Finnish")),
         geopol_baltic_anex = as.numeric(YEAR == 1940 & (ethnicity %in% c("Estonian", "Latvian", "Lithuanian"))),
         geopol_baltic_nazi = as.numeric(YEAR >= 1941 & YEAR <= 1943 &
                                           (ethnicity %in% c("Estonian", "Latvian", "Lithuanian"))),
         geopol_baltic_postwar = as.numeric(YEAR >= 1944 & 
                                           (ethnicity %in% c("Estonian", "Latvian", "Lithuanian"))),
         geopol_polish_war = as.numeric(YEAR == 1939 & (ethnicity == "Polish")),
         geopol_polish_soviet = as.numeric(YEAR == 1940 & (ethnicity  == "Polish")),
         geopol_polish_nazi = as.numeric(YEAR >= 1941 & YEAR < 1945 & (ethnicity == "Polish")),
         geopol_polish_postwar = as.numeric(YEAR >=  1945 & (ethnicity == "Polish")),
         geopol_japnese_war = as.numeric(YEAR >= 1938 & YEAR <= 1939 &  (ethnicity == "Japanese")),
         geopol_japnese_neutrality = as.numeric(YEAR >= 1938 & YEAR <= 1944 &  (ethnicity == "Japanese")),
         geopol_japnese_ww2 = as.numeric(YEAR == 1945 &  (ethnicity == "Japanese")),
         geopol_japnese_postwar = as.numeric(YEAR > 1945 &  (ethnicity == "Japanese")),
         geopol_hungarian_war = as.numeric(YEAR >= 1941 & YEAR < 1945 & (ethnicity == "Hungarian")),
         geopol_hungarian_postwar = as.numeric(YEAR >=  1945 & (ethnicity == "Hungarian")))
```

Year dummies - definition

``` r
years <- 1922:1960

year_dummies <- map_dfc(years, ~  if (dplyr::last(years) == .){
                                                    as.numeric(min_by_year$YEAR >= .)} else {
                                                    as.numeric(min_by_year$YEAR == .)}) %>% 
  rename_all(funs( c(paste0("year_" ,years))))

min_by_year <- bind_cols(min_by_year, year_dummies)
```

Define formulas

``` r
geopol_vars <- str_subset(names(min_by_year), "geopol")

fmla_pred_full_imp_date_no_trends_geopol <- as.formula(paste("log_n_pred_full_imp_date ~ ", 
                         paste0("german:","year_", years, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity+ ",  paste0(geopol_vars, collapse = " + ")))

fmla_pred_full_imp_date_no_trends_geopol_rehab <- as.formula(paste("log_n_pred_full_imp_date_rehab ~ ", 
                         paste0("german:","year_", years, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity+ ",  paste0(geopol_vars, collapse = " + ")))

fmla_pred_full_imp_date_no_trends <- as.formula(paste("log_n_pred_full_imp_date ~ ", 
                         paste0("german:","year_", years, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity"))
```

Fit the models

``` r
default_model <-  lm(fmla_pred_full_imp_date_no_trends_geopol, data = min_by_year)
rehabs_model <-  lm(fmla_pred_full_imp_date_no_trends_geopol_rehab, data = min_by_year)
no_ind_ethn_data <- min_by_year %>% 
  filter(ind_country == 0 | ethnicity == "German") 

no_ind_ethn_model <-  lm(data = no_ind_ethn_data, formula = fmla_pred_full_imp_date_no_trends)


robust_se <-  function(model,  vcov = "CR2",  level = 0.95, data = min_by_year){
  model %>% 
  conf_int(vcov = vcov,  level = level,
          cluster = data$ethnicity,  test = "Satterthwaite") %>% 
    pull(SE)
}

default_robust_se <- robust_se(default_model)
rehabs_robust_se <- robust_se(rehabs_model)
no_ind_ethn_robust_se <- robust_se(no_ind_ethn_model, data = no_ind_ethn_data)
```

Make the table

``` r
var_labels <- str_c("$",names(default_model$coefficients)) %>% 
  str_subset("german:year_") %>% 
  str_replace("german:year_", "\\\\beta\\_{") %>% 
  str_c("}$")


diff_table <-  stargazer(default_model, no_ind_ethn_model, rehabs_model, 
          keep = "german:year_+", omit.stat = c("ser", "f", "rsq"), label = "dif_table",
          se = list(default_robust_se, no_ind_ethn_robust_se, rehabs_robust_se), 
          covariate.labels = var_labels, title = "Difference-in-differences results",
          dep.var.labels.include = FALSE, single.row = T, header = F,
          font.size = "small", no.space = T,  dep.var.caption  = "Model",
          dep.var.labels = c("$\\log(1 + y_{it})$", "$\\log(1 + y_{it})$"), 
          add.lines = list(c("Eth. with ind. state excluded", "No", "Yes", "No"),
                           c("Only rehabilitated ind.", "No", "No", "Yes"), 
                           c("Geopol. relations controls", "Yes", "No", "Yes"),
                           c("Ethnicity-spec. time trends", "No", "No", "No")),
          table.placement = "!h",
          notes = "This will be replaced", notes.append = FALSE, notes.align = "l")

note_latex <- "\\multicolumn{4}{l} {\\parbox[t]{\\textwidth}{\\textit{Notes:} Cluster-robust standard errors are in the parentheses. The coefficients from model (1) are plotted in the figure \\ref{fig:did_effets}, from model (2) in the figure \\ref{fig:did_effets_no_ind_countries}, and from model (3) in the figure \\ref{fig:did_effects_rehabs}. For additional information, refer to the notes of the respective figures. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}}"

diff_table[grepl("Note",diff_table)] <- note_latex

diff_table %>% 
  cat(sep = '\n', file = here::here("tables/diff_table.tex"))
```

### Time trends

``` r
geopol_vars <- str_subset(names(min_by_year), "geopol")

fmla_pred_full_imp_date_lin_trends_geopol <- as.formula(paste("log_n_pred_full_imp_date ~ ", 
                         paste0("german:","year_", years, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity + ethnicity: YEAR +", paste0(geopol_vars, collapse = " + ")))

fmla_pred_full_imp_date_geopol <- as.formula(paste("log_n_pred_full_imp_date ~ ", 
                         paste0("german:","year_", years, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity + ethnicity: YEAR+ ethnicity: YEAR_sq + ",  paste0(geopol_vars, collapse = " + ")))


fmla_pred_full_imp_date_no_trends_geopol <- as.formula(paste("log_n_pred_full_imp_date ~ ", 
                         paste0("german:","year_", years, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity+ ",  paste0(geopol_vars, collapse = " + ")))
```

``` r
default_model <-  lm(fmla_pred_full_imp_date_no_trends_geopol, data = min_by_year)
lin_trends_model <-  lm(fmla_pred_full_imp_date_lin_trends_geopol, data = min_by_year)
quad_trends_model <-  lm(fmla_pred_full_imp_date_lin_trends_geopol, data = min_by_year)

robust_se_list <- map(list(default_model, lin_trends_model, quad_trends_model), robust_se)
```

``` r
diff_table2 <-  stargazer(default_model, lin_trends_model, quad_trends_model, 
          keep = "german:year_+", omit.stat = c("ser", "f", "rsq"), label = "dif_table_trends",
          se = robust_se_list, 
          covariate.labels = var_labels, title = "Difference-in-differences results - Ethnicity-specific time trends",
          dep.var.labels.include = FALSE, single.row = T, header = F,
          font.size = "small", no.space = T, 
          dep.var.labels = c("$\\log(1 + y_{it})$"),  dep.var.caption  = "Model",
          add.lines = list(c("Ethnicity-spec. time trends", "None", "Linear", "Quadratic"),
                           c("Eth. with ind. state excluded", "No", "Yes", "No"),
                           c("Geopol. relations controls", "Yes", "Yes", "Yes")),
          table.placement = "!h", 
         notes = "This will be replaced", notes.append = FALSE, notes.align = "l")

note_latex2 <- "\\multicolumn{4}{l} {\\parbox[t]{\\textwidth}{\\textit{Notes:} Cluster-robust standard errors are in the parentheses. The coefficients from these models are plotted in the figure \\ref{fig:did_robustness_time_trends}. For additional information, refer to the notes of the figures \\ref{fig:did_robustness_time_trends}. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}}"

diff_table2[grepl("Note",diff_table2)] <- note_latex2

diff_table2 %>% 
  cat(sep = '\n', file = here::here("tables/diff_table_trends.tex"))
```

### Ethnicity imputation adjustments

``` r
geopol_vars <- str_subset(names(min_by_year), "geopol")

fmla_pred_imp_date_no_trends_geopol <- as.formula(paste("log_n_pred_imp_date ~ ", 
                         paste0("german:","year_", years, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity+ ",  paste0(geopol_vars, collapse = " + ")))

fmla_pred_full_imp_date_no_trends_geopol <- as.formula(paste("log_n_pred_full_imp_date ~ ", 
                         paste0("german:","year_", years, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity+ ",  paste0(geopol_vars, collapse = " + ")))

fmla_pars_pred_imp_date_no_trends_geopol <- as.formula(paste("log_n_pred_pars_imp_date ~ ", 
                         paste0("german:","year_", years, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity+ ",  paste0(geopol_vars, collapse = " + ")))
```

``` r
pred_adj_formulas_full_years_geopol <- c(fmla_pred_full_imp_date_no_trends_geopol,
                                       fmla_pars_pred_imp_date_no_trends_geopol,
                                       fmla_pred_imp_date_no_trends_geopol)


pred_adj_formulas_full_years_geopol_lm<-map(pred_adj_formulas_full_years_geopol, ~lm(formula = ., data = min_by_year))

robust_se_list_pred_adj <- map(pred_adj_formulas_full_years_geopol_lm, robust_se)
```

``` r
diff_table3 <-  stargazer(pred_adj_formulas_full_years_geopol_lm, 
          keep = "german:year_+", omit.stat = c("ser", "f", "rsq"), label = "dif_table_pred_adj",
          se = robust_se_list_pred_adj, 
          covariate.labels = var_labels, title = "Difference-in-differences results - Ethnicity Imputation Adjustments",
          dep.var.labels.include = FALSE, single.row = T, header = F,
          font.size = "small", no.space = T, dep.var.caption  = "Model",
          add.lines = list(c("Ethnicity imputation adjust.", "Full-matrix", "Parsimonoius", "None"),
                           c("Ethnicity-spec. time trends", "None", "None", "None"),
                           c("Eth. with ind. state excluded", "No", "No", "No"),
                           c("Geopol. relations controls", "Yes", "Yes", "Yes")),
          table.placement = "!h", 
         notes = "This will be replaced", notes.append = FALSE, notes.align = "l")

note_latex3 <- "\\multicolumn{4}{l} {\\parbox[t]{\\textwidth}{\\textit{Notes:} Cluster-robust standard errors are in the parentheses. The coefficients from these models are plotted in the figure \\ref{fig:did_robustness_pred_adj}. For additional information, refer to the notes of the figures \\ref{fig:did_robustness_pred_adj}. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}}"

diff_table3[grepl("Note",diff_table3)] <- note_latex3

diff_table3 %>% 
  cat(sep = '\n', file = here::here("tables/diff_table_pred_adj.tex"))
```
