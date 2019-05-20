Heterogeneity by border frontiers
================
Martin Kosík
May 2, 2019

``` r
knitr::opts_chunk$set(echo = TRUE, fig.show = 'hide')
library(tidyverse)
library(broom)
library(stargazer)
library(here)
library(readxl)
library(knitr)
library(kableExtra)
library(estimatr)
library(gghighlight)
library(lubridate)
library(clubSandwich)
library(MSCMT)
library(timetk)
library(scales)
source(here::here("code/functions.R"))
```

Difference-in-differences
-------------------------

``` r
ethnicity_controls <- read_excel(here::here("data/ethnicity_info.xlsx")) %>%
  mutate(ethnicity_id = as.numeric(1:n()),
         urb_rate_pct = urb_rate * 100)


min_by_year <-  read_csv(here::here("data/min_by_year_borders.csv")) %>% 
  mutate(log_n_pred_full_non_border = log(1 + label_non_border + pred_adj_full_scaled_non_border),
         log_n_pred_full_border = log(1 + label_border + pred_adj_full_scaled_border)) %>% 
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
    ##   YEAR = col_integer(),
    ##   ethnicity = col_character(),
    ##   label_non_border = col_integer(),
    ##   prediction_non_border = col_integer(),
    ##   pred_adj_scaled_non_border = col_double(),
    ##   pred_adj_full_scaled_non_border = col_integer(),
    ##   n_non_border = col_integer(),
    ##   log_n_non_border = col_double(),
    ##   label_border = col_integer(),
    ##   prediction_border = col_integer(),
    ##   pred_adj_scaled_border = col_integer(),
    ##   pred_adj_full_scaled_border = col_integer(),
    ##   n_border = col_integer(),
    ##   log_n_border = col_double()
    ## )

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

``` r
years <- 1922:1960

year_dummies <- map_dfc(years, ~  if (dplyr::last(years) == .){
                                                    as.numeric(min_by_year$YEAR >= .)} else {
                                                    as.numeric(min_by_year$YEAR == .)}) %>% 
  rename_all(funs( c(paste0("year_" ,years))))

min_by_year <- bind_cols(min_by_year, year_dummies)
```

``` r
geopol_vars <- str_subset(names(min_by_year), "geopol")

border_no_trends_geopol <- as.formula(paste("log_n_border ~ ", 
                         paste0("german:","year_", years, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity+ ",  paste0(geopol_vars, collapse = " + ")))


non_border_no_trends_geopol <- as.formula(paste("log_n_non_border ~ ", 
                         paste0("german:","year_", years, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity+ ",  paste0(geopol_vars, collapse = " + ")))
```

``` r
plot_effects_robust_se(formula = border_no_trends_geopol, data = min_by_year) 
```

``` r
ggsave(here::here("plots/final/point_range_robust_cr2_border_provinces.pdf"))
```

    ## Saving 7 x 5 in image

``` r
plot_effects_robust_se(formula = border_no_trends_geopol, x_axis_breaks = seq(1922, 1960, 3), 
                       x_labels_angle = 0, x_labels_hjust = 0.5)
```

``` r
ggsave(here::here("plots/for_presentation/point_range_robust_cr2_border_provinces.pdf"), scale = 0.7)
```

    ## Saving 4.9 x 3.5 in image

``` r
plot_effects_robust_se(formula = non_border_no_trends_geopol, data = min_by_year) 
```

``` r
ggsave(here::here("plots/final/point_range_robust_cr2_non_border_provinces.pdf"))
```

    ## Saving 7 x 5 in image

``` r
plot_effects_robust_se(formula = non_border_no_trends_geopol, x_axis_breaks = seq(1922, 1960, 3), 
                       x_labels_angle = 0, x_labels_hjust = 0.5)
```

``` r
ggsave(here::here("plots/for_presentation/point_range_robust_cr2_non_border_provinces.pdf"), scale = 0.7)
```

    ## Saving 4.9 x 3.5 in image

``` r
fit_robust_model <- function(formula = fmla_window_pre_treat_pred_full_imp_date, 
                             data = min_by_year, level = 0.95, vcov = "CR2", estimatr = 1){
  if (estimatr != 1){
  model <- lm(formula,  data = data)
  model %>% 
  conf_int(vcov = vcov,  level = level,
          cluster = data$ethnicity,  test = "Satterthwaite") %>% 
  as_tibble(rownames = "term") %>% 
  filter(str_detect(term, "german:")) %>% 
 # mutate(nice_labs = str_replace(term,"german:","") %>% 
 #          fct_relevel("pre_treatment", "hostility", "pact", "war", "post_war")) %>% 
  rename(conf.low = CI_L, conf.high = CI_U, estimate = beta)
  }
  else {
    model_robust <- lm_robust(formula = formula, data = data, clusters = ethnicity, se_type = vcov,
                                ci = TRUE, alpha = 1 - level)
    model_robust %>% 
      estimatr::tidy(model_robust) %>% 
      filter(str_detect(term, "german:")) %>% 
      dplyr::select(term, estimate, SE = std.error,  conf.low, conf.high) %>% 
      as_tibble()
  }
  }
```

``` r
borders_formulas <- c(border_no_trends_geopol, non_border_no_trends_geopol)

## Position dodge
map(borders_formulas, ~ fit_robust_model(formula = ., vcov = "CR2", estimatr = 0)) %>% 
  bind_rows(.id = "id") %>% 
  mutate(nice_labs = str_replace(term,"german:year_","")
         #,nice_labs = ifelse(as.numeric(nice_labs) == max(as.numeric(nice_labs)), str_c(nice_labs, "+"), nice_labs)
         ) %>% 
  mutate(trends = recode_factor(id, "1" = "Border Region", "2" = "Non-border Region")) %>% 
  ggplot( aes(x = nice_labs, y = estimate, ymin = conf.low, ymax = conf.high, group = 1, col = trends))+ 
  geom_pointrange(position = position_dodge2(width = 0.45))+  geom_hline(yintercept= 0) + theme_minimal() + 
  theme(axis.line = element_line(size = 1), 
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=14),
        legend.position = "bottom", 
        axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "Year", 
        #caption = "error bars show 95% confidence intervals \n 
         #                      SE are based on the cluster-robust estimator by Pustejovsky and Tipton (2018)", 
       y = "Coefficient", col = "")
```

``` r
ggsave(here::here("plots/final/point_range_robust_cr2_border_compar.pdf"))
```

    ## Saving 7 x 5 in image

Synthetic control method
------------------------

``` r
ethnicity_controls <- read_excel(here::here("data/ethnicity_info.xlsx")) %>%
  mutate(ethnicity_id = as.numeric(1:n()),
         urb_rate_pct = urb_rate * 100)

min_by_year <-  read_csv(here::here("data/min_by_year_borders.csv")) %>% 
  mutate(log_n_pred_full_non_border = log(1 + label_non_border + pred_adj_full_scaled_non_border),
         log_n_pred_full_border = log(1 + label_border + pred_adj_full_scaled_border)) %>% 
  left_join(ethnicity_controls, by = "ethnicity") %>% 
  mutate(german = ifelse(ethnicity == "German", 1, 0),
         post_german = german * ifelse(YEAR >= 1933, 1, 0)) 
```

    ## Parsed with column specification:
    ## cols(
    ##   YEAR = col_integer(),
    ##   ethnicity = col_character(),
    ##   label_non_border = col_integer(),
    ##   prediction_non_border = col_integer(),
    ##   pred_adj_scaled_non_border = col_double(),
    ##   pred_adj_full_scaled_non_border = col_integer(),
    ##   n_non_border = col_integer(),
    ##   log_n_non_border = col_double(),
    ##   label_border = col_integer(),
    ##   prediction_border = col_integer(),
    ##   pred_adj_scaled_border = col_integer(),
    ##   pred_adj_full_scaled_border = col_integer(),
    ##   n_border = col_integer(),
    ##   log_n_border = col_double()
    ## )

``` r
data_prep_mscmt <- listFromLong(as.data.frame(min_by_year), unit.variable = "ethnicity_id", 
                                time.variable="YEAR", unit.names.variable="ethnicity")
```

``` r
dep_var <- "log_n_border"

treatment.identifier <- "German"
controls.identifier  <- setdiff(colnames(data_prep_mscmt[[1]]),
                                 treatment.identifier)
times.dep  <- cbind(dep_var                 = c("1921","1932"))
times.dep  <- cbind("log_n_border"                 = c("1921","1932"))

times.pred <- cbind("log_n_border"       = c("1921","1932"),
                    "pop_total"             = c("1921","1932"),
                    "clad_sim"              = c("1921","1932"),
                    "urb_rate"              = c("1921","1932"))

#agg.fns <- rep("mean", ncol(times.pred))                       
agg.fns <- rep("id", ncol(times.pred))
```

``` r
sc_placebo_border <- mscmt(data_prep_mscmt, treatment.identifier, controls.identifier, 
                              times.dep, times.pred, agg.fns,
                              seed=2019, placebo = TRUE, single.v=TRUE)
```

    ## 21:00:20: Starting placebo study, excluding original treated unit.
    ## 21:00:20: Using German as treated unit now.
    ## 21:00:20: Number of 'sunny' donors: 37 out of 37
    ## 21:00:20: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:20: with RMSPE 0.0977164750500378 and MSPE (loss v) 
    ## 21:00:20: 0.00954850949620466 is INFEASIBLE when respecting the 
    ## 21:00:20: predictors.
    ## 21:00:20: Starting optimization via DEoptC, random seed 2019.
    ## 21:00:24: Optimization finished (30961 calls to inner optimizer), rmspe: 
    ## 21:00:24: 0.0977164750504133, mspe: 0.00954850949627805.
    ## Final rmspe: 0.09771648, mspe (loss v): 0.009548509
    ## Optimal weights:
    ## Belorussian    Estonian   Kabardian      Korean        Mari     Mordvin 
    ##  0.02747539  0.06487403  0.11959373  0.30017826  0.12290509  0.12858288 
    ##    Ossetian     Russian       Tatar 
    ##  0.05869683  0.16150266  0.01619114 
    ## 
    ## 21:00:24: Using Altai as treated unit now.
    ## 21:00:24: Number of 'sunny' donors: 36 out of 36
    ## 21:00:24: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:24: with RMSPE 0.391447794365173 and MSPE (loss v) 0.153231375713359 
    ## 21:00:24: is INFEASIBLE when respecting the predictors.
    ## 21:00:24: Starting optimization via DEoptC, random seed 2019.
    ## 21:00:26: Optimization finished (27001 calls to inner optimizer), rmspe: 
    ## 21:00:26: 0.391447794365176, mspe: 0.153231375713361.
    ## Final rmspe: 0.3914478, mspe (loss v): 0.1532314
    ## Optimal weights:
    ##     Balkar    Chinese     Korean    Latvian       Mari    Mordvin 
    ## 0.31802809 0.01752013 0.06346222 0.12137537 0.31128868 0.16832551 
    ## 
    ## 21:00:26: Using Armenian as treated unit now.
    ## 21:00:26: Number of 'sunny' donors: 36 out of 36
    ## 21:00:26: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:26: with RMSPE 0.328072983762318 and MSPE (loss v) 0.10763188267471 
    ## 21:00:26: is INFEASIBLE when respecting the predictors.
    ## 21:00:27: Starting optimization via DEoptC, random seed 2019.
    ## 21:00:30: Optimization finished (30661 calls to inner optimizer), rmspe: 
    ## 21:00:30: 0.328072983762319, mspe: 0.107631882674711.
    ## Final rmspe: 0.328073, mspe (loss v): 0.1076319
    ## Optimal weights:
    ## Belorussian    Georgian     Chinese     Chuvash        Komi      Udmurt 
    ##  0.05843822  0.18460127  0.43813656  0.01031663  0.01497026  0.12267242 
    ##   Ukrainian       Yakut 
    ##  0.06002691  0.11083773 
    ## 
    ## 21:00:30: Using Balkar as treated unit now.
    ## 21:00:30: Number of 'sunny' donors: 36 out of 36
    ## 21:00:30: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:30: with RMSPE 0.306367332633383 and MSPE (loss v) 
    ## 21:00:30: 0.0938609425048937 is INFEASIBLE when respecting the predictors.
    ## 21:00:30: Starting optimization via DEoptC, random seed 2019.
    ## 21:00:32: Optimization finished (28561 calls to inner optimizer), rmspe: 
    ## 21:00:32: 0.306367332633456, mspe: 0.0938609425049385.
    ## Final rmspe: 0.3063673, mspe (loss v): 0.09386094
    ## Optimal weights:
    ##   Japanese     Kalmyk     Kazakh     Khakas    Mordvin      Tatar 
    ## 0.21022066 0.32828884 0.17441241 0.02282544 0.04723372 0.21701893 
    ## 
    ## 21:00:32: Using Bashkir as treated unit now.
    ## 21:00:32: Number of 'sunny' donors: 36 out of 36
    ## 21:00:32: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:32: with RMSPE 0.381934915936896 and MSPE (loss v) 0.145874280011724 
    ## 21:00:32: is INFEASIBLE when respecting the predictors.
    ## 21:00:32: Starting optimization via DEoptC, random seed 2019.
    ## 21:00:35: checking v: v contains NAs!
    ## 21:00:35: Optimization finished (28261 calls to inner optimizer), rmspe: 
    ## 21:00:35: 0.381934915936896, mspe: 0.145874280011724.
    ## Final rmspe: 0.3819349, mspe (loss v): 0.1458743
    ## Optimal weights:
    ##      Finnish        Greek      Chechen      Chuvash     Karelian 
    ## 0.3294252743 0.0641420242 0.0468036929 0.0003318391 0.4219520064 
    ##       Khakas 
    ## 0.1373451630 
    ## 
    ## 21:00:35: Using Belorussian as treated unit now.
    ## 21:00:35: Number of 'sunny' donors: 36 out of 36
    ## 21:00:35: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:35: is FEASIBLE even when respecting the predictors.
    ## Final rmspe: 0.5074218, mspe (loss v): 0.2574769
    ## Optimal weights:
    ##     Kazakh     Polish    Russian 
    ## 0.08766589 0.56467570 0.34765841 
    ## 
    ## 21:00:35: Using Bulgarian as treated unit now.
    ## 21:00:35: Number of 'sunny' donors: 36 out of 36
    ## 21:00:35: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:35: with RMSPE 0.527315851042401 and MSPE (loss v) 0.278062006760571 
    ## 21:00:35: is INFEASIBLE when respecting the predictors.
    ## 21:00:35: Starting optimization via DEoptC, random seed 2019.
    ## 21:00:37: Optimization finished (28561 calls to inner optimizer), rmspe: 
    ## 21:00:37: 0.527315851042426, mspe: 0.278062006760598.
    ## Final rmspe: 0.5273159, mspe (loss v): 0.278062
    ## Optimal weights:
    ##  Hungarian    Chinese     Kalmyk    Latvian   Moldovan     Udmurt 
    ## 0.05874807 0.14913148 0.17175449 0.07879365 0.05006651 0.48023749 
    ##      Uzbek 
    ## 0.01126831 
    ## 
    ## 21:00:37: Using Buryat as treated unit now.
    ## 21:00:37: Number of 'sunny' donors: 36 out of 36
    ## 21:00:37: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:37: with RMSPE 0.429845255570336 and MSPE (loss v) 0.184766943736327 
    ## 21:00:37: is INFEASIBLE when respecting the predictors.
    ## 21:00:37: Starting optimization via DEoptC, random seed 2019.
    ## 21:00:40: Optimization finished (32161 calls to inner optimizer), rmspe: 
    ## 21:00:40: 0.429845255570338, mspe: 0.184766943736329.
    ## Final rmspe: 0.4298453, mspe (loss v): 0.1847669
    ## Optimal weights:
    ## Belorussian     Finnish      Kazakh    Moldovan     Russian       Tatar 
    ## 0.083614678 0.138690605 0.203029368 0.392420246 0.017051410 0.002882674 
    ##      Udmurt 
    ## 0.162311020 
    ## 
    ## 21:00:40: Using Estonian as treated unit now.
    ## 21:00:40: Number of 'sunny' donors: 36 out of 36
    ## 21:00:40: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:40: with RMSPE 0.250675234687157 and MSPE (loss v) 
    ## 21:00:40: 0.0628380732854611 is INFEASIBLE when respecting the predictors.
    ## 21:00:40: Starting optimization via DEoptC, random seed 2019.
    ## 21:00:43: Optimization finished (37021 calls to inner optimizer), rmspe: 
    ## 21:00:43: 0.250675234687262, mspe: 0.0628380732855138.
    ## Final rmspe: 0.2506752, mspe (loss v): 0.06283807
    ## Optimal weights:
    ##      Buryat     Finnish       Greek    Moldovan      Polish     Russian 
    ## 0.016050770 0.005921409 0.037869367 0.078258554 0.228005010 0.063557231 
    ##       Tatar      Udmurt   Ukrainian 
    ## 0.437205767 0.075932757 0.057199135 
    ## 
    ## 21:00:43: Using Finnish as treated unit now.
    ## 21:00:43: Number of 'sunny' donors: 36 out of 36
    ## 21:00:43: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:43: with RMSPE 0.180970119407688 and MSPE (loss v) 0.032750184118433 
    ## 21:00:43: is INFEASIBLE when respecting the predictors.
    ## 21:00:44: Starting optimization via DEoptC, random seed 2019.
    ## 21:00:47: checking v: v contains NAs!
    ## 21:00:47: Optimization finished (28741 calls to inner optimizer), rmspe: 
    ## 21:00:47: 0.180970119407747, mspe: 0.0327501841184542.
    ## Final rmspe: 0.1809701, mspe (loss v): 0.03275018
    ## Optimal weights:
    ##    Bashkir     Buryat   Estonian   Japanese  Kabardian     Khakas 
    ## 0.33323175 0.21787637 0.07486710 0.15902573 0.07673249 0.04539606 
    ## Lithuanian    Russian 
    ## 0.05987645 0.03299404 
    ## 
    ## 21:00:47: Using Georgian as treated unit now.
    ## 21:00:47: Number of 'sunny' donors: 36 out of 36
    ## 21:00:47: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:47: with RMSPE 0.470263232332071 and MSPE (loss v) 0.221147507683407 
    ## 21:00:47: is INFEASIBLE when respecting the predictors.
    ## 21:00:47: Starting optimization via DEoptC, random seed 2019.
    ## 21:00:50: Optimization finished (28201 calls to inner optimizer), rmspe: 
    ## 21:00:50: 0.470263232332079, mspe: 0.221147507683415.
    ## Final rmspe: 0.4702632, mspe (loss v): 0.2211475
    ## Optimal weights:
    ##   Armenian     Buryat    Chinese   Karelian   Moldovan      Tatar 
    ## 0.15027850 0.04673686 0.30988254 0.26303369 0.11244848 0.01911703 
    ##     Udmurt 
    ## 0.09850290 
    ## 
    ## 21:00:50: Using Greek as treated unit now.
    ## 21:00:50: Number of 'sunny' donors: 36 out of 36
    ## 21:00:50: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:50: with RMSPE 0.428418232112765 and MSPE (loss v) 0.183542181606627 
    ## 21:00:50: is INFEASIBLE when respecting the predictors.
    ## 21:00:50: Starting optimization via DEoptC, random seed 2019.
    ## 21:00:52: Optimization finished (35041 calls to inner optimizer), rmspe: 
    ## 21:00:52: 0.428418232112779, mspe: 0.183542181606639.
    ## Final rmspe: 0.4284182, mspe (loss v): 0.1835422
    ## Optimal weights:
    ##  Hungarian     Khakas       Mari    Mordvin  Ukrainian      Yakut 
    ## 0.08554720 0.62377950 0.07595428 0.11118854 0.08876434 0.01476615 
    ## 
    ## 21:00:52: Using Hungarian as treated unit now.
    ## 21:00:53: Number of 'sunny' donors: 36 out of 36
    ## 21:00:53: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:53: with RMSPE 0.396280118904176 and MSPE (loss v) 0.157037932638708 
    ## 21:00:53: is INFEASIBLE when respecting the predictors.
    ## 21:00:53: Starting optimization via DEoptC, random seed 2019.
    ## 21:00:54: Optimization finished (24661 calls to inner optimizer), rmspe: 
    ## 21:00:54: 0.396280118904177, mspe: 0.157037932638708.
    ## Final rmspe: 0.3962801, mspe (loss v): 0.1570379
    ## Optimal weights:
    ##   Chechen    Kalmyk     Yakut 
    ## 0.1482255 0.4730429 0.3787316 
    ## 
    ## 21:00:54: Using Chechen as treated unit now.
    ## 21:00:54: Number of 'sunny' donors: 36 out of 36
    ## 21:00:54: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:54: with RMSPE 0.352780853698934 and MSPE (loss v) 0.124454330736549 
    ## 21:00:54: is INFEASIBLE when respecting the predictors.
    ## 21:00:54: Starting optimization via DEoptC, random seed 2019.
    ## 21:00:56: checking v: v contains NAs!
    ## 21:00:56: Optimization finished (24961 calls to inner optimizer), rmspe: 
    ## 21:00:56: 0.352780853698955, mspe: 0.124454330736563.
    ## Final rmspe: 0.3527809, mspe (loss v): 0.1244543
    ## Optimal weights:
    ##   Hungarian      Kalmyk     Mordvin       Yakut 
    ## 0.135748261 0.563190536 0.007865543 0.293195659 
    ## 
    ## 21:00:56: Using Chinese as treated unit now.
    ## 21:00:56: Number of 'sunny' donors: 36 out of 36
    ## 21:00:56: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:56: with RMSPE 0.421118096420942 and MSPE (loss v) 0.177340451133198 
    ## 21:00:56: is INFEASIBLE when respecting the predictors.
    ## 21:00:56: Starting optimization via DEoptC, random seed 2019.
    ## 21:00:58: Optimization finished (30361 calls to inner optimizer), rmspe: 
    ## 21:00:58: 0.421118096421185, mspe: 0.177340451133402.
    ## Final rmspe: 0.4211181, mspe (loss v): 0.1773405
    ## Optimal weights:
    ##   Armenian  Bulgarian   Georgian  Hungarian    Chechen      Uzbek 
    ## 0.25082833 0.01386276 0.28023979 0.12671465 0.21172933 0.11662514 
    ## 
    ## 21:00:58: Using Chuvash as treated unit now.
    ## 21:00:58: Number of 'sunny' donors: 36 out of 36
    ## 21:00:58: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:00:58: with RMSPE 0.708612769503895 and MSPE (loss v) 0.50213205710398 
    ## 21:00:58: is INFEASIBLE when respecting the predictors.
    ## 21:00:58: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:01: Optimization finished (26881 calls to inner optimizer), rmspe: 
    ## 21:01:01: 0.708612769503902, mspe: 0.50213205710399.
    ## Final rmspe: 0.7086128, mspe (loss v): 0.5021321
    ## Optimal weights:
    ##   Armenian   Georgian       Komi    Latvian   Ossetian    Russian 
    ## 0.14759747 0.06651739 0.26377880 0.12597232 0.16820901 0.02030288 
    ##     Udmurt      Yakut 
    ## 0.10778357 0.09983855 
    ## 
    ## 21:01:01: Using Japanese as treated unit now.
    ## 21:01:01: Number of 'sunny' donors: 36 out of 36
    ## 21:01:01: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:01: with RMSPE 0.22676912880002 and MSPE (loss v) 0.0514242377767202 
    ## 21:01:01: is INFEASIBLE when respecting the predictors.
    ## 21:01:01: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:03: Optimization finished (27121 calls to inner optimizer), rmspe: 
    ## 21:01:03: 0.226769128800033, mspe: 0.0514242377767258.
    ## Final rmspe: 0.2267691, mspe (loss v): 0.05142424
    ## Optimal weights:
    ##     Balkar    Bashkir     Buryat     Kazakh     Khakas      Uzbek 
    ## 0.01069540 0.23714033 0.04943810 0.05485329 0.05452686 0.59334602 
    ## 
    ## 21:01:03: Using Jewish as treated unit now.
    ## 21:01:03: Number of 'sunny' donors: 36 out of 36
    ## 21:01:03: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:03: with RMSPE 0.462963844332797 and MSPE (loss v) 0.214335521159402 
    ## 21:01:03: is INFEASIBLE when respecting the predictors.
    ## 21:01:03: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:05: Optimization finished (31801 calls to inner optimizer), rmspe: 
    ## 21:01:05: 0.462963844332821, mspe: 0.214335521159424.
    ## Final rmspe: 0.4629638, mspe (loss v): 0.2143355
    ## Optimal weights:
    ##   Georgian    Russian      Yakut 
    ## 0.06866184 0.51124119 0.42009697 
    ## 
    ## 21:01:05: Using Kabardian as treated unit now.
    ## 21:01:05: Number of 'sunny' donors: 36 out of 36
    ## 21:01:05: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:05: with RMSPE 0.426752293141247 and MSPE (loss v) 0.182117519701313 
    ## 21:01:05: is INFEASIBLE when respecting the predictors.
    ## 21:01:05: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:07: checking v: v contains NAs!
    ## 21:01:07: Optimization finished (28021 calls to inner optimizer), rmspe: 
    ## 21:01:07: 0.426752293141265, mspe: 0.182117519701328.
    ## Final rmspe: 0.4267523, mspe (loss v): 0.1821175
    ## Optimal weights:
    ## Belorussian      Buryat    Japanese      Khakas    Ossetian 
    ## 0.049149801 0.008761144 0.068617916 0.178572087 0.694899051 
    ## 
    ## 21:01:07: Using Kalmyk as treated unit now.
    ## 21:01:07: Number of 'sunny' donors: 36 out of 36
    ## 21:01:07: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:07: with RMSPE 0.250103892111064 and MSPE (loss v) 
    ## 21:01:07: 0.0625519568491028 is INFEASIBLE when respecting the predictors.
    ## 21:01:07: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:09: Optimization finished (28681 calls to inner optimizer), rmspe: 
    ## 21:01:09: 0.250103892111067, mspe: 0.0625519568491045.
    ## Final rmspe: 0.2501039, mspe (loss v): 0.06255196
    ## Optimal weights:
    ##     Balkar  Hungarian    Chechen   Karelian   Moldovan 
    ## 0.04930470 0.20431815 0.37572515 0.34585601 0.02479599 
    ## 
    ## 21:01:09: Using Karelian as treated unit now.
    ## 21:01:10: Number of 'sunny' donors: 36 out of 36
    ## 21:01:10: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:10: with RMSPE 0.114435270854612 and MSPE (loss v) 
    ## 21:01:10: 0.0130954312155684 is INFEASIBLE when respecting the predictors.
    ## 21:01:10: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:12: Optimization finished (29701 calls to inner optimizer), rmspe: 
    ## 21:01:12: 0.114435270854612, mspe: 0.0130954312155684.
    ## Final rmspe: 0.1144353, mspe (loss v): 0.01309543
    ## Optimal weights:
    ##    Bashkir   Georgian     Kalmyk     Udmurt 
    ## 0.05556872 0.08007012 0.42173737 0.44262379 
    ## 
    ## 21:01:12: Using Kazakh as treated unit now.
    ## 21:01:12: Number of 'sunny' donors: 36 out of 36
    ## 21:01:12: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:12: with RMSPE 0.87675088986454 and MSPE (loss v) 0.768692122878262 
    ## 21:01:12: is INFEASIBLE when respecting the predictors.
    ## 21:01:12: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:14: Optimization finished (33721 calls to inner optimizer), rmspe: 
    ## 21:01:14: 0.87675088986454, mspe: 0.768692122878262.
    ## Final rmspe: 0.8767509, mspe (loss v): 0.7686921
    ## Optimal weights:
    ## Belorussian   Kabardian        Mari       Tatar 
    ##   0.1680571   0.2206078   0.3740508   0.2372843 
    ## 
    ## 21:01:14: Using Khakas as treated unit now.
    ## 21:01:14: Number of 'sunny' donors: 36 out of 36
    ## 21:01:14: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:14: with RMSPE 0.382896840201811 and MSPE (loss v) 0.146609990236531 
    ## 21:01:14: is INFEASIBLE when respecting the predictors.
    ## 21:01:14: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:16: Optimization finished (27481 calls to inner optimizer), rmspe: 
    ## 21:01:16: 0.382896840201813, mspe: 0.146609990236533.
    ## Final rmspe: 0.3828968, mspe (loss v): 0.14661
    ## Optimal weights:
    ##      Greek  Kabardian    Mordvin    Russian 
    ## 0.45419251 0.33810096 0.19748662 0.01021991 
    ## 
    ## 21:01:16: Using Komi as treated unit now.
    ## 21:01:16: Number of 'sunny' donors: 36 out of 36
    ## 21:01:16: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:16: with RMSPE 0.492192381354092 and MSPE (loss v) 0.242253340263012 
    ## 21:01:16: is INFEASIBLE when respecting the predictors.
    ## 21:01:16: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:19: checking v: v contains NAs!
    ## 21:01:19: Optimization finished (27121 calls to inner optimizer), rmspe: 
    ## 21:01:19: 0.492192381354092, mspe: 0.242253340263012.
    ## Final rmspe: 0.4921924, mspe (loss v): 0.2422533
    ## Optimal weights:
    ##    Chechen    Chuvash     Jewish     Udmurt  Ukrainian      Yakut 
    ## 0.19069758 0.04820157 0.07269157 0.48884908 0.06585800 0.13370220 
    ## 
    ## 21:01:19: Using Korean as treated unit now.
    ## 21:01:19: Number of 'sunny' donors: 36 out of 36
    ## 21:01:19: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:19: with RMSPE 0.318951592531129 and MSPE (loss v) 0.101730118378143 
    ## 21:01:19: is INFEASIBLE when respecting the predictors.
    ## 21:01:19: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:21: Optimization finished (30181 calls to inner optimizer), rmspe: 
    ## 21:01:21: 0.318951592531497, mspe: 0.101730118378378.
    ## Final rmspe: 0.3189516, mspe (loss v): 0.1017301
    ## Optimal weights:
    ##      Altai  Bulgarian    Chinese     Jewish    Latvian       Mari 
    ## 0.34303430 0.02337723 0.12551402 0.08187227 0.01124356 0.13319971 
    ##    Russian 
    ## 0.28175891 
    ## 
    ## 21:01:21: Using Latvian as treated unit now.
    ## 21:01:21: Number of 'sunny' donors: 36 out of 36
    ## 21:01:21: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:21: with RMSPE 0.565803449023616 and MSPE (loss v) 0.32013354292702 
    ## 21:01:21: is INFEASIBLE when respecting the predictors.
    ## 21:01:21: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:23: Optimization finished (29701 calls to inner optimizer), rmspe: 
    ## 21:01:23: 0.565803449023618, mspe: 0.320133542927022.
    ## Final rmspe: 0.5658034, mspe (loss v): 0.3201335
    ## Optimal weights:
    ##       Altai Belorussian   Bulgarian    Ossetian       Yakut 
    ##  0.05930106  0.28894127  0.02422003  0.40994497  0.21759267 
    ## 
    ## 21:01:23: Using Lithuanian as treated unit now.
    ## 21:01:23: Number of 'sunny' donors: 36 out of 36
    ## 21:01:23: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:23: with RMSPE 0.275181353884878 and MSPE (loss v) 
    ## 21:01:23: 0.0757247775259143 is INFEASIBLE when respecting the predictors.
    ## 21:01:23: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:26: checking v: v contains NAs!
    ## 21:01:26: Optimization finished (30001 calls to inner optimizer), rmspe: 
    ## 21:01:26: 0.275181353886087, mspe: 0.0757247775265801.
    ## Final rmspe: 0.2751814, mspe (loss v): 0.07572478
    ## Optimal weights:
    ##  Bulgarian   Estonian    Chechen    Chinese     Korean     Polish 
    ## 0.10378120 0.20467089 0.36516222 0.02182822 0.13029374 0.07432011 
    ##      Tatar      Uzbek 
    ## 0.05984495 0.04009867 
    ## 
    ## 21:01:26: Using Mari as treated unit now.
    ## 21:01:26: Number of 'sunny' donors: 36 out of 36
    ## 21:01:26: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:26: with RMSPE 0.734800705729409 and MSPE (loss v) 0.539932077140438 
    ## 21:01:26: is INFEASIBLE when respecting the predictors.
    ## 21:01:26: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:28: Optimization finished (36241 calls to inner optimizer), rmspe: 
    ## 21:01:28: 0.734800705729409, mspe: 0.539932077140438.
    ## Final rmspe: 0.7348007, mspe (loss v): 0.5399321
    ## Optimal weights:
    ##      Greek  Hungarian     Kazakh     Korean 
    ## 0.02123235 0.10446517 0.34143162 0.53287085 
    ## 
    ## 21:01:29: Using Moldovan as treated unit now.
    ## 21:01:29: Number of 'sunny' donors: 36 out of 36
    ## 21:01:29: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:29: with RMSPE 0.571540528064044 and MSPE (loss v) 0.326658575219726 
    ## 21:01:29: is INFEASIBLE when respecting the predictors.
    ## 21:01:29: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:31: Optimization finished (36241 calls to inner optimizer), rmspe: 
    ## 21:01:31: 0.571540528064044, mspe: 0.326658575219726.
    ## Final rmspe: 0.5715405, mspe (loss v): 0.3266586
    ## Optimal weights:
    ##     Buryat  Hungarian     Jewish     Kalmyk 
    ## 0.38299813 0.03136681 0.08530449 0.50033056 
    ## 
    ## 21:01:31: Using Mordvin as treated unit now.
    ## 21:01:31: Number of 'sunny' donors: 36 out of 36
    ## 21:01:31: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:31: with RMSPE 0.614091807080963 and MSPE (loss v) 0.377108747523963 
    ## 21:01:31: is INFEASIBLE when respecting the predictors.
    ## 21:01:31: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:33: Optimization finished (28561 calls to inner optimizer), rmspe: 
    ## 21:01:33: 0.614091807080964, mspe: 0.377108747523964.
    ## Final rmspe: 0.6140918, mspe (loss v): 0.3771087
    ## Optimal weights:
    ##      Greek    Chechen     Khakas      Tatar 
    ## 0.14461282 0.09825127 0.16260695 0.59452896 
    ## 
    ## 21:01:33: Using Ossetian as treated unit now.
    ## 21:01:33: Number of 'sunny' donors: 36 out of 36
    ## 21:01:33: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:33: with RMSPE 0.301451677578817 and MSPE (loss v) 0.090873113915083 
    ## 21:01:33: is INFEASIBLE when respecting the predictors.
    ## 21:01:33: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:36: Optimization finished (28501 calls to inner optimizer), rmspe: 
    ## 21:01:36: 0.301451677578822, mspe: 0.0908731139150862.
    ## Final rmspe: 0.3014517, mspe (loss v): 0.09087311
    ## Optimal weights:
    ##    Chuvash  Kabardian     Kazakh    Latvian    Mordvin      Uzbek 
    ## 0.09369726 0.60005179 0.06695964 0.07094545 0.14299210 0.02535377 
    ## 
    ## 21:01:36: Using Polish as treated unit now.
    ## 21:01:36: Number of 'sunny' donors: 36 out of 36
    ## 21:01:36: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:36: with RMSPE 0.256151683924711 and MSPE (loss v) 
    ## 21:01:36: 0.0656136851774653 is INFEASIBLE when respecting the predictors.
    ## 21:01:36: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:39: Optimization finished (32761 calls to inner optimizer), rmspe: 
    ## 21:01:39: 0.256151683924831, mspe: 0.0656136851775264.
    ## Final rmspe: 0.2561517, mspe (loss v): 0.06561369
    ## Optimal weights:
    ## Belorussian       Greek  Lithuanian    Ossetian     Russian   Ukrainian 
    ##  0.52747656  0.05973431  0.09847300  0.04431336  0.07326808  0.16976556 
    ##       Yakut 
    ##  0.02696913 
    ## 
    ## 21:01:39: Using Russian as treated unit now.
    ## 21:01:39: Number of 'sunny' donors: 36 out of 36
    ## 21:01:39: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:39: is FEASIBLE even when respecting the predictors.
    ## Final rmspe: 2.075721, mspe (loss v): 4.308616
    ## Optimal weights:
    ## Belorussian 
    ##           1 
    ## 
    ## 21:01:39: Using Tatar as treated unit now.
    ## 21:01:39: Number of 'sunny' donors: 36 out of 36
    ## 21:01:39: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:39: with RMSPE 0.396033431293087 and MSPE (loss v) 0.156842478701776 
    ## 21:01:39: is INFEASIBLE when respecting the predictors.
    ## 21:01:39: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:41: checking v: v contains NAs!
    ## 21:01:41: Optimization finished (28081 calls to inner optimizer), rmspe: 
    ## 21:01:41: 0.396033431293088, mspe: 0.156842478701777.
    ## Final rmspe: 0.3960334, mspe (loss v): 0.1568425
    ## Optimal weights:
    ##       Balkar       Buryat     Estonian       Kazakh      Mordvin 
    ## 0.1238011707 0.0002983641 0.2911711284 0.0803545736 0.4006826167 
    ##       Udmurt        Uzbek 
    ## 0.0793205118 0.0243716347 
    ## 
    ## 21:01:41: Using Udmurt as treated unit now.
    ## 21:01:41: Number of 'sunny' donors: 36 out of 36
    ## 21:01:41: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:41: with RMSPE 0.416315296098703 and MSPE (loss v) 0.173318425765751 
    ## 21:01:41: is INFEASIBLE when respecting the predictors.
    ## 21:01:42: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:44: checking v: v contains NAs!
    ## 21:01:44: Optimization finished (28261 calls to inner optimizer), rmspe: 
    ## 21:01:44: 0.416315296098709, mspe: 0.173318425765755.
    ## Final rmspe: 0.4163153, mspe (loss v): 0.1733184
    ## Optimal weights:
    ##    Armenian Belorussian   Bulgarian    Karelian        Komi 
    ## 0.004711966 0.010089066 0.196626392 0.649998633 0.138573943 
    ## 
    ## 21:01:44: Using Ukrainian as treated unit now.
    ## 21:01:44: Number of 'sunny' donors: 36 out of 36
    ## 21:01:44: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:44: with RMSPE 0.62298569593512 and MSPE (loss v) 0.388111177339766 
    ## 21:01:44: is INFEASIBLE when respecting the predictors.
    ## 21:01:44: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:47: Optimization finished (34381 calls to inner optimizer), rmspe: 
    ## 21:01:47: 0.622985695935248, mspe: 0.388111177339925.
    ## Final rmspe: 0.6229857, mspe (loss v): 0.3881112
    ## Optimal weights:
    ##       Greek     Chuvash        Komi      Polish     Russian 
    ## 0.211714523 0.113793901 0.003694244 0.613870949 0.056926383 
    ## 
    ## 21:01:47: Using Uzbek as treated unit now.
    ## 21:01:47: Number of 'sunny' donors: 36 out of 36
    ## 21:01:47: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:47: with RMSPE 0.347782617356869 and MSPE (loss v) 0.120952748935594 
    ## 21:01:47: is INFEASIBLE when respecting the predictors.
    ## 21:01:47: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:49: Optimization finished (26101 calls to inner optimizer), rmspe: 
    ## 21:01:49: 0.347782617357007, mspe: 0.120952748935691.
    ## Final rmspe: 0.3477826, mspe (loss v): 0.1209527
    ## Optimal weights:
    ##  Bulgarian    Chechen    Chinese   Japanese   Ossetian 
    ## 0.05822294 0.01217678 0.18627368 0.59502722 0.14829937 
    ## 
    ## 21:01:49: Using Yakut as treated unit now.
    ## 21:01:49: Number of 'sunny' donors: 36 out of 36
    ## 21:01:49: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:49: with RMSPE 0.549871118760903 and MSPE (loss v) 0.302358247247367 
    ## 21:01:49: is INFEASIBLE when respecting the predictors.
    ## 21:01:49: Starting optimization via DEoptC, random seed 2019.
    ## 21:01:51: checking v: v contains NAs!
    ## 21:01:51: Optimization finished (30361 calls to inner optimizer), rmspe: 
    ## 21:01:51: 0.549871118761059, mspe: 0.302358247247539.
    ## Final rmspe: 0.5498711, mspe (loss v): 0.3023582
    ## Optimal weights:
    ##  Hungarian    Chechen    Chuvash     Jewish       Komi 
    ## 0.65519417 0.13440123 0.04090401 0.02968379 0.13981680

``` r
placebo_highlight_all(sc_placebo_border, "log_n_border")
```

    ## Warning: You set use_group_by = TRUE, but grouped calculation failed.
    ## Falling back to ungrouped filter operation...

    ## label_key: ethnicity

``` r
ggsave(here::here("plots/final/placebo_highlight_all_border_provinces.pdf"))
```

    ## Saving 7 x 5 in image

``` r
ggsave(here::here("plots/for_presentation/placebo_highlight_all_border_provinces.pdf"), scale = 0.7)
```

    ## Saving 4.9 x 3.5 in image

``` r
placebo_highlight_mspe(sc_placebo_border, "log_n_border", exclusion_ratio = 20)
```

    ## Warning: You set use_group_by = TRUE, but grouped calculation failed.
    ## Falling back to ungrouped filter operation...

    ## label_key: ethnicity

``` r
placebo_mspe_barplot(sc_placebo_border, "log_n_border", range_post = c("1933", "1960"))
```

``` r
ggsave(here::here("plots/final/mspe_ratios_border_provinces.pdf"))
```

    ## Saving 7 x 5 in image

``` r
placebo_mspe_barplot(sc_placebo_border, "log_n_border", range_post = c("1933", "1939"))
```

``` r
ggsave(here::here("plots/final/mspe_ratios_border_provinces_until_1939.pdf"))
```

    ## Saving 7 x 5 in image

``` r
dep_var <- "log_n_non_border"
times.dep  <- cbind("log_n_non_border"      = c("1921","1932"))

times.pred <- cbind("log_n_non_border"      = c("1921","1932"),
                    "pop_total"             = c("1921","1932"),
                    "clad_sim"              = c("1921","1932"),
                    "urb_rate"              = c("1921","1932"))

sc_placebo_non_border <- mscmt(data_prep_mscmt, treatment.identifier, controls.identifier, 
                              times.dep, times.pred, agg.fns,
                              seed=2019, placebo = TRUE, single.v=TRUE)
```

    ## 21:01:57: Starting placebo study, excluding original treated unit.
    ## 21:01:57: Using German as treated unit now.
    ## 21:01:57: Number of 'sunny' donors: 37 out of 37
    ## 21:01:57: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:01:57: with RMSPE 0.227847440658581 and MSPE (loss v) 
    ## 21:01:57: 0.0519144562146656 is INFEASIBLE when respecting the predictors.
    ## 21:01:57: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:02: Optimization finished (27241 calls to inner optimizer), rmspe: 
    ## 21:02:02: 0.227847440660925, mspe: 0.0519144562157336.
    ## Final rmspe: 0.2278474, mspe (loss v): 0.05191446
    ## Optimal weights:
    ##      Greek    Chuvash     Korean Lithuanian   Moldovan    Russian 
    ## 0.17078581 0.07424741 0.00899821 0.10065543 0.06686319 0.18562484 
    ##  Ukrainian      Yakut 
    ## 0.37014237 0.02268274 
    ## 
    ## 21:02:02: Using Altai as treated unit now.
    ## 21:02:02: Number of 'sunny' donors: 36 out of 36
    ## 21:02:02: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:02: with RMSPE 0.362993369267773 and MSPE (loss v) 0.13176418613237 
    ## 21:02:02: is INFEASIBLE when respecting the predictors.
    ## 21:02:02: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:04: Optimization finished (27421 calls to inner optimizer), rmspe: 
    ## 21:02:04: 0.362993369267778, mspe: 0.131764186132374.
    ## Final rmspe: 0.3629934, mspe (loss v): 0.1317642
    ## Optimal weights:
    ##      Balkar   Bulgarian    Estonian   Hungarian     Mordvin    Ossetian 
    ## 0.632097407 0.084847227 0.092087436 0.028030386 0.131205338 0.005349661 
    ##      Udmurt 
    ## 0.026382546 
    ## 
    ## 21:02:04: Using Armenian as treated unit now.
    ## 21:02:05: Number of 'sunny' donors: 36 out of 36
    ## 21:02:05: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:05: with RMSPE 0.191213943466135 and MSPE (loss v) 
    ## 21:02:05: 0.0365627721758701 is INFEASIBLE when respecting the predictors.
    ## 21:02:05: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:07: checking v: v contains NAs!
    ## 21:02:07: Optimization finished (27361 calls to inner optimizer), rmspe: 
    ## 21:02:07: 0.191213943466205, mspe: 0.036562772175897.
    ## Final rmspe: 0.1912139, mspe (loss v): 0.03656277
    ## Optimal weights:
    ##      Buryat     Finnish     Chinese     Chuvash        Komi     Mordvin 
    ## 0.199946118 0.036916009 0.158628826 0.058020624 0.089661989 0.007565284 
    ##    Ossetian       Yakut 
    ## 0.212355572 0.236905579 
    ## 
    ## 21:02:07: Using Balkar as treated unit now.
    ## 21:02:07: Number of 'sunny' donors: 36 out of 36
    ## 21:02:07: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:07: with RMSPE 0.271502267957274 and MSPE (loss v) 
    ## 21:02:07: 0.0737134815059435 is INFEASIBLE when respecting the predictors.
    ## 21:02:08: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:09: Optimization finished (29221 calls to inner optimizer), rmspe: 
    ## 21:02:09: 0.271502267957321, mspe: 0.0737134815059689.
    ## Final rmspe: 0.2715023, mspe (loss v): 0.07371348
    ## Optimal weights:
    ##      Altai   Japanese     Kalmyk   Ossetian      Uzbek 
    ## 0.33428808 0.10198729 0.33583304 0.02048769 0.20740391 
    ## 
    ## 21:02:09: Using Bashkir as treated unit now.
    ## 21:02:09: Number of 'sunny' donors: 36 out of 36
    ## 21:02:09: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:09: with RMSPE 0.20790875055954 and MSPE (loss v) 0.043226048559229 
    ## 21:02:09: is INFEASIBLE when respecting the predictors.
    ## 21:02:09: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:12: Optimization finished (33301 calls to inner optimizer), rmspe: 
    ## 21:02:12: 0.207908750559541, mspe: 0.0432260485592295.
    ## Final rmspe: 0.2079088, mspe (loss v): 0.04322605
    ## Optimal weights:
    ##       Greek      Korean     Mordvin       Tatar      Udmurt       Uzbek 
    ## 0.115844608 0.007389163 0.098999078 0.281356118 0.441665916 0.054745117 
    ## 
    ## 21:02:12: Using Belorussian as treated unit now.
    ## 21:02:12: Number of 'sunny' donors: 36 out of 36
    ## 21:02:12: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:12: with RMSPE 0.313661928091989 and MSPE (loss v) 
    ## 21:02:12: 0.0983838051343841 is INFEASIBLE when respecting the predictors.
    ## 21:02:12: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:15: checking v: v contains NAs!
    ## 21:02:15: Optimization finished (26881 calls to inner optimizer), rmspe: 
    ## 21:02:15: 0.31366192809199, mspe: 0.0983838051343845.
    ## Final rmspe: 0.3136619, mspe (loss v): 0.09838381
    ## Optimal weights:
    ##   Hungarian     Chinese    Karelian        Komi        Mari     Mordvin 
    ## 0.007511393 0.047546756 0.155224988 0.249310954 0.367511730 0.021937953 
    ##       Yakut 
    ## 0.150956226 
    ## 
    ## 21:02:15: Using Bulgarian as treated unit now.
    ## 21:02:15: Number of 'sunny' donors: 36 out of 36
    ## 21:02:15: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:15: with RMSPE 0.219695481248905 and MSPE (loss v) 
    ## 21:02:15: 0.0482661044811879 is INFEASIBLE when respecting the predictors.
    ## 21:02:15: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:17: Optimization finished (28081 calls to inner optimizer), rmspe: 
    ## 21:02:17: 0.219695481248949, mspe: 0.0482661044812072.
    ## Final rmspe: 0.2196955, mspe (loss v): 0.0482661
    ## Optimal weights:
    ##      Altai    Finnish      Greek  Kabardian   Moldovan      Uzbek 
    ## 0.07232161 0.49626859 0.03472278 0.06478080 0.22739862 0.10450760 
    ## 
    ## 21:02:17: Using Buryat as treated unit now.
    ## 21:02:17: Number of 'sunny' donors: 36 out of 36
    ## 21:02:17: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:17: with RMSPE 0.368406898793851 and MSPE (loss v) 0.135723643078903 
    ## 21:02:17: is INFEASIBLE when respecting the predictors.
    ## 21:02:18: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:20: Optimization finished (30061 calls to inner optimizer), rmspe: 
    ## 21:02:20: 0.368406898793856, mspe: 0.135723643078907.
    ## Final rmspe: 0.3684069, mspe (loss v): 0.1357236
    ## Optimal weights:
    ##     Balkar    Chinese    Chuvash     Kalmyk   Moldovan   Ossetian 
    ## 0.07412976 0.02912146 0.16088461 0.30079418 0.02235821 0.20186672 
    ##      Uzbek 
    ## 0.21084505 
    ## 
    ## 21:02:20: Using Estonian as treated unit now.
    ## 21:02:20: Number of 'sunny' donors: 36 out of 36
    ## 21:02:20: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:20: with RMSPE 0.547512129833816 and MSPE (loss v) 0.299769532315161 
    ## 21:02:20: is INFEASIBLE when respecting the predictors.
    ## 21:02:20: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:23: checking v: v contains NAs!
    ## 21:02:23: Optimization finished (27121 calls to inner optimizer), rmspe: 
    ## 21:02:23: 0.547512129833817, mspe: 0.299769532315163.
    ## Final rmspe: 0.5475121, mspe (loss v): 0.2997695
    ## Optimal weights:
    ##  Hungarian  Kabardian   Karelian     Korean       Mari 
    ## 0.46142652 0.09113683 0.04592436 0.34209441 0.05941787 
    ## 
    ## 21:02:23: Using Finnish as treated unit now.
    ## 21:02:23: Number of 'sunny' donors: 36 out of 36
    ## 21:02:23: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:23: with RMSPE 0.295049097449279 and MSPE (loss v) 
    ## 21:02:23: 0.0870539699056341 is INFEASIBLE when respecting the predictors.
    ## 21:02:23: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:26: Optimization finished (31561 calls to inner optimizer), rmspe: 
    ## 21:02:26: 0.295049097449336, mspe: 0.0870539699056677.
    ## Final rmspe: 0.2950491, mspe (loss v): 0.08705397
    ## Optimal weights:
    ##  Bulgarian    Chinese   Japanese     Khakas 
    ## 0.60832124 0.04279316 0.10921653 0.23966908 
    ## 
    ## 21:02:26: Using Georgian as treated unit now.
    ## 21:02:26: Number of 'sunny' donors: 36 out of 36
    ## 21:02:26: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:26: with RMSPE 0.205989366889168 and MSPE (loss v) 
    ## 21:02:26: 0.0424316192714004 is INFEASIBLE when respecting the predictors.
    ## 21:02:26: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:29: Optimization finished (32281 calls to inner optimizer), rmspe: 
    ## 21:02:29: 0.205989366889245, mspe: 0.0424316192714321.
    ## Final rmspe: 0.2059894, mspe (loss v): 0.04243162
    ## Optimal weights:
    ##        Greek    Hungarian      Chuvash       Jewish    Kabardian 
    ## 0.0852899092 0.1196462885 0.0051039000 0.1128120890 0.2471999151 
    ##       Kalmyk     Karelian   Lithuanian     Moldovan     Ossetian 
    ## 0.0279051420 0.0574936041 0.0006327501 0.3166768555 0.0272395466 
    ## 
    ## 21:02:29: Using Greek as treated unit now.
    ## 21:02:30: Number of 'sunny' donors: 36 out of 36
    ## 21:02:30: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:30: with RMSPE 0.588211774170562 and MSPE (loss v) 0.34599309127288 
    ## 21:02:30: is INFEASIBLE when respecting the predictors.
    ## 21:02:30: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:33: Optimization finished (33661 calls to inner optimizer), rmspe: 
    ## 21:02:33: 0.588211774170594, mspe: 0.345993091272918.
    ## Final rmspe: 0.5882118, mspe (loss v): 0.3459931
    ## Optimal weights:
    ##    Bashkir  Bulgarian    Chechen    Chuvash     Khakas     Korean 
    ## 0.57028909 0.02659587 0.09587603 0.07453751 0.02414773 0.12840397 
    ##  Ukrainian      Uzbek 
    ## 0.04751351 0.03263630 
    ## 
    ## 21:02:33: Using Hungarian as treated unit now.
    ## 21:02:33: Number of 'sunny' donors: 36 out of 36
    ## 21:02:33: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:33: with RMSPE 0.845437256126247 and MSPE (loss v) 0.714764154046277 
    ## 21:02:33: is INFEASIBLE when respecting the predictors.
    ## 21:02:33: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:35: Optimization finished (30181 calls to inner optimizer), rmspe: 
    ## 21:02:35: 0.84543725612628, mspe: 0.714764154046333.
    ## Final rmspe: 0.8454373, mspe (loss v): 0.7147642
    ## Optimal weights:
    ##    Chinese     Jewish     Kalmyk 
    ## 0.26212480 0.08675004 0.65112517 
    ## 
    ## 21:02:35: Using Chechen as treated unit now.
    ## 21:02:35: Number of 'sunny' donors: 36 out of 36
    ## 21:02:35: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:35: with RMSPE 0.385952333010401 and MSPE (loss v) 0.148959203356171 
    ## 21:02:35: is INFEASIBLE when respecting the predictors.
    ## 21:02:35: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:38: Optimization finished (30061 calls to inner optimizer), rmspe: 
    ## 21:02:38: 0.385952333010442, mspe: 0.148959203356203.
    ## Final rmspe: 0.3859523, mspe (loss v): 0.1489592
    ## Optimal weights:
    ##  Bulgarian     Buryat      Greek    Chuvash   Japanese     Kalmyk 
    ## 0.25328408 0.03446101 0.22514052 0.05172736 0.02179602 0.13203331 
    ##   Karelian    Mordvin 
    ## 0.14876903 0.13278866 
    ## 
    ## 21:02:38: Using Chinese as treated unit now.
    ## 21:02:39: Number of 'sunny' donors: 36 out of 36
    ## 21:02:39: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:39: with RMSPE 0.915769230004917 and MSPE (loss v) 0.838633282623799 
    ## 21:02:39: is INFEASIBLE when respecting the predictors.
    ## 21:02:39: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:40: Optimization finished (25261 calls to inner optimizer), rmspe: 
    ## 21:02:40: 0.915769230004971, mspe: 0.838633282623898.
    ## Final rmspe: 0.9157692, mspe (loss v): 0.8386333
    ## Optimal weights:
    ## Hungarian    Kalmyk  Karelian  Ossetian 
    ## 0.3090328 0.2411844 0.2034414 0.2463414 
    ## 
    ## 21:02:40: Using Chuvash as treated unit now.
    ## 21:02:40: Number of 'sunny' donors: 36 out of 36
    ## 21:02:40: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:40: with RMSPE 0.836190546284359 and MSPE (loss v) 0.699214629695334 
    ## 21:02:40: is INFEASIBLE when respecting the predictors.
    ## 21:02:40: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:43: Optimization finished (27061 calls to inner optimizer), rmspe: 
    ## 21:02:43: 0.836190546284386, mspe: 0.699214629695381.
    ## Final rmspe: 0.8361905, mspe (loss v): 0.6992146
    ## Optimal weights:
    ##     Buryat      Greek    Mordvin  Ukrainian      Uzbek 
    ## 0.26424514 0.07200083 0.25310326 0.39050496 0.02014582 
    ## 
    ## 21:02:43: Using Japanese as treated unit now.
    ## 21:02:43: Number of 'sunny' donors: 36 out of 36
    ## 21:02:43: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:43: with RMSPE 0.241752683357644 and MSPE (loss v) 
    ## 21:02:43: 0.0584443599106214 is INFEASIBLE when respecting the predictors.
    ## 21:02:43: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:45: checking v: v contains NAs!
    ## 21:02:45: Optimization finished (25261 calls to inner optimizer), rmspe: 
    ## 21:02:45: 0.241752683358059, mspe: 0.0584443599108218.
    ## Final rmspe: 0.2417527, mspe (loss v): 0.05844436
    ## Optimal weights:
    ##     Balkar     Buryat    Finnish    Chinese     Korean    Mordvin 
    ## 0.33050643 0.03492139 0.27098269 0.04181060 0.16002715 0.10778997 
    ##   Ossetian 
    ## 0.05396177 
    ## 
    ## 21:02:45: Using Jewish as treated unit now.
    ## 21:02:45: Number of 'sunny' donors: 36 out of 36
    ## 21:02:45: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:45: with RMSPE 0.721751131045523 and MSPE (loss v) 0.520924695165492 
    ## 21:02:45: is INFEASIBLE when respecting the predictors.
    ## 21:02:45: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:48: Optimization finished (31381 calls to inner optimizer), rmspe: 
    ## 21:02:48: 0.721751131045538, mspe: 0.520924695165513.
    ## Final rmspe: 0.7217511, mspe (loss v): 0.5209247
    ## Optimal weights:
    ##  Hungarian   Karelian    Russian 
    ## 0.55417798 0.04692986 0.39889216 
    ## 
    ## 21:02:48: Using Kabardian as treated unit now.
    ## 21:02:48: Number of 'sunny' donors: 36 out of 36
    ## 21:02:48: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:48: with RMSPE 0.428942636929588 and MSPE (loss v) 0.183991785776109 
    ## 21:02:48: is INFEASIBLE when respecting the predictors.
    ## 21:02:48: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:50: Optimization finished (28981 calls to inner optimizer), rmspe: 
    ## 21:02:50: 0.428942636929599, mspe: 0.183991785776117.
    ## Final rmspe: 0.4289426, mspe (loss v): 0.1839918
    ## Optimal weights:
    ##  Bulgarian   Estonian    Chinese    Chuvash   Ossetian      Uzbek 
    ## 0.23055043 0.11834940 0.03245565 0.08270200 0.10152408 0.30556780 
    ##      Yakut 
    ## 0.12885063 
    ## 
    ## 21:02:50: Using Kalmyk as treated unit now.
    ## 21:02:51: Number of 'sunny' donors: 36 out of 36
    ## 21:02:51: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:51: with RMSPE 0.641678741483983 and MSPE (loss v) 0.411751607272469 
    ## 21:02:51: is INFEASIBLE when respecting the predictors.
    ## 21:02:51: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:52: Optimization finished (27361 calls to inner optimizer), rmspe: 
    ## 21:02:52: 0.641678741483983, mspe: 0.411751607272468.
    ## Final rmspe: 0.6416787, mspe (loss v): 0.4117516
    ## Optimal weights:
    ## Hungarian   Chinese  Moldovan  Ossetian 
    ## 0.3419107 0.1752773 0.1930220 0.2897900 
    ## 
    ## 21:02:52: Using Karelian as treated unit now.
    ## 21:02:53: Number of 'sunny' donors: 36 out of 36
    ## 21:02:53: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:53: with RMSPE 1.1230422877722 and MSPE (loss v) 1.26122398012463 is 
    ## 21:02:53: INFEASIBLE when respecting the predictors.
    ## 21:02:53: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:54: Optimization finished (25261 calls to inner optimizer), rmspe: 
    ## 21:02:54: 1.12304228777226, mspe: 1.26122398012475.
    ## Final rmspe: 1.123042, mspe (loss v): 1.261224
    ## Optimal weights:
    ##    Chechen    Chinese     Jewish    Russian 
    ## 0.03835138 0.70652927 0.06867606 0.18644329 
    ## 
    ## 21:02:54: Using Kazakh as treated unit now.
    ## 21:02:55: Number of 'sunny' donors: 36 out of 36
    ## 21:02:55: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:55: with RMSPE 0.128507301246472 and MSPE (loss v) 
    ## 21:02:55: 0.0165141264736514 is INFEASIBLE when respecting the predictors.
    ## 21:02:55: Starting optimization via DEoptC, random seed 2019.
    ## 21:02:58: Optimization finished (35881 calls to inner optimizer), rmspe: 
    ## 21:02:58: 0.128507301246472, mspe: 0.0165141264736515.
    ## Final rmspe: 0.1285073, mspe (loss v): 0.01651413
    ## Optimal weights:
    ##   Estonian    Chuvash  Kabardian       Mari   Moldovan    Mordvin 
    ## 0.04078153 0.13424248 0.03906836 0.41718097 0.05248104 0.03090595 
    ##      Tatar 
    ## 0.28533966 
    ## 
    ## 21:02:58: Using Khakas as treated unit now.
    ## 21:02:58: Number of 'sunny' donors: 36 out of 36
    ## 21:02:58: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:02:58: with RMSPE 0.115709605339742 and MSPE (loss v) 
    ## 21:02:58: 0.0133887127678788 is INFEASIBLE when respecting the predictors.
    ## 21:02:58: Starting optimization via DEoptC, random seed 2019.
    ## 21:03:01: Optimization finished (31141 calls to inner optimizer), rmspe: 
    ## 21:03:01: 0.115709605339746, mspe: 0.0133887127678798.
    ## Final rmspe: 0.1157096, mspe (loss v): 0.01338871
    ## Optimal weights:
    ##     Finnish       Greek     Chuvash      Korean     Mordvin   Ukrainian 
    ## 0.490083925 0.059130988 0.029563998 0.241092267 0.170646574 0.009482248 
    ## 
    ## 21:03:01: Using Komi as treated unit now.
    ## 21:03:01: Number of 'sunny' donors: 36 out of 36
    ## 21:03:01: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:01: with RMSPE 0.648689839183725 and MSPE (loss v) 0.420798507460207 
    ## 21:03:01: is INFEASIBLE when respecting the predictors.
    ## 21:03:01: Starting optimization via DEoptC, random seed 2019.
    ## 21:03:04: Optimization finished (29281 calls to inner optimizer), rmspe: 
    ## 21:03:04: 0.6486898391839, mspe: 0.420798507460435.
    ## Final rmspe: 0.6486898, mspe (loss v): 0.4207985
    ## Optimal weights:
    ## Belorussian     Chuvash      Jewish     Latvian    Moldovan     Mordvin 
    ## 0.332600583 0.059216956 0.075797795 0.191605969 0.053589351 0.008370252 
    ##      Polish 
    ## 0.278819093 
    ## 
    ## 21:03:04: Using Korean as treated unit now.
    ## 21:03:04: Number of 'sunny' donors: 36 out of 36
    ## 21:03:04: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:04: with RMSPE 0.793542005862894 and MSPE (loss v) 0.629708915068905 
    ## 21:03:04: is INFEASIBLE when respecting the predictors.
    ## 21:03:04: Starting optimization via DEoptC, random seed 2019.
    ## 21:03:06: Optimization finished (27001 calls to inner optimizer), rmspe: 
    ## 21:03:06: 0.793542005862986, mspe: 0.629708915069051.
    ## Final rmspe: 0.793542, mspe (loss v): 0.6297089
    ## Optimal weights:
    ##  Estonian     Greek  Japanese   Russian 
    ## 0.1822781 0.1853984 0.4453727 0.1869508 
    ## 
    ## 21:03:06: Using Latvian as treated unit now.
    ## 21:03:06: Number of 'sunny' donors: 36 out of 36
    ## 21:03:06: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:06: with RMSPE 0.22106815834769 and MSPE (loss v) 0.0488711306352394 
    ## 21:03:06: is INFEASIBLE when respecting the predictors.
    ## 21:03:06: Starting optimization via DEoptC, random seed 2019.
    ## 21:03:09: Optimization finished (31201 calls to inner optimizer), rmspe: 
    ## 21:03:09: 0.221068158348011, mspe: 0.0488711306353811.
    ## Final rmspe: 0.2210682, mspe (loss v): 0.04887113
    ## Optimal weights:
    ##    Estonian   Hungarian     Chinese      Jewish    Karelian        Komi 
    ## 0.087115215 0.041224700 0.145196976 0.026834288 0.029933854 0.308450158 
    ##  Lithuanian    Moldovan       Yakut 
    ## 0.130935959 0.008454488 0.221854362 
    ## 
    ## 21:03:09: Using Lithuanian as treated unit now.
    ## 21:03:09: Number of 'sunny' donors: 36 out of 36
    ## 21:03:09: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:09: with RMSPE 0.293593461591797 and MSPE (loss v) 
    ## 21:03:09: 0.0861971206894538 is INFEASIBLE when respecting the predictors.
    ## 21:03:09: Starting optimization via DEoptC, random seed 2019.
    ## 21:03:11: Optimization finished (27721 calls to inner optimizer), rmspe: 
    ## 21:03:11: 0.293593461592814, mspe: 0.086197120690051.
    ## Final rmspe: 0.2935935, mspe (loss v): 0.08619712
    ## Optimal weights:
    ##      Greek    Chuvash     Kalmyk    Latvian      Yakut 
    ## 0.07546940 0.03400671 0.57265676 0.04582288 0.27204426 
    ## 
    ## 21:03:11: Using Mari as treated unit now.
    ## 21:03:11: Number of 'sunny' donors: 36 out of 36
    ## 21:03:11: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:11: with RMSPE 0.476645594153652 and MSPE (loss v) 0.227191022426088 
    ## 21:03:11: is INFEASIBLE when respecting the predictors.
    ## 21:03:12: Starting optimization via DEoptC, random seed 2019.
    ## 21:03:14: Optimization finished (29341 calls to inner optimizer), rmspe: 
    ## 21:03:14: 0.4766455941537, mspe: 0.227191022426133.
    ## Final rmspe: 0.4766456, mspe (loss v): 0.227191
    ## Optimal weights:
    ## Belorussian      Kazakh      Korean     Mordvin     Russian 
    ##  0.17864847  0.59019819  0.11007984  0.05536413  0.06570937 
    ## 
    ## 21:03:14: Using Moldovan as treated unit now.
    ## 21:03:14: Number of 'sunny' donors: 36 out of 36
    ## 21:03:14: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:14: with RMSPE 0.734212380220654 and MSPE (loss v) 0.539067819269279 
    ## 21:03:14: is INFEASIBLE when respecting the predictors.
    ## 21:03:14: Starting optimization via DEoptC, random seed 2019.
    ## 21:03:17: Optimization finished (30241 calls to inner optimizer), rmspe: 
    ## 21:03:17: 0.734212380220655, mspe: 0.53906781926928.
    ## Final rmspe: 0.7342124, mspe (loss v): 0.5390678
    ## Optimal weights:
    ##   Bulgarian    Georgian      Kalmyk        Komi 
    ## 0.457866431 0.192596243 0.339801498 0.009735828 
    ## 
    ## 21:03:17: Using Mordvin as treated unit now.
    ## 21:03:17: Number of 'sunny' donors: 36 out of 36
    ## 21:03:17: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:17: with RMSPE 0.964938248652895 and MSPE (loss v) 0.931105823713316 
    ## 21:03:17: is INFEASIBLE when respecting the predictors.
    ## 21:03:17: Starting optimization via DEoptC, random seed 2019.
    ## 21:03:20: Optimization finished (33601 calls to inner optimizer), rmspe: 
    ## 21:03:20: 0.964938248652898, mspe: 0.931105823713322.
    ## Final rmspe: 0.9649382, mspe (loss v): 0.9311058
    ## Optimal weights:
    ##   Bashkir   Chuvash    Khakas      Mari   Russian 
    ## 0.3938561 0.2085790 0.2010400 0.1235466 0.0729783 
    ## 
    ## 21:03:20: Using Ossetian as treated unit now.
    ## 21:03:20: Number of 'sunny' donors: 36 out of 36
    ## 21:03:20: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:20: with RMSPE 0.522855885118535 and MSPE (loss v) 0.273378276603086 
    ## 21:03:20: is INFEASIBLE when respecting the predictors.
    ## 21:03:20: Starting optimization via DEoptC, random seed 2019.
    ## 21:03:22: checking v: v contains NAs!
    ## 21:03:22: Optimization finished (26521 calls to inner optimizer), rmspe: 
    ## 21:03:22: 0.522855885118543, mspe: 0.273378276603095.
    ## Final rmspe: 0.5228559, mspe (loss v): 0.2733783
    ## Optimal weights:
    ##     Balkar     Buryat  Hungarian    Chinese  Kabardian     Kalmyk 
    ## 0.17106384 0.14486488 0.23207749 0.04481944 0.12220333 0.28497102 
    ## 
    ## 21:03:22: Using Polish as treated unit now.
    ## 21:03:22: Number of 'sunny' donors: 36 out of 36
    ## 21:03:22: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:22: with RMSPE 0.121584788113432 and MSPE (loss v) 
    ## 21:03:22: 0.0147828607005881 is INFEASIBLE when respecting the predictors.
    ## 21:03:22: Starting optimization via DEoptC, random seed 2019.
    ## 21:03:25: Optimization finished (30661 calls to inner optimizer), rmspe: 
    ## 21:03:25: 0.121584788113919, mspe: 0.0147828607007066.
    ## Final rmspe: 0.1215848, mspe (loss v): 0.01478286
    ## Optimal weights:
    ##  Hungarian    Chechen     Kalmyk       Komi Lithuanian   Moldovan 
    ## 0.10893623 0.07677238 0.07517576 0.09022262 0.23730967 0.05824310 
    ##    Mordvin    Russian 
    ## 0.07334406 0.27999618 
    ## 
    ## 21:03:25: Using Russian as treated unit now.
    ## 21:03:25: Number of 'sunny' donors: 36 out of 36
    ## 21:03:25: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:25: is FEASIBLE even when respecting the predictors.
    ## Final rmspe: 3.224247, mspe (loss v): 10.39577
    ## Optimal weights:
    ## Ukrainian 
    ##         1 
    ## 
    ## 21:03:25: Using Tatar as treated unit now.
    ## 21:03:25: Number of 'sunny' donors: 36 out of 36
    ## 21:03:25: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:25: is FEASIBLE even when respecting the predictors.
    ## Final rmspe: 0.3177111, mspe (loss v): 0.1009404
    ## Optimal weights:
    ##    Bashkir    Russian  Ukrainian 
    ## 0.24806804 0.08785235 0.66407961 
    ## 
    ## 21:03:25: Using Udmurt as treated unit now.
    ## 21:03:26: Number of 'sunny' donors: 36 out of 36
    ## 21:03:26: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:26: with RMSPE 0.512189300759458 and MSPE (loss v) 0.262337879812463 
    ## 21:03:26: is INFEASIBLE when respecting the predictors.
    ## 21:03:26: Starting optimization via DEoptC, random seed 2019.
    ## 21:03:28: Optimization finished (27361 calls to inner optimizer), rmspe: 
    ## 21:03:28: 0.512189300759461, mspe: 0.262337879812465.
    ## Final rmspe: 0.5121893, mspe (loss v): 0.2623379
    ## Optimal weights:
    ##      Altai    Bashkir    Chinese   Karelian   Moldovan 
    ## 0.04694495 0.77523317 0.08450321 0.03891487 0.05440380 
    ## 
    ## 21:03:28: Using Ukrainian as treated unit now.
    ## 21:03:28: Number of 'sunny' donors: 36 out of 36
    ## 21:03:28: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:28: with RMSPE 0.257906621920482 and MSPE (loss v) 
    ## 21:03:28: 0.0665158256304347 is INFEASIBLE when respecting the predictors.
    ## 21:03:28: Starting optimization via DEoptC, random seed 2019.
    ## 21:03:30: Optimization finished (27541 calls to inner optimizer), rmspe: 
    ## 21:03:30: 0.257906621920976, mspe: 0.0665158256306892.
    ## Final rmspe: 0.2579066, mspe (loss v): 0.06651583
    ## Optimal weights:
    ##    Chuvash    Russian      Tatar      Yakut 
    ## 0.20499967 0.14217523 0.57489629 0.07792881 
    ## 
    ## 21:03:30: Using Uzbek as treated unit now.
    ## 21:03:30: Number of 'sunny' donors: 36 out of 36
    ## 21:03:30: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:30: with RMSPE 0.588146046897209 and MSPE (loss v) 0.345915772480814 
    ## 21:03:30: is INFEASIBLE when respecting the predictors.
    ## 21:03:30: Starting optimization via DEoptC, random seed 2019.
    ## 21:03:33: Optimization finished (36661 calls to inner optimizer), rmspe: 
    ## 21:03:33: 0.588146046897209, mspe: 0.345915772480814.
    ## Final rmspe: 0.588146, mspe (loss v): 0.3459158
    ## Optimal weights:
    ##     Balkar    Bashkir     Buryat      Greek  Kabardian 
    ## 0.08556229 0.19605616 0.29774202 0.15141216 0.26922737 
    ## 
    ## 21:03:33: Using Yakut as treated unit now.
    ## 21:03:33: Number of 'sunny' donors: 36 out of 36
    ## 21:03:33: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:03:33: with RMSPE 0.896639226429562 and MSPE (loss v) 0.803961902372203 
    ## 21:03:33: is INFEASIBLE when respecting the predictors.
    ## 21:03:33: Starting optimization via DEoptC, random seed 2019.
    ## 21:03:35: Optimization finished (27481 calls to inner optimizer), rmspe: 
    ## 21:03:35: 0.896639226429671, mspe: 0.8039619023724.
    ## Final rmspe: 0.8966392, mspe (loss v): 0.8039619
    ## Optimal weights:
    ##  Hungarian  Kabardian    Latvian    Russian  Ukrainian 
    ## 0.09440588 0.02714902 0.63135465 0.01274693 0.23434352

``` r
placebo_highlight_all(sc_placebo_non_border, "log_n_non_border")
```

    ## Warning: You set use_group_by = TRUE, but grouped calculation failed.
    ## Falling back to ungrouped filter operation...

    ## label_key: ethnicity

``` r
ggsave(here::here("plots/final/placebo_highlight_all_non_border_provinces.pdf"))
```

    ## Saving 7 x 5 in image

``` r
ggsave(here::here("plots/for_presentation/placebo_highlight_all_non_border_provinces.pdf"), scale = 0.7)
```

    ## Saving 4.9 x 3.5 in image

``` r
placebo_highlight_mspe(sc_placebo_non_border, "log_n_non_border", exclusion_ratio = 20)
```

    ## Warning: You set use_group_by = TRUE, but grouped calculation failed.
    ## Falling back to ungrouped filter operation...

    ## label_key: ethnicity

``` r
placebo_mspe_barplot(sc_placebo_non_border, "log_n_non_border", range_post = c("1933", "1960"))
```

``` r
ggsave(here::here("plots/final/mspe_ratios_non_border_provinces.pdf"))
```

    ## Saving 7 x 5 in image

``` r
placebo_mspe_barplot(sc_placebo_non_border, "log_n_non_border", range_post = c("1933", "1939"))
```

``` r
ggsave(here::here("plots/final/mspe_ratios_non_border_provinces_until_1939.pdf"))
```

    ## Saving 7 x 5 in image
