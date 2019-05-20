Effect size calculations
================
Martin Kosík
April 23, 2019

``` r
knitr::opts_chunk$set(echo = TRUE, fig.show = 'hide')
library(tidyverse)
library(here)
library(readxl)
library(broom)
library(MSCMT)
library(timetk)
```

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

fmla_pred_full_imp_date_no_trends_geopol <- as.formula(paste("log_n_pred_full_imp_date ~ ", 
                         paste0("german:","year_", years, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity+ ",  paste0(geopol_vars, collapse = " + ")))


no_log_pred_full_imp_date_no_trends_geopol <- as.formula(paste("n_pred_full_imp_date ~ ", 
                         paste0("german:","year_", years, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity+ ",  paste0(geopol_vars, collapse = " + ")))
```

``` r
years_from_1933 <- 1933:1960
years_from_1927 <- 1927:1960

fmla_pred_full_imp_date_no_trends_geopol_from_1933 <- as.formula(paste("log_n_pred_full_imp_date ~ ", 
                         paste0("german:","year_", years_from_1933, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity+ ",  paste0(geopol_vars, collapse = " + ")))


fmla_pred_full_imp_date_no_trends_geopol_from_1927 <- as.formula(paste("log_n_pred_full_imp_date ~ ", 
                         paste0("german:","year_", years_from_1927, collapse = " + "),  "+ as.factor(YEAR) +
                         ethnicity+ ",  paste0(geopol_vars, collapse = " + ")))
```

``` r
arrests_in_year <- function(base_year_from = 1921, base_year_to = 1921){
  totals %>% 
  filter(YEAR >= base_year_from, YEAR <= base_year_from) %>% 
  summarise(total_repression = mean(total_repression)) %>% 
  pull(total_repression)
}

totals <- min_by_year %>% 
  filter(ethnicity == "German") %>% 
  mutate(total_repression = label_imp_date + pred_adj_full_scaled_imp_date) %>% 
  dplyr::select(YEAR, total_repression)


new_data<- min_by_year %>% 
  filter(ethnicity == "German") %>% 
  mutate_at(vars(starts_with("year_")), funs(ifelse(. == 1, 0, 0))) %>% 
  bind_rows(filter(min_by_year, ethnicity == "German"), .id = "type_col")

counter_facts <-  augment(lm(fmla_pred_full_imp_date_no_trends_geopol, data = min_by_year), newdata =  new_data) %>% 
  dplyr::select(type_col, YEAR, .fitted) %>% 
  mutate(type_col = recode_factor(type_col, "1" = "counterfactual", "2" = "actual")) %>% 
  tidyr::spread(key = "type_col", value = ".fitted") %>% 
  mutate(count_synth = exp(counterfactual) - 1,
         count_treat = exp(actual) - 1,
         effect_count = round(count_treat- count_synth),
         effect_log = actual - counterfactual,
         effect_pct_change_naive = 100 * (exp(effect_log) - 1),
         effect_count_based_on_pct = arrests_in_year() * (effect_pct_change_naive/100),
         effect_count_based_on_pct_rounded = round(effect_count_based_on_pct))

counter_facts %>% 
  filter(YEAR > 1932) %>% 
  summarize_at(c("count_synth", "count_treat", "effect_count"), sum) 
```

    ## # A tibble: 1 x 3
    ##   count_synth count_treat effect_count
    ##         <dbl>       <dbl>        <dbl>
    ## 1      25038.     153060.       128022

``` r
counter_facts %>% 
  filter(YEAR > 1940, YEAR < 1946) %>% 
  summarize_at(c("count_synth", "count_treat", "effect_count"), sum) 
```

    ## # A tibble: 1 x 3
    ##   count_synth count_treat effect_count
    ##         <dbl>       <dbl>        <dbl>
    ## 1       4836.      94005.        89169

``` r
counter_facts %>% 
  filter(YEAR > 1932, YEAR < 1940) %>% 
  summarize_at(c("count_synth", "count_treat", "effect_count"), sum) 
```

    ## # A tibble: 1 x 3
    ##   count_synth count_treat effect_count
    ##         <dbl>       <dbl>        <dbl>
    ## 1      18310.      31041.        12731

``` r
counter_facts %>% 
  filter(YEAR > 1921, YEAR < 1933) %>% 
  summarize_at(c("count_synth", "count_treat", "effect_count"), sum) 
```

    ## # A tibble: 1 x 3
    ##   count_synth count_treat effect_count
    ##         <dbl>       <dbl>        <dbl>
    ## 1       8629.      18517.         9888

``` r
augment(lm(fmla_pred_full_imp_date_no_trends_geopol_from_1927, data = min_by_year), newdata =  new_data) %>% 
  dplyr::select(type_col, YEAR, .fitted) %>% 
  mutate(type_col = recode_factor(type_col, "1" = "counterfactual", "2" = "actual")) %>% 
  tidyr::spread(key = "type_col", value = ".fitted") %>% 
  mutate(count_synth = exp(counterfactual) - 1,
         count_treat = exp(actual) - 1,
         effect_count = round(count_treat- count_synth),
         effect_log = actual - counterfactual,
         effect_pct_change_naive = 100 * (exp(effect_log) - 1),
         effect_count_based_on_pct = arrests_in_year() * (effect_pct_change_naive/100),
         effect_count_based_on_pct_rounded = round(effect_count_based_on_pct))
```

    ## # A tibble: 40 x 10
    ##     YEAR counterfactual actual count_synth count_treat effect_count
    ##    <int>          <dbl>  <dbl>       <dbl>       <dbl>        <dbl>
    ##  1  1921           5.03   5.03       151.        151.             0
    ##  2  1922           3.67   3.67        38.3        38.3            0
    ##  3  1923           3.38   3.38        28.3        28.3            0
    ##  4  1924           3.32   3.32        26.6        26.6            0
    ##  5  1925           3.63   3.63        36.6        36.6            0
    ##  6  1926           3.93   3.93        50.2        50.2            0
    ##  7  1927           4.83   4.98       124.        145.            21
    ##  8  1928           5.34   5.42       208.        226.            18
    ##  9  1929           6.78   7.44       875.       1699.           824
    ## 10  1930           8.19   8.92      3603.       7497.          3894
    ## # ... with 30 more rows, and 4 more variables: effect_log <dbl>,
    ## #   effect_pct_change_naive <dbl>, effect_count_based_on_pct <dbl>,
    ## #   effect_count_based_on_pct_rounded <dbl>

``` r
totals %>% 
  filter(YEAR > 1932) %>% 
  summarise(total = sum(total_repression))
```

    ## # A tibble: 1 x 1
    ##    total
    ##    <int>
    ## 1 153060

``` r
arrests_in_year()
```

    ## [1] 135

``` r
effects_data <- lm(fmla_pred_full_imp_date_no_trends_geopol, data = min_by_year) %>% 
  broom::tidy() %>% 
  filter(str_detect(term, "german:year")) %>% 
  mutate(nice_labs = as.numeric(str_replace(term, "german:year_", ""))) %>% 
  left_join(totals, by = c("nice_labs"  = "YEAR")) %>% 
  mutate(effect_pct_change = 100 * (exp(estimate) - 1),
         effect_count = total_repression * (estimate/100),
         effect_count_rounded = round(effect_count)) 




pct_effects <- function(formula = fmla_pred_full_imp_date_no_trends_geopol, data = min_by_year,
                        base_year_from = 1921, base_year_to = 1921){
  lm_robust(formula, data = data, 
                          clusters = data$ethnicity, se_type = "CR2") %>% 
  estimatr::tidy() %>% 
  filter(str_detect(term, "german:year")) %>% 
  mutate(nice_labs = as.numeric(str_replace(term, "german:year_", ""))) %>% 
  left_join(totals, by = c("nice_labs"  = "YEAR")) %>% 
  mutate(effect_pct_change_naive = 100 * (exp(estimate) - 1),
         effect_pct_change = 100 * (exp(estimate - (1/2) * std.error^2) - 1),
         effect_pct_change_var = 100^2 * exp(2 * estimate) * (exp(std.error^2) - exp(-2 * std.error^2)),
         effect_pct_change_se = sqrt(effect_pct_change_var),
         effect_pct_change_ci_lower = effect_pct_change - qt(0.975, df = round(df)) * effect_pct_change_se,
         effect_pct_change_ci_upper = effect_pct_change + qt(0.975, df = round(df)) * effect_pct_change_se,
         effect_count = arrests_in_year(base_year_from = base_year_from, base_year_to = base_year_to) *
                       (effect_pct_change/100),
         effect_count_rounded = round(effect_count),
         effect_count_lower = arrests_in_year(base_year_from = base_year_from, base_year_to = base_year_tp) *
                          (effect_pct_change_ci_lower/100),
         effect_count_rounded_lower = round(effect_count_lower),
         effect_count_upper = arrests_in_year(base_year_from = base_year_from, base_year_to = base_year_to) *
                          (effect_pct_change_ci_upper/100),
         effect_count_rounded_upper = round(effect_count_upper)) %>% 
    as_tibble()
}


pct_effects_time_window <- function(formula = fmla_window_pre_treat_pred_full_imp_date_no_trends_geopol, 
                                    data = min_by_year, base_year = 1921){
  lm_robust(formula, data = data, 
                          clusters = data$ethnicity, se_type = "CR2") %>% 
  estimatr::tidy() %>% 
  filter(str_detect(term, "german:")) %>% 
 # mutate(nice_labs = as.numeric(str_replace(term, "german:", ""))) %>% 
  mutate(effect_pct_change_naive = 100 * (exp(estimate) - 1),
         effect_pct_change = 100 * (exp(estimate - (1/2) * std.error^2) - 1),
         effect_pct_change_var = 100^2 * exp(2 * estimate) * (exp(std.error^2) - exp(-2 * std.error^2)),
         effect_pct_change_se = sqrt(effect_pct_change_var),
         effect_pct_change_ci_lower = effect_pct_change - qt(0.975, df = round(df)) * effect_pct_change_se,
         effect_pct_change_ci_upper = effect_pct_change + qt(0.975, df = round(df)) * effect_pct_change_se,
         effect_count = arrests_in_year(base_year = base_year) * (effect_pct_change/100),
         effect_count_rounded = round(effect_count),
         effect_count_lower = arrests_in_year(base_year = base_year) * (effect_pct_change_ci_lower/100),
         effect_count_rounded_lower = round(effect_count_lower),
         effect_count_upper = arrests_in_year(base_year = base_year) * (effect_pct_change_ci_upper/100),
         effect_count_rounded_upper = round(effect_count_upper)) 
}



total_effects <- function(data, year_from = 1932, year_til = 1961){
  data %>% 
    filter(nice_labs > year_from, nice_labs < year_til) %>% 
    summarize_at(c("effect_count", "effect_count_lower", "effect_count_rounded_upper"), sum) %>% 
    as_tibble()
}
```

Synthetic control method
------------------------

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
         n_pred_full_imp_date = label_imp_date + pred_adj_full_scaled_imp_date,
         log_n_pred_full_imp_date_rehab = log(1 + label_rehab + pred_adj_full_scaled_rehab)) %>% 
  left_join(ethnicity_controls, by = "ethnicity") %>% 
  mutate(german = ifelse(ethnicity == "German", 1, 0),
         post_german = german * ifelse(YEAR >= 1933, 1, 0)) 
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

``` r
data_prep_mscmt <- listFromLong(as.data.frame(min_by_year), unit.variable = "ethnicity_id", 
                                time.variable="YEAR", unit.names.variable="ethnicity")
```

Definition of the variables

``` r
dep_var <- "log_n_pred_full_imp_date"

treatment.identifier <- "German"
controls.identifier  <- setdiff(colnames(data_prep_mscmt[[1]]),
                                 treatment.identifier)
times.dep  <- cbind("log_n_pred_full_imp_date" = c("1921","1932"))


times.pred <- cbind("log_n_pred_full_imp_date" = c("1921","1932"),
                    "pop_total"                = c("1921","1932"),
                    "clad_sim"                 = c("1921","1932"),
                    "urb_rate"                 = c("1921","1932"))

agg.fns <- rep("id", ncol(times.pred))
```

``` r
sc_placebo_pred_full_imp_date <- mscmt(data_prep_mscmt, treatment.identifier, controls.identifier, times.dep,
                                       times.pred, agg.fns, seed=2019, placebo = TRUE, single.v=TRUE)
```

    ## 22:11:29: Starting placebo study, excluding original treated unit.
    ## 22:11:29: Using German as treated unit now.
    ## 22:11:29: Number of 'sunny' donors: 37 out of 37
    ## 22:11:29: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:11:29: with RMSPE 0.147834274985798 and MSPE (loss v) 
    ## 22:11:29: 0.0218549728605767 is INFEASIBLE when respecting the predictors.
    ## 22:11:29: Starting optimization via DEoptC, random seed 2019.
    ## 22:11:33: Optimization finished (31261 calls to inner optimizer), rmspe: 
    ## 22:11:33: 0.147834274989802, mspe: 0.0218549728617605.
    ## Final rmspe: 0.1478343, mspe (loss v): 0.02185497
    ## Optimal weights:
    ##   Georgian      Greek    Chuvash     Korean Lithuanian   Ossetian 
    ## 0.05188119 0.31903984 0.06847046 0.01897588 0.10282469 0.08006683 
    ##    Russian      Tatar 
    ## 0.30709238 0.05164872 
    ## 
    ## 22:11:33: Using Altai as treated unit now.
    ## 22:11:33: Number of 'sunny' donors: 36 out of 36
    ## 22:11:33: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:11:33: with RMSPE 0.386843865853203 and MSPE (loss v) 0.149648176548251 
    ## 22:11:33: is INFEASIBLE when respecting the predictors.
    ## 22:11:33: Starting optimization via DEoptC, random seed 2019.
    ## 22:11:35: Optimization finished (26041 calls to inner optimizer), rmspe: 
    ## 22:11:35: 0.386843865853212, mspe: 0.149648176548258.
    ## Final rmspe: 0.3868439, mspe (loss v): 0.1496482
    ## Optimal weights:
    ##     Balkar  Hungarian    Chinese  Kabardian     Korean     Udmurt 
    ## 0.39496722 0.12898326 0.07015499 0.07245170 0.18617603 0.14726681 
    ## 
    ## 22:11:35: Using Armenian as treated unit now.
    ## 22:11:35: Number of 'sunny' donors: 36 out of 36
    ## 22:11:35: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:11:35: with RMSPE 0.297234046607783 and MSPE (loss v) 0.088348078462838 
    ## 22:11:35: is INFEASIBLE when respecting the predictors.
    ## 22:11:35: Starting optimization via DEoptC, random seed 2019.
    ## 22:11:38: Optimization finished (35101 calls to inner optimizer), rmspe: 
    ## 22:11:38: 0.29723404660782, mspe: 0.0883480784628597.
    ## Final rmspe: 0.297234, mspe (loss v): 0.08834808
    ## Optimal weights:
    ##  Hungarian    Chinese     Kalmyk       Komi    Latvian   Ossetian 
    ## 0.11967850 0.11613241 0.08952587 0.14875713 0.04098062 0.33843550 
    ##      Yakut 
    ## 0.14648997 
    ## 
    ## 22:11:39: Using Balkar as treated unit now.
    ## 22:11:39: Number of 'sunny' donors: 36 out of 36
    ## 22:11:39: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:11:39: with RMSPE 0.422238773128067 and MSPE (loss v) 0.178285581532695 
    ## 22:11:39: is INFEASIBLE when respecting the predictors.
    ## 22:11:39: Starting optimization via DEoptC, random seed 2019.
    ## 22:11:40: Optimization finished (31201 calls to inner optimizer), rmspe: 
    ## 22:11:40: 0.422238773128071, mspe: 0.178285581532698.
    ## Final rmspe: 0.4222388, mspe (loss v): 0.1782856
    ## Optimal weights:
    ##     Altai Bulgarian    Kalmyk     Uzbek 
    ## 0.2419899 0.1524354 0.1750580 0.4305167 
    ## 
    ## 22:11:40: Using Bashkir as treated unit now.
    ## 22:11:40: Number of 'sunny' donors: 36 out of 36
    ## 22:11:40: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:11:40: with RMSPE 0.373659688496821 and MSPE (loss v) 0.139621562807542 
    ## 22:11:40: is INFEASIBLE when respecting the predictors.
    ## 22:11:41: Starting optimization via DEoptC, random seed 2019.
    ## 22:11:43: Optimization finished (36061 calls to inner optimizer), rmspe: 
    ## 22:11:43: 0.373659688496825, mspe: 0.139621562807544.
    ## Final rmspe: 0.3736597, mspe (loss v): 0.1396216
    ## Optimal weights:
    ##      Greek    Chechen    Mordvin      Tatar     Udmurt  Ukrainian 
    ## 0.06733137 0.03540904 0.29218098 0.11058834 0.47457087 0.01991940 
    ## 
    ## 22:11:43: Using Belorussian as treated unit now.
    ## 22:11:44: Number of 'sunny' donors: 36 out of 36
    ## 22:11:44: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:11:44: with RMSPE 0.410589187157624 and MSPE (loss v) 0.168583480610759 
    ## 22:11:44: is INFEASIBLE when respecting the predictors.
    ## 22:11:44: Starting optimization via DEoptC, random seed 2019.
    ## 22:11:47: Optimization finished (54001 calls to inner optimizer), rmspe: 
    ## 22:11:47: 0.410589187158154, mspe: 0.168583480611194.
    ## Final rmspe: 0.4105892, mspe (loss v): 0.1685835
    ## Optimal weights:
    ##   Estonian    Chinese     Jewish    Mordvin     Polish    Russian 
    ## 0.03209245 0.27188012 0.03531801 0.11415056 0.18975023 0.35680862 
    ## 
    ## 22:11:47: Using Bulgarian as treated unit now.
    ## 22:11:47: Number of 'sunny' donors: 36 out of 36
    ## 22:11:47: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:11:47: with RMSPE 0.431870856593713 and MSPE (loss v) 0.186512436774987 
    ## 22:11:47: is INFEASIBLE when respecting the predictors.
    ## 22:11:47: Starting optimization via DEoptC, random seed 2019.
    ## 22:11:50: Optimization finished (33721 calls to inner optimizer), rmspe: 
    ## 22:11:50: 0.431870856593738, mspe: 0.186512436775009.
    ## Final rmspe: 0.4318709, mspe (loss v): 0.1865124
    ## Optimal weights:
    ##     Balkar    Finnish    Chuvash     Kalmyk   Moldovan      Uzbek 
    ## 0.34251135 0.15484241 0.03421413 0.03240790 0.29081579 0.14520842 
    ## 
    ## 22:11:50: Using Buryat as treated unit now.
    ## 22:11:50: Number of 'sunny' donors: 36 out of 36
    ## 22:11:50: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:11:50: with RMSPE 0.20533236410822 and MSPE (loss v) 0.0421613797502707 
    ## 22:11:50: is INFEASIBLE when respecting the predictors.
    ## 22:11:51: Starting optimization via DEoptC, random seed 2019.
    ## 22:11:53: checking v: v contains NAs!
    ## 22:11:53: Optimization finished (26281 calls to inner optimizer), rmspe: 
    ## 22:11:53: 0.205332364108241, mspe: 0.0421613797502791.
    ## Final rmspe: 0.2053324, mspe (loss v): 0.04216138
    ## Optimal weights:
    ##    Hungarian      Chuvash       Kalmyk     Karelian      Mordvin 
    ## 0.1196810160 0.3803556496 0.1935073507 0.0001422161 0.0889086818 
    ##        Uzbek 
    ## 0.2174050858 
    ## 
    ## 22:11:53: Using Estonian as treated unit now.
    ## 22:11:53: Number of 'sunny' donors: 36 out of 36
    ## 22:11:53: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:11:53: with RMSPE 0.241632110537875 and MSPE (loss v) 
    ## 22:11:53: 0.0583860768429877 is INFEASIBLE when respecting the predictors.
    ## 22:11:53: Starting optimization via DEoptC, random seed 2019.
    ## 22:11:55: Optimization finished (26821 calls to inner optimizer), rmspe: 
    ## 22:11:55: 0.241632110538961, mspe: 0.0583860768435125.
    ## Final rmspe: 0.2416321, mspe (loss v): 0.05838608
    ## Optimal weights:
    ##       Altai Belorussian    Georgian   Hungarian     Chinese      Jewish 
    ## 0.020749740 0.072912015 0.020740322 0.359858429 0.002228735 0.089808115 
    ##    Karelian      Korean     Russian 
    ## 0.058933158 0.254244221 0.120525264 
    ## 
    ## 22:11:55: Using Finnish as treated unit now.
    ## 22:11:55: Number of 'sunny' donors: 36 out of 36
    ## 22:11:55: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:11:55: with RMSPE 0.165544878592461 and MSPE (loss v) 
    ## 22:11:55: 0.0274051068281925 is INFEASIBLE when respecting the predictors.
    ## 22:11:56: Starting optimization via DEoptC, random seed 2019.
    ## 22:11:58: Optimization finished (25921 calls to inner optimizer), rmspe: 
    ## 22:11:58: 0.16554487859275, mspe: 0.0274051068282883.
    ## Final rmspe: 0.1655449, mspe (loss v): 0.02740511
    ## Optimal weights:
    ##   Bulgarian     Chechen    Japanese      Khakas        Komi     Latvian 
    ## 0.381192411 0.060864885 0.064862071 0.257840701 0.009947096 0.203780589 
    ##       Tatar 
    ## 0.021512247 
    ## 
    ## 22:11:58: Using Georgian as treated unit now.
    ## 22:11:58: Number of 'sunny' donors: 36 out of 36
    ## 22:11:58: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:11:58: with RMSPE 0.308621632695222 and MSPE (loss v) 
    ## 22:11:58: 0.0952473121674644 is INFEASIBLE when respecting the predictors.
    ## 22:11:58: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:01: Optimization finished (28621 calls to inner optimizer), rmspe: 
    ## 22:12:01: 0.308621632695238, mspe: 0.0952473121674745.
    ## Final rmspe: 0.3086216, mspe (loss v): 0.09524731
    ## Optimal weights:
    ##     Balkar    Chinese     Korean Lithuanian   Moldovan 
    ## 0.16763472 0.34946050 0.05627929 0.01320429 0.41342120 
    ## 
    ## 22:12:01: Using Greek as treated unit now.
    ## 22:12:01: Number of 'sunny' donors: 36 out of 36
    ## 22:12:01: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:01: with RMSPE 0.298263329338972 and MSPE (loss v) 
    ## 22:12:01: 0.0889610136283681 is INFEASIBLE when respecting the predictors.
    ## 22:12:01: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:04: Optimization finished (31141 calls to inner optimizer), rmspe: 
    ## 22:12:04: 0.298263329338982, mspe: 0.0889610136283743.
    ## Final rmspe: 0.2982633, mspe (loss v): 0.08896101
    ## Optimal weights:
    ##     Khakas    Russian      Tatar  Ukrainian      Yakut 
    ## 0.71609990 0.07157196 0.13211037 0.06828728 0.01193048 
    ## 
    ## 22:12:04: Using Hungarian as treated unit now.
    ## 22:12:04: Number of 'sunny' donors: 36 out of 36
    ## 22:12:04: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:04: with RMSPE 0.880005719757757 and MSPE (loss v) 0.774410066806368 
    ## 22:12:04: is INFEASIBLE when respecting the predictors.
    ## 22:12:04: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:05: Optimization finished (31561 calls to inner optimizer), rmspe: 
    ## 22:12:05: 0.880005719757771, mspe: 0.774410066806392.
    ## Final rmspe: 0.8800057, mspe (loss v): 0.7744101
    ## Optimal weights:
    ##    Chinese     Jewish     Kalmyk 
    ## 0.07860859 0.08548922 0.83590219 
    ## 
    ## 22:12:06: Using Chechen as treated unit now.
    ## 22:12:06: Number of 'sunny' donors: 36 out of 36
    ## 22:12:06: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:06: with RMSPE 0.384180448112309 and MSPE (loss v) 0.147594616711775 
    ## 22:12:06: is INFEASIBLE when respecting the predictors.
    ## 22:12:06: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:08: checking v: v contains NAs!
    ## 22:12:08: Optimization finished (32221 calls to inner optimizer), rmspe: 
    ## 22:12:08: 0.38418044811231, mspe: 0.147594616711775.
    ## Final rmspe: 0.3841804, mspe (loss v): 0.1475946
    ## Optimal weights:
    ##    Bashkir  Bulgarian     Kalmyk   Ossetian     Udmurt 
    ## 0.46485672 0.02856198 0.22951513 0.19209421 0.08497196 
    ## 
    ## 22:12:08: Using Chinese as treated unit now.
    ## 22:12:08: Number of 'sunny' donors: 36 out of 36
    ## 22:12:08: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:08: with RMSPE 0.80914275254453 and MSPE (loss v) 0.654711993995338 
    ## 22:12:08: is INFEASIBLE when respecting the predictors.
    ## 22:12:09: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:11: Optimization finished (30421 calls to inner optimizer), rmspe: 
    ## 22:12:11: 0.809142752544544, mspe: 0.654711993995361.
    ## Final rmspe: 0.8091428, mspe (loss v): 0.654712
    ## Optimal weights:
    ##   Armenian   Georgian  Hungarian  Kabardian   Karelian 
    ## 0.15750643 0.31681390 0.08560617 0.29312061 0.14695290 
    ## 
    ## 22:12:11: Using Chuvash as treated unit now.
    ## 22:12:11: Number of 'sunny' donors: 36 out of 36
    ## 22:12:11: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:11: with RMSPE 0.879896185542204 and MSPE (loss v) 0.77421729733172 
    ## 22:12:11: is INFEASIBLE when respecting the predictors.
    ## 22:12:11: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:13: Optimization finished (26341 calls to inner optimizer), rmspe: 
    ## 22:12:13: 0.879896185542209, mspe: 0.77421729733173.
    ## Final rmspe: 0.8798962, mspe (loss v): 0.7742173
    ## Optimal weights:
    ##     Buryat       Komi    Russian      Tatar     Udmurt 
    ## 0.47566387 0.01035899 0.08750964 0.35166100 0.07480651 
    ## 
    ## 22:12:13: Using Japanese as treated unit now.
    ## 22:12:13: Number of 'sunny' donors: 36 out of 36
    ## 22:12:13: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:13: with RMSPE 0.172288107773526 and MSPE (loss v) 
    ## 22:12:13: 0.0296831920801822 is INFEASIBLE when respecting the predictors.
    ## 22:12:13: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:15: Optimization finished (30001 calls to inner optimizer), rmspe: 
    ## 22:12:15: 0.172288107773628, mspe: 0.0296831920802171.
    ## Final rmspe: 0.1722881, mspe (loss v): 0.02968319
    ## Optimal weights:
    ##       Altai     Finnish      Kalmyk    Karelian      Khakas     Latvian 
    ## 0.152893087 0.279593386 0.075780289 0.003744051 0.090492543 0.115992843 
    ##     Mordvin       Uzbek 
    ## 0.081603042 0.199900759 
    ## 
    ## 22:12:16: Using Jewish as treated unit now.
    ## 22:12:16: Number of 'sunny' donors: 36 out of 36
    ## 22:12:16: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:16: with RMSPE 0.851483322024242 and MSPE (loss v) 0.725023847685439 
    ## 22:12:16: is INFEASIBLE when respecting the predictors.
    ## 22:12:16: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:17: Optimization finished (27061 calls to inner optimizer), rmspe: 
    ## 22:12:17: 0.851483322024261, mspe: 0.725023847685472.
    ## Final rmspe: 0.8514833, mspe (loss v): 0.7250238
    ## Optimal weights:
    ##  Hungarian   Karelian    Russian 
    ## 0.45754702 0.04976069 0.49269229 
    ## 
    ## 22:12:17: Using Kabardian as treated unit now.
    ## 22:12:17: Number of 'sunny' donors: 36 out of 36
    ## 22:12:17: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:17: with RMSPE 0.508795998019183 and MSPE (loss v) 0.258873367600336 
    ## 22:12:17: is INFEASIBLE when respecting the predictors.
    ## 22:12:17: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:20: checking v: v contains NAs!
    ## 22:12:20: Optimization finished (31561 calls to inner optimizer), rmspe: 
    ## 22:12:20: 0.508795998019183, mspe: 0.258873367600336.
    ## Final rmspe: 0.508796, mspe (loss v): 0.2588734
    ## Optimal weights:
    ##      Altai    Bashkir   Ossetian      Tatar      Uzbek 
    ## 0.33967513 0.03019005 0.36263050 0.12619624 0.14130808 
    ## 
    ## 22:12:20: Using Kalmyk as treated unit now.
    ## 22:12:20: Number of 'sunny' donors: 36 out of 36
    ## 22:12:20: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:20: with RMSPE 0.906837145905573 and MSPE (loss v) 0.822353609194166 
    ## 22:12:20: is INFEASIBLE when respecting the predictors.
    ## 22:12:20: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:21: checking v: v contains NAs!
    ## 22:12:21: Optimization finished (27541 calls to inner optimizer), rmspe: 
    ## 22:12:21: 0.906837145905597, mspe: 0.82235360919421.
    ## Final rmspe: 0.9068371, mspe (loss v): 0.8223536
    ## Optimal weights:
    ## Bulgarian Hungarian  Moldovan 
    ## 0.1158291 0.7474566 0.1367142 
    ## 
    ## 22:12:21: Using Karelian as treated unit now.
    ## 22:12:22: Number of 'sunny' donors: 36 out of 36
    ## 22:12:22: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:22: with RMSPE 1.1489962739756 and MSPE (loss v) 1.32019243760982 is 
    ## 22:12:22: INFEASIBLE when respecting the predictors.
    ## 22:12:22: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:23: Optimization finished (25021 calls to inner optimizer), rmspe: 
    ## 22:12:23: 1.14899627397561, mspe: 1.32019243760984.
    ## Final rmspe: 1.148996, mspe (loss v): 1.320192
    ## Optimal weights:
    ##  Hungarian    Chinese     Jewish       Komi 
    ## 0.23376112 0.04948875 0.05225700 0.66449313 
    ## 
    ## 22:12:23: Using Kazakh as treated unit now.
    ## 22:12:23: Number of 'sunny' donors: 36 out of 36
    ## 22:12:23: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:23: with RMSPE 0.182550505205933 and MSPE (loss v) 
    ## 22:12:23: 0.0333246869509413 is INFEASIBLE when respecting the predictors.
    ## 22:12:23: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:26: Optimization finished (27241 calls to inner optimizer), rmspe: 
    ## 22:12:26: 0.182550505205934, mspe: 0.0333246869509417.
    ## Final rmspe: 0.1825505, mspe (loss v): 0.03332469
    ## Optimal weights:
    ##     Bashkir   Bulgarian     Chuvash   Kabardian      Korean     Mordvin 
    ## 0.035029841 0.004386157 0.083568809 0.071109525 0.192456581 0.044077888 
    ##       Tatar       Uzbek 
    ## 0.472227325 0.097143873 
    ## 
    ## 22:12:26: Using Khakas as treated unit now.
    ## 22:12:26: Number of 'sunny' donors: 36 out of 36
    ## 22:12:26: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:26: with RMSPE 0.271855125972731 and MSPE (loss v) 
    ## 22:12:26: 0.0739052095176495 is INFEASIBLE when respecting the predictors.
    ## 22:12:26: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:28: Optimization finished (29341 calls to inner optimizer), rmspe: 
    ## 22:12:28: 0.271855125972733, mspe: 0.0739052095176506.
    ## Final rmspe: 0.2718551, mspe (loss v): 0.07390521
    ## Optimal weights:
    ##     Balkar    Bashkir    Finnish      Greek     Kalmyk    Mordvin 
    ## 0.31902607 0.07798229 0.07347298 0.39896290 0.01935135 0.11120440 
    ## 
    ## 22:12:29: Using Komi as treated unit now.
    ## 22:12:29: Number of 'sunny' donors: 36 out of 36
    ## 22:12:29: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:29: with RMSPE 0.658636954790221 and MSPE (loss v) 0.433802638215336 
    ## 22:12:29: is INFEASIBLE when respecting the predictors.
    ## 22:12:29: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:31: checking v: v contains NAs!
    ## 22:12:31: Optimization finished (27481 calls to inner optimizer), rmspe: 
    ## 22:12:31: 0.658636954790242, mspe: 0.433802638215364.
    ## Final rmspe: 0.658637, mspe (loss v): 0.4338026
    ## Optimal weights:
    ##    Chuvash     Kalmyk   Karelian  Ukrainian      Yakut 
    ## 0.09051473 0.18527856 0.33650143 0.26328530 0.12441997 
    ## 
    ## 22:12:31: Using Korean as treated unit now.
    ## 22:12:31: Number of 'sunny' donors: 36 out of 36
    ## 22:12:31: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:31: with RMSPE 0.260310204114686 and MSPE (loss v) 
    ## 22:12:31: 0.0677614023662296 is INFEASIBLE when respecting the predictors.
    ## 22:12:31: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:33: Optimization finished (29641 calls to inner optimizer), rmspe: 
    ## 22:12:33: 0.260310204115713, mspe: 0.0677614023667641.
    ## Final rmspe: 0.2603102, mspe (loss v): 0.0677614
    ## Optimal weights:
    ##     Altai    Jewish      Mari   Russian 
    ## 0.4417795 0.1496721 0.2183209 0.1902275 
    ## 
    ## 22:12:33: Using Latvian as treated unit now.
    ## 22:12:33: Number of 'sunny' donors: 36 out of 36
    ## 22:12:33: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:33: with RMSPE 0.379033432874932 and MSPE (loss v) 0.143666343236955 
    ## 22:12:33: is INFEASIBLE when respecting the predictors.
    ## 22:12:33: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:36: Optimization finished (26941 calls to inner optimizer), rmspe: 
    ## 22:12:36: 0.379033432874991, mspe: 0.143666343237.
    ## Final rmspe: 0.3790334, mspe (loss v): 0.1436663
    ## Optimal weights:
    ##    Armenian     Bashkir Belorussian     Finnish      Jewish    Karelian 
    ## 0.322917012 0.081161642 0.036377861 0.030278939 0.360947858 0.054949452 
    ##    Ossetian      Udmurt   Ukrainian 
    ## 0.098700380 0.012317536 0.002349319 
    ## 
    ## 22:12:36: Using Lithuanian as treated unit now.
    ## 22:12:36: Number of 'sunny' donors: 36 out of 36
    ## 22:12:36: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:36: with RMSPE 0.244538664565769 and MSPE (loss v) 
    ## 22:12:36: 0.0597991584676094 is INFEASIBLE when respecting the predictors.
    ## 22:12:36: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:39: Optimization finished (39361 calls to inner optimizer), rmspe: 
    ## 22:12:39: 0.244538664565912, mspe: 0.0597991584676796.
    ## Final rmspe: 0.2445387, mspe (loss v): 0.05979916
    ## Optimal weights:
    ##   Georgian     Jewish     Kalmyk   Karelian   Ossetian      Yakut 
    ## 0.03268453 0.10752792 0.28655958 0.10095670 0.35156628 0.12070500 
    ## 
    ## 22:12:39: Using Mari as treated unit now.
    ## 22:12:39: Number of 'sunny' donors: 36 out of 36
    ## 22:12:39: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:39: with RMSPE 0.100134192932982 and MSPE (loss v) 
    ## 22:12:39: 0.0100268565943397 is INFEASIBLE when respecting the predictors.
    ## 22:12:39: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:42: Optimization finished (31561 calls to inner optimizer), rmspe: 
    ## 22:12:42: 0.100134192936011, mspe: 0.0100268565949462.
    ## Final rmspe: 0.1001342, mspe (loss v): 0.01002686
    ## Optimal weights:
    ##     Chinese    Japanese   Kabardian      Korean     Mordvin     Russian 
    ## 0.003856797 0.040094861 0.023399628 0.540961784 0.083032566 0.134992757 
    ##       Uzbek       Yakut 
    ## 0.125572610 0.048088996 
    ## 
    ## 22:12:42: Using Moldovan as treated unit now.
    ## 22:12:42: Number of 'sunny' donors: 36 out of 36
    ## 22:12:42: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:42: with RMSPE 0.549796788834761 and MSPE (loss v) 0.302276509013015 
    ## 22:12:42: is INFEASIBLE when respecting the predictors.
    ## 22:12:42: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:45: Optimization finished (28441 calls to inner optimizer), rmspe: 
    ## 22:12:45: 0.549796788834762, mspe: 0.302276509013015.
    ## Final rmspe: 0.5497968, mspe (loss v): 0.3022765
    ## Optimal weights:
    ##  Bulgarian   Georgian     Kalmyk    Russian     Udmurt      Yakut 
    ## 0.19180497 0.09431761 0.49543475 0.05139067 0.12024748 0.04680452 
    ## 
    ## 22:12:45: Using Mordvin as treated unit now.
    ## 22:12:45: Number of 'sunny' donors: 36 out of 36
    ## 22:12:45: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:45: with RMSPE 0.621813001602366 and MSPE (loss v) 0.386651408961744 
    ## 22:12:45: is INFEASIBLE when respecting the predictors.
    ## 22:12:45: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:47: Optimization finished (28201 calls to inner optimizer), rmspe: 
    ## 22:12:47: 0.621813001602368, mspe: 0.386651408961747.
    ## Final rmspe: 0.621813, mspe (loss v): 0.3866514
    ## Optimal weights:
    ##    Bashkir    Chuvash     Khakas    Russian 
    ## 0.63655592 0.12383072 0.16565222 0.07396114 
    ## 
    ## 22:12:47: Using Ossetian as treated unit now.
    ## 22:12:47: Number of 'sunny' donors: 36 out of 36
    ## 22:12:47: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:47: with RMSPE 0.283613568696048 and MSPE (loss v) 
    ## 22:12:47: 0.0804366563485079 is INFEASIBLE when respecting the predictors.
    ## 22:12:47: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:50: Optimization finished (29761 calls to inner optimizer), rmspe: 
    ## 22:12:50: 0.283613568696094, mspe: 0.080436656348534.
    ## Final rmspe: 0.2836136, mspe (loss v): 0.08043666
    ## Optimal weights:
    ##    Armenian      Balkar   Bulgarian     Chechen   Kabardian  Lithuanian 
    ## 0.306045427 0.031817286 0.075080089 0.017980000 0.327369959 0.106955710 
    ##      Udmurt       Uzbek       Yakut 
    ## 0.001726942 0.119126603 0.013897983 
    ## 
    ## 22:12:50: Using Polish as treated unit now.
    ## 22:12:50: Number of 'sunny' donors: 36 out of 36
    ## 22:12:50: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:50: with RMSPE 0.159800710978158 and MSPE (loss v) 
    ## 22:12:50: 0.0255362672291247 is INFEASIBLE when respecting the predictors.
    ## 22:12:50: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:53: Optimization finished (36421 calls to inner optimizer), rmspe: 
    ## 22:12:53: 0.159800710978361, mspe: 0.0255362672291898.
    ## Final rmspe: 0.1598007, mspe (loss v): 0.02553627
    ## Optimal weights:
    ## Belorussian       Greek      Jewish    Karelian  Lithuanian     Russian 
    ##  0.20176333  0.20668586  0.21187644  0.05201336  0.06161524  0.08100896 
    ##   Ukrainian       Yakut 
    ##  0.16133509  0.02370173 
    ## 
    ## 22:12:53: Using Russian as treated unit now.
    ## 22:12:53: Number of 'sunny' donors: 36 out of 36
    ## 22:12:53: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:53: with RMSPE 3.2501348996931 and MSPE (loss v) 10.5633768662031 is 
    ## 22:12:53: INFEASIBLE when respecting the predictors.
    ## 22:12:53: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:54: Optimization finished (24661 calls to inner optimizer), rmspe: 
    ## 22:12:54: 3.25013489969315, mspe: 10.5633768662034.
    ## Final rmspe: 3.250135, mspe (loss v): 10.56338
    ## Optimal weights:
    ## Belorussian   Ukrainian 
    ##   0.4984924   0.5015076 
    ## 
    ## 22:12:54: Using Tatar as treated unit now.
    ## 22:12:54: Number of 'sunny' donors: 36 out of 36
    ## 22:12:54: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:54: with RMSPE 0.442175126264263 and MSPE (loss v) 0.195518842286817 
    ## 22:12:54: is INFEASIBLE when respecting the predictors.
    ## 22:12:55: Starting optimization via DEoptC, random seed 2019.
    ## 22:12:57: Optimization finished (40141 calls to inner optimizer), rmspe: 
    ## 22:12:57: 0.442175126264269, mspe: 0.195518842286822.
    ## Final rmspe: 0.4421751, mspe (loss v): 0.1955188
    ## Optimal weights:
    ##    Bashkir    Chuvash     Kazakh    Russian  Ukrainian 
    ## 0.12075474 0.03446493 0.51847744 0.16924471 0.15705819 
    ## 
    ## 22:12:57: Using Udmurt as treated unit now.
    ## 22:12:57: Number of 'sunny' donors: 36 out of 36
    ## 22:12:57: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:12:57: with RMSPE 0.55464115108008 and MSPE (loss v) 0.307626806471436 
    ## 22:12:57: is INFEASIBLE when respecting the predictors.
    ## 22:12:57: Starting optimization via DEoptC, random seed 2019.
    ## 22:13:00: checking v: v contains NAs!
    ## 22:13:00: Optimization finished (26701 calls to inner optimizer), rmspe: 
    ## 22:13:00: 0.554641151080088, mspe: 0.307626806471445.
    ## Final rmspe: 0.5546412, mspe (loss v): 0.3076268
    ## Optimal weights:
    ##     Balkar    Bashkir  Bulgarian    Chechen    Chinese    Chuvash 
    ## 0.01741814 0.63394960 0.02275444 0.07926692 0.13242804 0.03341217 
    ##   Moldovan      Uzbek 
    ## 0.04066148 0.04010920 
    ## 
    ## 22:13:00: Using Ukrainian as treated unit now.
    ## 22:13:00: Number of 'sunny' donors: 36 out of 36
    ## 22:13:00: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:13:00: with RMSPE 0.341993881028781 and MSPE (loss v) 0.116959814661128 
    ## 22:13:00: is INFEASIBLE when respecting the predictors.
    ## 22:13:00: Starting optimization via DEoptC, random seed 2019.
    ## 22:13:02: Optimization finished (27421 calls to inner optimizer), rmspe: 
    ## 22:13:02: 0.341993881029037, mspe: 0.116959814661303.
    ## Final rmspe: 0.3419939, mspe (loss v): 0.1169598
    ## Optimal weights:
    ##      Greek    Chechen       Komi    Mordvin    Russian      Tatar 
    ## 0.03403262 0.19239622 0.13116390 0.03999314 0.29575548 0.30665865 
    ## 
    ## 22:13:02: Using Uzbek as treated unit now.
    ## 22:13:02: Number of 'sunny' donors: 36 out of 36
    ## 22:13:02: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:13:02: with RMSPE 0.572506904317076 and MSPE (loss v) 0.327764155490722 
    ## 22:13:02: is INFEASIBLE when respecting the predictors.
    ## 22:13:02: Starting optimization via DEoptC, random seed 2019.
    ## 22:13:04: Optimization finished (35101 calls to inner optimizer), rmspe: 
    ## 22:13:04: 0.572506904317077, mspe: 0.327764155490722.
    ## Final rmspe: 0.5725069, mspe (loss v): 0.3277642
    ## Optimal weights:
    ##     Balkar    Bashkir      Tatar 
    ## 0.71313654 0.26216187 0.02470159 
    ## 
    ## 22:13:04: Using Yakut as treated unit now.
    ## 22:13:04: Number of 'sunny' donors: 36 out of 36
    ## 22:13:04: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 22:13:04: with RMSPE 0.957923493797545 and MSPE (loss v) 0.917617419969296 
    ## 22:13:04: is INFEASIBLE when respecting the predictors.
    ## 22:13:04: Starting optimization via DEoptC, random seed 2019.
    ## 22:13:06: Optimization finished (26101 calls to inner optimizer), rmspe: 
    ## 22:13:06: 0.957923493797573, mspe: 0.917617419969348.
    ## Final rmspe: 0.9579235, mspe (loss v): 0.9176174
    ## Optimal weights:
    ##   Armenian       Komi    Russian 
    ## 0.81020652 0.07132729 0.11846619

``` r
synth_data <- sc_placebo_pred_full_imp_date$German$data.synth[["log_n_pred_full_imp_date"]] %>% 
  tk_tbl() 

treat_data <- sc_placebo_pred_full_imp_date$German$data.treat[["log_n_pred_full_imp_date"]] %>% 
  tk_tbl() 

effects_data <- treat_data %>% 
  left_join(synth_data, by = "index", suffix = c("_treat", "_synth")) %>% 
  mutate(count_synth = exp(value_synth) - 1,
         count_treat = exp(value_treat) - 1,
         effect_count = round(count_treat- count_synth),
         effect_log = value_treat - value_synth,
         effect_pct_change_naive = 100 * (exp(effect_log) - 1),
         effect_count_based_on_pct = arrests_in_year() * (effect_pct_change_naive/100),
         effect_count_based_on_pct_rounded = round(effect_count_based_on_pct)) 


total_effects_synth <- function(data, year_from = 1932, year_til = 1961){
  data %>% 
    filter(index > year_from, index < year_til) %>% 
    summarize_at(c("effect_count", "count_synth", "count_treat", "effect_count_based_on_pct"), sum) %>% 
    as_tibble()
}

total_effects_synth(effects_data)
```

    ## # A tibble: 1 x 4
    ##   effect_count count_synth count_treat effect_count_based_on_pct
    ##          <dbl>       <dbl>       <dbl>                     <dbl>
    ## 1       127880      25180.     153060.                    20876.

``` r
total_effects_synth(effects_data, year_from = 1932, year_til = 1940)
```

    ## # A tibble: 1 x 4
    ##   effect_count count_synth count_treat effect_count_based_on_pct
    ##          <dbl>       <dbl>       <dbl>                     <dbl>
    ## 1        16526      14515.      31041.                     1625.

``` r
total_effects_synth(effects_data, year_from = 1921, year_til = 1933)
```

    ## # A tibble: 1 x 4
    ##   effect_count count_synth count_treat effect_count_based_on_pct
    ##          <dbl>       <dbl>       <dbl>                     <dbl>
    ## 1         1638      16879.      18517.                      25.3
