Synthetic control
================
Martin Kosík
February 14, 2019

``` r
knitr::opts_chunk$set(echo = TRUE,  fig.show = 'hide')
library(tidyverse)
library(broom)
library(lubridate)
library(here)
library(readxl)
library(MSCMT)
library(kableExtra)
library(knitr)
library(gghighlight)
library(timetk)
library(scales)
source(here::here("code/functions.R"))
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
opts_current$set(label = "sc_predictors")
ethnicity_controls %>% 
  arrange(ethnicity) %>% 
  dplyr::select(ethnicity, pop_total, clad_sim, urb_rate_pct, ind_country) %>% 
  kable("latex", booktabs = T, digits = 2, linesep = "", format.args = list(big.mark = " "),
        col.names = c("Ethnic group", "Total population", "Ling. similarity to Russian", 
                      "Urbanization rate", "Ind. state"),
        caption = "Pre-treatment characteristics of ethnic groups in the USSR") %>%
  footnote(general = "Total population and urbanization rate of the ethnic group in the USSR is taken from 1926 census. The linguistic similarity to Russian is measured by the number of common nodes in the language tree (cladistic similarity). Independent state equals one if the ethnic group was a core group in an independent country that existed in the interwar period.",
           threeparttable = T) %>% 
  kable_styling(latex_options = c("hold_position")) %>% 
    write_file(here::here("tables/sc_predictors.tex"))
```

Data preparation

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
sc_results <- mscmt(data_prep_mscmt, treatment.identifier, controls.identifier, times.dep, times.pred, agg.fns, seed=2019)
```

    ## 21:37:55: Number of 'sunny' donors: 37 out of 37
    ## 21:37:55: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:37:55: with RMSPE 0.147834274985798 and MSPE (loss v) 
    ## 21:37:55: 0.0218549728605767 is INFEASIBLE when respecting the predictors.
    ## 21:37:55: Starting optimization via DEoptC, random seed 2019.
    ## 21:37:58: Optimization finished (31261 calls to inner optimizer), rmspe: 
    ## 21:37:58: 0.147834274989802, mspe: 0.0218549728617605.
    ## Final rmspe: 0.1478343, mspe (loss v): 0.02185497
    ## Optimal weights:
    ##   Georgian      Greek    Chuvash     Korean Lithuanian   Ossetian 
    ## 0.05188119 0.31903984 0.06847046 0.01897588 0.10282469 0.08006683 
    ##    Russian      Tatar 
    ## 0.30709238 0.05164872

``` r
dep_var <- "log_n_pred_full_imp_date"
times.dep  <- cbind("log_n_pred_full_imp_date" = c("1921","1932"))

sc_placebo_pred_full_imp_date <- mscmt(data_prep_mscmt, treatment.identifier, controls.identifier, times.dep,
                                       times.pred, agg.fns, seed=2019, placebo = TRUE, single.v=TRUE)
```

    ## 21:37:58: Starting placebo study, excluding original treated unit.
    ## 21:37:58: Using German as treated unit now.
    ## 21:37:58: Number of 'sunny' donors: 37 out of 37
    ## 21:37:58: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:37:58: with RMSPE 0.147834274985798 and MSPE (loss v) 
    ## 21:37:58: 0.0218549728605767 is INFEASIBLE when respecting the predictors.
    ## 21:37:59: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:02: Optimization finished (31261 calls to inner optimizer), rmspe: 
    ## 21:38:02: 0.147834274989802, mspe: 0.0218549728617605.
    ## Final rmspe: 0.1478343, mspe (loss v): 0.02185497
    ## Optimal weights:
    ##   Georgian      Greek    Chuvash     Korean Lithuanian   Ossetian 
    ## 0.05188119 0.31903984 0.06847046 0.01897588 0.10282469 0.08006683 
    ##    Russian      Tatar 
    ## 0.30709238 0.05164872 
    ## 
    ## 21:38:02: Using Altai as treated unit now.
    ## 21:38:02: Number of 'sunny' donors: 36 out of 36
    ## 21:38:02: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:02: with RMSPE 0.386843865853203 and MSPE (loss v) 0.149648176548251 
    ## 21:38:02: is INFEASIBLE when respecting the predictors.
    ## 21:38:02: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:04: Optimization finished (26041 calls to inner optimizer), rmspe: 
    ## 21:38:04: 0.386843865853212, mspe: 0.149648176548258.
    ## Final rmspe: 0.3868439, mspe (loss v): 0.1496482
    ## Optimal weights:
    ##     Balkar  Hungarian    Chinese  Kabardian     Korean     Udmurt 
    ## 0.39496722 0.12898326 0.07015499 0.07245170 0.18617603 0.14726681 
    ## 
    ## 21:38:04: Using Armenian as treated unit now.
    ## 21:38:04: Number of 'sunny' donors: 36 out of 36
    ## 21:38:04: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:04: with RMSPE 0.297234046607783 and MSPE (loss v) 0.088348078462838 
    ## 21:38:04: is INFEASIBLE when respecting the predictors.
    ## 21:38:04: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:08: Optimization finished (35101 calls to inner optimizer), rmspe: 
    ## 21:38:08: 0.29723404660782, mspe: 0.0883480784628597.
    ## Final rmspe: 0.297234, mspe (loss v): 0.08834808
    ## Optimal weights:
    ##  Hungarian    Chinese     Kalmyk       Komi    Latvian   Ossetian 
    ## 0.11967850 0.11613241 0.08952587 0.14875713 0.04098062 0.33843550 
    ##      Yakut 
    ## 0.14648997 
    ## 
    ## 21:38:08: Using Balkar as treated unit now.
    ## 21:38:08: Number of 'sunny' donors: 36 out of 36
    ## 21:38:08: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:08: with RMSPE 0.422238773128067 and MSPE (loss v) 0.178285581532695 
    ## 21:38:08: is INFEASIBLE when respecting the predictors.
    ## 21:38:08: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:09: Optimization finished (31201 calls to inner optimizer), rmspe: 
    ## 21:38:09: 0.422238773128071, mspe: 0.178285581532698.
    ## Final rmspe: 0.4222388, mspe (loss v): 0.1782856
    ## Optimal weights:
    ##     Altai Bulgarian    Kalmyk     Uzbek 
    ## 0.2419899 0.1524354 0.1750580 0.4305167 
    ## 
    ## 21:38:09: Using Bashkir as treated unit now.
    ## 21:38:10: Number of 'sunny' donors: 36 out of 36
    ## 21:38:10: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:10: with RMSPE 0.373659688496821 and MSPE (loss v) 0.139621562807542 
    ## 21:38:10: is INFEASIBLE when respecting the predictors.
    ## 21:38:10: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:12: Optimization finished (36061 calls to inner optimizer), rmspe: 
    ## 21:38:12: 0.373659688496825, mspe: 0.139621562807544.
    ## Final rmspe: 0.3736597, mspe (loss v): 0.1396216
    ## Optimal weights:
    ##      Greek    Chechen    Mordvin      Tatar     Udmurt  Ukrainian 
    ## 0.06733137 0.03540904 0.29218098 0.11058834 0.47457087 0.01991940 
    ## 
    ## 21:38:13: Using Belorussian as treated unit now.
    ## 21:38:13: Number of 'sunny' donors: 36 out of 36
    ## 21:38:13: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:13: with RMSPE 0.410589187157624 and MSPE (loss v) 0.168583480610759 
    ## 21:38:13: is INFEASIBLE when respecting the predictors.
    ## 21:38:13: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:16: Optimization finished (54001 calls to inner optimizer), rmspe: 
    ## 21:38:16: 0.410589187158154, mspe: 0.168583480611194.
    ## Final rmspe: 0.4105892, mspe (loss v): 0.1685835
    ## Optimal weights:
    ##   Estonian    Chinese     Jewish    Mordvin     Polish    Russian 
    ## 0.03209245 0.27188012 0.03531801 0.11415056 0.18975023 0.35680862 
    ## 
    ## 21:38:16: Using Bulgarian as treated unit now.
    ## 21:38:17: Number of 'sunny' donors: 36 out of 36
    ## 21:38:17: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:17: with RMSPE 0.431870856593713 and MSPE (loss v) 0.186512436774987 
    ## 21:38:17: is INFEASIBLE when respecting the predictors.
    ## 21:38:17: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:19: Optimization finished (33721 calls to inner optimizer), rmspe: 
    ## 21:38:19: 0.431870856593738, mspe: 0.186512436775009.
    ## Final rmspe: 0.4318709, mspe (loss v): 0.1865124
    ## Optimal weights:
    ##     Balkar    Finnish    Chuvash     Kalmyk   Moldovan      Uzbek 
    ## 0.34251135 0.15484241 0.03421413 0.03240790 0.29081579 0.14520842 
    ## 
    ## 21:38:19: Using Buryat as treated unit now.
    ## 21:38:19: Number of 'sunny' donors: 36 out of 36
    ## 21:38:19: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:19: with RMSPE 0.20533236410822 and MSPE (loss v) 0.0421613797502707 
    ## 21:38:19: is INFEASIBLE when respecting the predictors.
    ## 21:38:20: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:22: checking v: v contains NAs!
    ## 21:38:22: Optimization finished (26281 calls to inner optimizer), rmspe: 
    ## 21:38:22: 0.205332364108241, mspe: 0.0421613797502791.
    ## Final rmspe: 0.2053324, mspe (loss v): 0.04216138
    ## Optimal weights:
    ##    Hungarian      Chuvash       Kalmyk     Karelian      Mordvin 
    ## 0.1196810160 0.3803556496 0.1935073507 0.0001422161 0.0889086818 
    ##        Uzbek 
    ## 0.2174050858 
    ## 
    ## 21:38:22: Using Estonian as treated unit now.
    ## 21:38:22: Number of 'sunny' donors: 36 out of 36
    ## 21:38:22: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:22: with RMSPE 0.241632110537875 and MSPE (loss v) 
    ## 21:38:22: 0.0583860768429877 is INFEASIBLE when respecting the predictors.
    ## 21:38:22: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:24: Optimization finished (26821 calls to inner optimizer), rmspe: 
    ## 21:38:24: 0.241632110538961, mspe: 0.0583860768435125.
    ## Final rmspe: 0.2416321, mspe (loss v): 0.05838608
    ## Optimal weights:
    ##       Altai Belorussian    Georgian   Hungarian     Chinese      Jewish 
    ## 0.020749740 0.072912015 0.020740322 0.359858429 0.002228735 0.089808115 
    ##    Karelian      Korean     Russian 
    ## 0.058933158 0.254244221 0.120525264 
    ## 
    ## 21:38:24: Using Finnish as treated unit now.
    ## 21:38:24: Number of 'sunny' donors: 36 out of 36
    ## 21:38:24: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:24: with RMSPE 0.165544878592461 and MSPE (loss v) 
    ## 21:38:24: 0.0274051068281925 is INFEASIBLE when respecting the predictors.
    ## 21:38:24: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:27: Optimization finished (25921 calls to inner optimizer), rmspe: 
    ## 21:38:27: 0.16554487859275, mspe: 0.0274051068282883.
    ## Final rmspe: 0.1655449, mspe (loss v): 0.02740511
    ## Optimal weights:
    ##   Bulgarian     Chechen    Japanese      Khakas        Komi     Latvian 
    ## 0.381192411 0.060864885 0.064862071 0.257840701 0.009947096 0.203780589 
    ##       Tatar 
    ## 0.021512247 
    ## 
    ## 21:38:27: Using Georgian as treated unit now.
    ## 21:38:27: Number of 'sunny' donors: 36 out of 36
    ## 21:38:27: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:27: with RMSPE 0.308621632695222 and MSPE (loss v) 
    ## 21:38:27: 0.0952473121674644 is INFEASIBLE when respecting the predictors.
    ## 21:38:27: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:29: Optimization finished (28621 calls to inner optimizer), rmspe: 
    ## 21:38:29: 0.308621632695238, mspe: 0.0952473121674745.
    ## Final rmspe: 0.3086216, mspe (loss v): 0.09524731
    ## Optimal weights:
    ##     Balkar    Chinese     Korean Lithuanian   Moldovan 
    ## 0.16763472 0.34946050 0.05627929 0.01320429 0.41342120 
    ## 
    ## 21:38:30: Using Greek as treated unit now.
    ## 21:38:30: Number of 'sunny' donors: 36 out of 36
    ## 21:38:30: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:30: with RMSPE 0.298263329338972 and MSPE (loss v) 
    ## 21:38:30: 0.0889610136283681 is INFEASIBLE when respecting the predictors.
    ## 21:38:30: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:32: Optimization finished (31141 calls to inner optimizer), rmspe: 
    ## 21:38:32: 0.298263329338982, mspe: 0.0889610136283743.
    ## Final rmspe: 0.2982633, mspe (loss v): 0.08896101
    ## Optimal weights:
    ##     Khakas    Russian      Tatar  Ukrainian      Yakut 
    ## 0.71609990 0.07157196 0.13211037 0.06828728 0.01193048 
    ## 
    ## 21:38:32: Using Hungarian as treated unit now.
    ## 21:38:32: Number of 'sunny' donors: 36 out of 36
    ## 21:38:32: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:32: with RMSPE 0.880005719757757 and MSPE (loss v) 0.774410066806368 
    ## 21:38:32: is INFEASIBLE when respecting the predictors.
    ## 21:38:33: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:35: Optimization finished (31561 calls to inner optimizer), rmspe: 
    ## 21:38:35: 0.880005719757771, mspe: 0.774410066806392.
    ## Final rmspe: 0.8800057, mspe (loss v): 0.7744101
    ## Optimal weights:
    ##    Chinese     Jewish     Kalmyk 
    ## 0.07860859 0.08548922 0.83590219 
    ## 
    ## 21:38:35: Using Chechen as treated unit now.
    ## 21:38:35: Number of 'sunny' donors: 36 out of 36
    ## 21:38:35: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:35: with RMSPE 0.384180448112309 and MSPE (loss v) 0.147594616711775 
    ## 21:38:35: is INFEASIBLE when respecting the predictors.
    ## 21:38:35: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:38: checking v: v contains NAs!
    ## 21:38:38: Optimization finished (32221 calls to inner optimizer), rmspe: 
    ## 21:38:38: 0.38418044811231, mspe: 0.147594616711775.
    ## Final rmspe: 0.3841804, mspe (loss v): 0.1475946
    ## Optimal weights:
    ##    Bashkir  Bulgarian     Kalmyk   Ossetian     Udmurt 
    ## 0.46485672 0.02856198 0.22951513 0.19209421 0.08497196 
    ## 
    ## 21:38:38: Using Chinese as treated unit now.
    ## 21:38:38: Number of 'sunny' donors: 36 out of 36
    ## 21:38:38: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:38: with RMSPE 0.80914275254453 and MSPE (loss v) 0.654711993995338 
    ## 21:38:38: is INFEASIBLE when respecting the predictors.
    ## 21:38:38: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:40: Optimization finished (30421 calls to inner optimizer), rmspe: 
    ## 21:38:40: 0.809142752544544, mspe: 0.654711993995361.
    ## Final rmspe: 0.8091428, mspe (loss v): 0.654712
    ## Optimal weights:
    ##   Armenian   Georgian  Hungarian  Kabardian   Karelian 
    ## 0.15750643 0.31681390 0.08560617 0.29312061 0.14695290 
    ## 
    ## 21:38:40: Using Chuvash as treated unit now.
    ## 21:38:40: Number of 'sunny' donors: 36 out of 36
    ## 21:38:40: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:40: with RMSPE 0.879896185542204 and MSPE (loss v) 0.77421729733172 
    ## 21:38:40: is INFEASIBLE when respecting the predictors.
    ## 21:38:40: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:42: Optimization finished (26341 calls to inner optimizer), rmspe: 
    ## 21:38:42: 0.879896185542209, mspe: 0.77421729733173.
    ## Final rmspe: 0.8798962, mspe (loss v): 0.7742173
    ## Optimal weights:
    ##     Buryat       Komi    Russian      Tatar     Udmurt 
    ## 0.47566387 0.01035899 0.08750964 0.35166100 0.07480651 
    ## 
    ## 21:38:42: Using Japanese as treated unit now.
    ## 21:38:42: Number of 'sunny' donors: 36 out of 36
    ## 21:38:42: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:42: with RMSPE 0.172288107773526 and MSPE (loss v) 
    ## 21:38:42: 0.0296831920801822 is INFEASIBLE when respecting the predictors.
    ## 21:38:42: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:45: Optimization finished (30001 calls to inner optimizer), rmspe: 
    ## 21:38:45: 0.172288107773628, mspe: 0.0296831920802171.
    ## Final rmspe: 0.1722881, mspe (loss v): 0.02968319
    ## Optimal weights:
    ##       Altai     Finnish      Kalmyk    Karelian      Khakas     Latvian 
    ## 0.152893087 0.279593386 0.075780289 0.003744051 0.090492543 0.115992843 
    ##     Mordvin       Uzbek 
    ## 0.081603042 0.199900759 
    ## 
    ## 21:38:45: Using Jewish as treated unit now.
    ## 21:38:45: Number of 'sunny' donors: 36 out of 36
    ## 21:38:45: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:45: with RMSPE 0.851483322024242 and MSPE (loss v) 0.725023847685439 
    ## 21:38:45: is INFEASIBLE when respecting the predictors.
    ## 21:38:45: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:47: Optimization finished (27061 calls to inner optimizer), rmspe: 
    ## 21:38:47: 0.851483322024261, mspe: 0.725023847685472.
    ## Final rmspe: 0.8514833, mspe (loss v): 0.7250238
    ## Optimal weights:
    ##  Hungarian   Karelian    Russian 
    ## 0.45754702 0.04976069 0.49269229 
    ## 
    ## 21:38:47: Using Kabardian as treated unit now.
    ## 21:38:47: Number of 'sunny' donors: 36 out of 36
    ## 21:38:47: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:47: with RMSPE 0.508795998019183 and MSPE (loss v) 0.258873367600336 
    ## 21:38:47: is INFEASIBLE when respecting the predictors.
    ## 21:38:47: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:49: checking v: v contains NAs!
    ## 21:38:49: Optimization finished (31561 calls to inner optimizer), rmspe: 
    ## 21:38:49: 0.508795998019183, mspe: 0.258873367600336.
    ## Final rmspe: 0.508796, mspe (loss v): 0.2588734
    ## Optimal weights:
    ##      Altai    Bashkir   Ossetian      Tatar      Uzbek 
    ## 0.33967513 0.03019005 0.36263050 0.12619624 0.14130808 
    ## 
    ## 21:38:49: Using Kalmyk as treated unit now.
    ## 21:38:49: Number of 'sunny' donors: 36 out of 36
    ## 21:38:49: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:49: with RMSPE 0.906837145905573 and MSPE (loss v) 0.822353609194166 
    ## 21:38:49: is INFEASIBLE when respecting the predictors.
    ## 21:38:49: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:51: checking v: v contains NAs!
    ## 21:38:51: Optimization finished (27541 calls to inner optimizer), rmspe: 
    ## 21:38:51: 0.906837145905597, mspe: 0.82235360919421.
    ## Final rmspe: 0.9068371, mspe (loss v): 0.8223536
    ## Optimal weights:
    ## Bulgarian Hungarian  Moldovan 
    ## 0.1158291 0.7474566 0.1367142 
    ## 
    ## 21:38:51: Using Karelian as treated unit now.
    ## 21:38:51: Number of 'sunny' donors: 36 out of 36
    ## 21:38:51: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:51: with RMSPE 1.1489962739756 and MSPE (loss v) 1.32019243760982 is 
    ## 21:38:51: INFEASIBLE when respecting the predictors.
    ## 21:38:51: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:52: Optimization finished (25021 calls to inner optimizer), rmspe: 
    ## 21:38:52: 1.14899627397561, mspe: 1.32019243760984.
    ## Final rmspe: 1.148996, mspe (loss v): 1.320192
    ## Optimal weights:
    ##  Hungarian    Chinese     Jewish       Komi 
    ## 0.23376112 0.04948875 0.05225700 0.66449313 
    ## 
    ## 21:38:52: Using Kazakh as treated unit now.
    ## 21:38:53: Number of 'sunny' donors: 36 out of 36
    ## 21:38:53: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:53: with RMSPE 0.182550505205933 and MSPE (loss v) 
    ## 21:38:53: 0.0333246869509413 is INFEASIBLE when respecting the predictors.
    ## 21:38:53: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:55: Optimization finished (27241 calls to inner optimizer), rmspe: 
    ## 21:38:55: 0.182550505205934, mspe: 0.0333246869509417.
    ## Final rmspe: 0.1825505, mspe (loss v): 0.03332469
    ## Optimal weights:
    ##     Bashkir   Bulgarian     Chuvash   Kabardian      Korean     Mordvin 
    ## 0.035029841 0.004386157 0.083568809 0.071109525 0.192456581 0.044077888 
    ##       Tatar       Uzbek 
    ## 0.472227325 0.097143873 
    ## 
    ## 21:38:55: Using Khakas as treated unit now.
    ## 21:38:55: Number of 'sunny' donors: 36 out of 36
    ## 21:38:55: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:55: with RMSPE 0.271855125972731 and MSPE (loss v) 
    ## 21:38:55: 0.0739052095176495 is INFEASIBLE when respecting the predictors.
    ## 21:38:55: Starting optimization via DEoptC, random seed 2019.
    ## 21:38:57: Optimization finished (29341 calls to inner optimizer), rmspe: 
    ## 21:38:57: 0.271855125972733, mspe: 0.0739052095176506.
    ## Final rmspe: 0.2718551, mspe (loss v): 0.07390521
    ## Optimal weights:
    ##     Balkar    Bashkir    Finnish      Greek     Kalmyk    Mordvin 
    ## 0.31902607 0.07798229 0.07347298 0.39896290 0.01935135 0.11120440 
    ## 
    ## 21:38:57: Using Komi as treated unit now.
    ## 21:38:58: Number of 'sunny' donors: 36 out of 36
    ## 21:38:58: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:38:58: with RMSPE 0.658636954790221 and MSPE (loss v) 0.433802638215336 
    ## 21:38:58: is INFEASIBLE when respecting the predictors.
    ## 21:38:58: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:00: checking v: v contains NAs!
    ## 21:39:00: Optimization finished (27481 calls to inner optimizer), rmspe: 
    ## 21:39:00: 0.658636954790242, mspe: 0.433802638215364.
    ## Final rmspe: 0.658637, mspe (loss v): 0.4338026
    ## Optimal weights:
    ##    Chuvash     Kalmyk   Karelian  Ukrainian      Yakut 
    ## 0.09051473 0.18527856 0.33650143 0.26328530 0.12441997 
    ## 
    ## 21:39:00: Using Korean as treated unit now.
    ## 21:39:00: Number of 'sunny' donors: 36 out of 36
    ## 21:39:00: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:00: with RMSPE 0.260310204114686 and MSPE (loss v) 
    ## 21:39:00: 0.0677614023662296 is INFEASIBLE when respecting the predictors.
    ## 21:39:00: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:02: Optimization finished (29641 calls to inner optimizer), rmspe: 
    ## 21:39:02: 0.260310204115713, mspe: 0.0677614023667641.
    ## Final rmspe: 0.2603102, mspe (loss v): 0.0677614
    ## Optimal weights:
    ##     Altai    Jewish      Mari   Russian 
    ## 0.4417795 0.1496721 0.2183209 0.1902275 
    ## 
    ## 21:39:02: Using Latvian as treated unit now.
    ## 21:39:02: Number of 'sunny' donors: 36 out of 36
    ## 21:39:02: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:02: with RMSPE 0.379033432874932 and MSPE (loss v) 0.143666343236955 
    ## 21:39:02: is INFEASIBLE when respecting the predictors.
    ## 21:39:02: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:05: Optimization finished (26941 calls to inner optimizer), rmspe: 
    ## 21:39:05: 0.379033432874991, mspe: 0.143666343237.
    ## Final rmspe: 0.3790334, mspe (loss v): 0.1436663
    ## Optimal weights:
    ##    Armenian     Bashkir Belorussian     Finnish      Jewish    Karelian 
    ## 0.322917012 0.081161642 0.036377861 0.030278939 0.360947858 0.054949452 
    ##    Ossetian      Udmurt   Ukrainian 
    ## 0.098700380 0.012317536 0.002349319 
    ## 
    ## 21:39:05: Using Lithuanian as treated unit now.
    ## 21:39:05: Number of 'sunny' donors: 36 out of 36
    ## 21:39:05: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:05: with RMSPE 0.244538664565769 and MSPE (loss v) 
    ## 21:39:05: 0.0597991584676094 is INFEASIBLE when respecting the predictors.
    ## 21:39:05: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:08: Optimization finished (39361 calls to inner optimizer), rmspe: 
    ## 21:39:08: 0.244538664565912, mspe: 0.0597991584676796.
    ## Final rmspe: 0.2445387, mspe (loss v): 0.05979916
    ## Optimal weights:
    ##   Georgian     Jewish     Kalmyk   Karelian   Ossetian      Yakut 
    ## 0.03268453 0.10752792 0.28655958 0.10095670 0.35156628 0.12070500 
    ## 
    ## 21:39:08: Using Mari as treated unit now.
    ## 21:39:08: Number of 'sunny' donors: 36 out of 36
    ## 21:39:08: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:08: with RMSPE 0.100134192932982 and MSPE (loss v) 
    ## 21:39:08: 0.0100268565943397 is INFEASIBLE when respecting the predictors.
    ## 21:39:08: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:10: Optimization finished (31561 calls to inner optimizer), rmspe: 
    ## 21:39:10: 0.100134192936011, mspe: 0.0100268565949462.
    ## Final rmspe: 0.1001342, mspe (loss v): 0.01002686
    ## Optimal weights:
    ##     Chinese    Japanese   Kabardian      Korean     Mordvin     Russian 
    ## 0.003856797 0.040094861 0.023399628 0.540961784 0.083032566 0.134992757 
    ##       Uzbek       Yakut 
    ## 0.125572610 0.048088996 
    ## 
    ## 21:39:10: Using Moldovan as treated unit now.
    ## 21:39:11: Number of 'sunny' donors: 36 out of 36
    ## 21:39:11: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:11: with RMSPE 0.549796788834761 and MSPE (loss v) 0.302276509013015 
    ## 21:39:11: is INFEASIBLE when respecting the predictors.
    ## 21:39:11: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:13: Optimization finished (28441 calls to inner optimizer), rmspe: 
    ## 21:39:13: 0.549796788834762, mspe: 0.302276509013015.
    ## Final rmspe: 0.5497968, mspe (loss v): 0.3022765
    ## Optimal weights:
    ##  Bulgarian   Georgian     Kalmyk    Russian     Udmurt      Yakut 
    ## 0.19180497 0.09431761 0.49543475 0.05139067 0.12024748 0.04680452 
    ## 
    ## 21:39:13: Using Mordvin as treated unit now.
    ## 21:39:13: Number of 'sunny' donors: 36 out of 36
    ## 21:39:13: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:13: with RMSPE 0.621813001602366 and MSPE (loss v) 0.386651408961744 
    ## 21:39:13: is INFEASIBLE when respecting the predictors.
    ## 21:39:13: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:15: Optimization finished (28201 calls to inner optimizer), rmspe: 
    ## 21:39:15: 0.621813001602368, mspe: 0.386651408961747.
    ## Final rmspe: 0.621813, mspe (loss v): 0.3866514
    ## Optimal weights:
    ##    Bashkir    Chuvash     Khakas    Russian 
    ## 0.63655592 0.12383072 0.16565222 0.07396114 
    ## 
    ## 21:39:15: Using Ossetian as treated unit now.
    ## 21:39:15: Number of 'sunny' donors: 36 out of 36
    ## 21:39:15: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:15: with RMSPE 0.283613568696048 and MSPE (loss v) 
    ## 21:39:15: 0.0804366563485079 is INFEASIBLE when respecting the predictors.
    ## 21:39:15: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:18: Optimization finished (29761 calls to inner optimizer), rmspe: 
    ## 21:39:18: 0.283613568696094, mspe: 0.080436656348534.
    ## Final rmspe: 0.2836136, mspe (loss v): 0.08043666
    ## Optimal weights:
    ##    Armenian      Balkar   Bulgarian     Chechen   Kabardian  Lithuanian 
    ## 0.306045427 0.031817286 0.075080089 0.017980000 0.327369959 0.106955710 
    ##      Udmurt       Uzbek       Yakut 
    ## 0.001726942 0.119126603 0.013897983 
    ## 
    ## 21:39:18: Using Polish as treated unit now.
    ## 21:39:18: Number of 'sunny' donors: 36 out of 36
    ## 21:39:18: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:18: with RMSPE 0.159800710978158 and MSPE (loss v) 
    ## 21:39:18: 0.0255362672291247 is INFEASIBLE when respecting the predictors.
    ## 21:39:18: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:21: Optimization finished (36421 calls to inner optimizer), rmspe: 
    ## 21:39:21: 0.159800710978361, mspe: 0.0255362672291898.
    ## Final rmspe: 0.1598007, mspe (loss v): 0.02553627
    ## Optimal weights:
    ## Belorussian       Greek      Jewish    Karelian  Lithuanian     Russian 
    ##  0.20176333  0.20668586  0.21187644  0.05201336  0.06161524  0.08100896 
    ##   Ukrainian       Yakut 
    ##  0.16133509  0.02370173 
    ## 
    ## 21:39:21: Using Russian as treated unit now.
    ## 21:39:21: Number of 'sunny' donors: 36 out of 36
    ## 21:39:21: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:21: with RMSPE 3.2501348996931 and MSPE (loss v) 10.5633768662031 is 
    ## 21:39:21: INFEASIBLE when respecting the predictors.
    ## 21:39:22: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:23: Optimization finished (24661 calls to inner optimizer), rmspe: 
    ## 21:39:23: 3.25013489969315, mspe: 10.5633768662034.
    ## Final rmspe: 3.250135, mspe (loss v): 10.56338
    ## Optimal weights:
    ## Belorussian   Ukrainian 
    ##   0.4984924   0.5015076 
    ## 
    ## 21:39:23: Using Tatar as treated unit now.
    ## 21:39:23: Number of 'sunny' donors: 36 out of 36
    ## 21:39:23: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:23: with RMSPE 0.442175126264263 and MSPE (loss v) 0.195518842286817 
    ## 21:39:23: is INFEASIBLE when respecting the predictors.
    ## 21:39:23: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:25: Optimization finished (40141 calls to inner optimizer), rmspe: 
    ## 21:39:25: 0.442175126264269, mspe: 0.195518842286822.
    ## Final rmspe: 0.4421751, mspe (loss v): 0.1955188
    ## Optimal weights:
    ##    Bashkir    Chuvash     Kazakh    Russian  Ukrainian 
    ## 0.12075474 0.03446493 0.51847744 0.16924471 0.15705819 
    ## 
    ## 21:39:25: Using Udmurt as treated unit now.
    ## 21:39:26: Number of 'sunny' donors: 36 out of 36
    ## 21:39:26: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:26: with RMSPE 0.55464115108008 and MSPE (loss v) 0.307626806471436 
    ## 21:39:26: is INFEASIBLE when respecting the predictors.
    ## 21:39:26: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:28: checking v: v contains NAs!
    ## 21:39:28: Optimization finished (26701 calls to inner optimizer), rmspe: 
    ## 21:39:28: 0.554641151080088, mspe: 0.307626806471445.
    ## Final rmspe: 0.5546412, mspe (loss v): 0.3076268
    ## Optimal weights:
    ##     Balkar    Bashkir  Bulgarian    Chechen    Chinese    Chuvash 
    ## 0.01741814 0.63394960 0.02275444 0.07926692 0.13242804 0.03341217 
    ##   Moldovan      Uzbek 
    ## 0.04066148 0.04010920 
    ## 
    ## 21:39:28: Using Ukrainian as treated unit now.
    ## 21:39:28: Number of 'sunny' donors: 36 out of 36
    ## 21:39:28: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:28: with RMSPE 0.341993881028781 and MSPE (loss v) 0.116959814661128 
    ## 21:39:28: is INFEASIBLE when respecting the predictors.
    ## 21:39:28: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:30: Optimization finished (27421 calls to inner optimizer), rmspe: 
    ## 21:39:30: 0.341993881029037, mspe: 0.116959814661303.
    ## Final rmspe: 0.3419939, mspe (loss v): 0.1169598
    ## Optimal weights:
    ##      Greek    Chechen       Komi    Mordvin    Russian      Tatar 
    ## 0.03403262 0.19239622 0.13116390 0.03999314 0.29575548 0.30665865 
    ## 
    ## 21:39:30: Using Uzbek as treated unit now.
    ## 21:39:30: Number of 'sunny' donors: 36 out of 36
    ## 21:39:30: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:30: with RMSPE 0.572506904317076 and MSPE (loss v) 0.327764155490722 
    ## 21:39:30: is INFEASIBLE when respecting the predictors.
    ## 21:39:30: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:32: Optimization finished (35101 calls to inner optimizer), rmspe: 
    ## 21:39:32: 0.572506904317077, mspe: 0.327764155490722.
    ## Final rmspe: 0.5725069, mspe (loss v): 0.3277642
    ## Optimal weights:
    ##     Balkar    Bashkir      Tatar 
    ## 0.71313654 0.26216187 0.02470159 
    ## 
    ## 21:39:32: Using Yakut as treated unit now.
    ## 21:39:32: Number of 'sunny' donors: 36 out of 36
    ## 21:39:32: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:32: with RMSPE 0.957923493797545 and MSPE (loss v) 0.917617419969296 
    ## 21:39:32: is INFEASIBLE when respecting the predictors.
    ## 21:39:33: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:35: Optimization finished (26101 calls to inner optimizer), rmspe: 
    ## 21:39:35: 0.957923493797573, mspe: 0.917617419969348.
    ## Final rmspe: 0.9579235, mspe (loss v): 0.9176174
    ## Optimal weights:
    ##   Armenian       Komi    Russian 
    ## 0.81020652 0.07132729 0.11846619

``` r
agg.fns <- rep("mean", ncol(times.pred))                       
dep_var <- "log_n_pred_full_imp_date"
times.dep  <- cbind("log_n_pred_full_imp_date" = c("1921","1932"))

sc_placebo_pred_full_imp_date_mean <- mscmt(data_prep_mscmt, treatment.identifier, controls.identifier, times.dep, times.pred, agg.fns, seed=2019, placebo = TRUE, single.v=TRUE)
```

    ## 21:39:36: Starting placebo study, excluding original treated unit.
    ## 21:39:36: Using German as treated unit now.
    ## 21:39:36: Number of 'sunny' donors: 6 out of 37
    ## 21:39:36: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:36: with RMSPE 0.147834274985798 and MSPE (loss v) 
    ## 21:39:36: 0.0218549728605767 is INFEASIBLE when respecting the predictors.
    ## 21:39:36: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:36: Optimization finished (48121 calls to inner optimizer), rmspe: 
    ## 21:39:36: 0.294405107674494, mspe: 0.0866743674248306.
    ## Final rmspe: 0.2944051, mspe (loss v): 0.08667437
    ## Optimal weights:
    ##       Korean         Mari       Polish        Tatar 
    ## 1.384854e-01 1.001108e-14 4.114940e-01 4.500206e-01 
    ## 
    ## 21:39:36: Using Altai as treated unit now.
    ## 21:39:37: Number of 'sunny' donors: 11 out of 36
    ## 21:39:37: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:37: with RMSPE 0.386843865853203 and MSPE (loss v) 0.149648176548251 
    ## 21:39:37: is INFEASIBLE when respecting the predictors.
    ## 21:39:37: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:37: Optimization finished (25021 calls to inner optimizer), rmspe: 
    ## 21:39:37: 0.755827685746493, mspe: 0.571275490540899.
    ## Final rmspe: 0.7558277, mspe (loss v): 0.5712755
    ## Optimal weights:
    ## Balkar Khakas 
    ## 0.5319 0.4681 
    ## 
    ## 21:39:37: Using Armenian as treated unit now.
    ## 21:39:37: No 'sunny' donors!
    ## Final rmspe: 0.707594, mspe (loss v): 0.5006893
    ## Optimal weights:
    ##      Altai  Bulgarian  Hungarian Lithuanian    Russian 
    ## 0.40712760 0.02323210 0.13401172 0.41594641 0.01968218 
    ## 
    ## 21:39:37: Using Balkar as treated unit now.
    ## 21:39:37: Number of 'sunny' donors: 6 out of 36
    ## 21:39:37: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:37: with RMSPE 0.422238773128067 and MSPE (loss v) 0.178285581532695 
    ## 21:39:37: is INFEASIBLE when respecting the predictors.
    ## 21:39:37: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:37: Optimization finished (36901 calls to inner optimizer), rmspe: 
    ## 21:39:37: 0.737457269730461, mspe: 0.543843224678305.
    ## Final rmspe: 0.7374573, mspe (loss v): 0.5438432
    ## Optimal weights:
    ##      Altai  Hungarian     Kalmyk 
    ## 0.78408417 0.01157795 0.20433788 
    ## 
    ## 21:39:37: Using Bashkir as treated unit now.
    ## 21:39:37: No 'sunny' donors!
    ## Final rmspe: 0.8862448, mspe (loss v): 0.7854298
    ## Optimal weights:
    ##     Kazakh     Khakas       Mari      Uzbek 
    ## 0.07475224 0.48961433 0.37574745 0.05988599 
    ## 
    ## 21:39:37: Using Belorussian as treated unit now.
    ## 21:39:38: Number of 'sunny' donors: 8 out of 36
    ## 21:39:38: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:38: with RMSPE 0.410589187157624 and MSPE (loss v) 0.168583480610759 
    ## 21:39:38: is INFEASIBLE when respecting the predictors.
    ## 21:39:38: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:38: Optimization finished (44701 calls to inner optimizer), rmspe: 
    ## 21:39:38: 0.508614379117158, mspe: 0.258688586644732.
    ## Final rmspe: 0.5086144, mspe (loss v): 0.2586886
    ## Optimal weights:
    ##      Mari    Polish   Russian 
    ## 0.3255947 0.5132070 0.1611983 
    ## 
    ## 21:39:38: Using Bulgarian as treated unit now.
    ## 21:39:38: Number of 'sunny' donors: 14 out of 36
    ## 21:39:38: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:38: with RMSPE 0.431870856593713 and MSPE (loss v) 0.186512436774987 
    ## 21:39:38: is INFEASIBLE when respecting the predictors.
    ## 21:39:38: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:38: Optimization finished (40861 calls to inner optimizer), rmspe: 
    ## 21:39:38: 0.523508729275586, mspe: 0.274061389627739.
    ## Final rmspe: 0.5235087, mspe (loss v): 0.2740614
    ## Optimal weights:
    ##     Balkar    Latvian Lithuanian   Moldovan 
    ## 0.60095162 0.06849142 0.01737864 0.31317833 
    ## 
    ## 21:39:38: Using Buryat as treated unit now.
    ## 21:39:38: No 'sunny' donors!
    ## Final rmspe: 0.9100318, mspe (loss v): 0.8281579
    ## Optimal weights:
    ##      Altai     Kazakh       Mari      Tatar 
    ## 0.92201903 0.01457578 0.01658908 0.04681611 
    ## 
    ## 21:39:39: Using Estonian as treated unit now.
    ## 21:39:39: No 'sunny' donors!
    ## Final rmspe: 0.8940706, mspe (loss v): 0.7993622
    ## Optimal weights:
    ##      Altai    Chinese   Japanese     Korean      Tatar 
    ## 0.21876613 0.05536332 0.17525925 0.51610590 0.03450540 
    ## 
    ## 21:39:39: Using Finnish as treated unit now.
    ## 21:39:39: No 'sunny' donors!
    ## Final rmspe: 0.8120539, mspe (loss v): 0.6594315
    ## Optimal weights:
    ##      Altai  Hungarian     Korean       Mari      Tatar 
    ## 0.62376696 0.14484778 0.07986522 0.13636364 0.01515641 
    ## 
    ## 21:39:39: Using Georgian as treated unit now.
    ## 21:39:39: No 'sunny' donors!
    ## Final rmspe: 0.6682458, mspe (loss v): 0.4465524
    ## Optimal weights:
    ##  Japanese    Kalmyk    Kazakh     Uzbek 
    ## 0.1515881 0.3987168 0.2154703 0.2342247 
    ## 
    ## 21:39:39: Using Greek as treated unit now.
    ## 21:39:39: No 'sunny' donors!
    ## Final rmspe: 0.9237812, mspe (loss v): 0.8533716
    ## Optimal weights:
    ##      Altai  Bulgarian   Japanese     Korean     Polish 
    ## 0.03234046 0.13978905 0.11113463 0.52319157 0.19354429 
    ## 
    ## 21:39:39: Using Hungarian as treated unit now.
    ## 21:39:39: Number of 'sunny' donors: 8 out of 36
    ## 21:39:39: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:39: with RMSPE 0.880005719757757 and MSPE (loss v) 0.774410066806368 
    ## 21:39:39: is INFEASIBLE when respecting the predictors.
    ## 21:39:39: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:40: Optimization finished (28381 calls to inner optimizer), rmspe: 
    ## 21:39:40: 0.917648984081117, mspe: 0.842079657985106.
    ## Final rmspe: 0.917649, mspe (loss v): 0.8420797
    ## Optimal weights:
    ##  Chinese   Kalmyk 
    ## 0.226107 0.773893 
    ## 
    ## 21:39:40: Using Chechen as treated unit now.
    ## 21:39:40: No 'sunny' donors!
    ## Final rmspe: 1.00626, mspe (loss v): 1.01256
    ## Optimal weights:
    ##       Altai      Kazakh       Tatar       Uzbek 
    ## 0.926369359 0.039583461 0.007776471 0.026270709 
    ## 
    ## 21:39:40: Using Chinese as treated unit now.
    ## 21:39:40: No 'sunny' donors!
    ## Final rmspe: 1.078746, mspe (loss v): 1.163694
    ## Optimal weights:
    ##      Altai   Estonian  Hungarian   Japanese 
    ## 0.08111736 0.03620342 0.25949584 0.62318338 
    ## 
    ## 21:39:40: Using Chuvash as treated unit now.
    ## 21:39:40: No 'sunny' donors!
    ## Final rmspe: 1.267214, mspe (loss v): 1.605831
    ## Optimal weights:
    ##      Altai    Bashkir     Kazakh       Mari      Tatar 
    ## 0.17004034 0.05263158 0.18331400 0.55727515 0.03673893 
    ## 
    ## 21:39:40: Using Japanese as treated unit now.
    ## 21:39:40: Number of 'sunny' donors: 13 out of 36
    ## 21:39:40: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:40: with RMSPE 0.172288107773526 and MSPE (loss v) 
    ## 21:39:40: 0.0296831920801822 is INFEASIBLE when respecting the predictors.
    ## 21:39:40: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:41: Optimization finished (58441 calls to inner optimizer), rmspe: 
    ## 21:39:41: 1.07834884080061, mspe: 1.16283622245602.
    ## Final rmspe: 1.078349, mspe (loss v): 1.162836
    ## Optimal weights:
    ##      Chinese       Korean      Latvian       Polish 
    ## 7.201672e-01 7.469146e-02 2.051413e-01 1.015715e-13 
    ## 
    ## 21:39:41: Using Jewish as treated unit now.
    ## 21:39:41: Number of 'sunny' donors: 7 out of 36
    ## 21:39:41: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:41: with RMSPE 0.851483322024242 and MSPE (loss v) 0.725023847685439 
    ## 21:39:41: is INFEASIBLE when respecting the predictors.
    ## 21:39:41: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:41: Optimization finished (36601 calls to inner optimizer), rmspe: 
    ## 21:39:41: 1.29866125903372, mspe: 1.68652106571505.
    ## Final rmspe: 1.298661, mspe (loss v): 1.686521
    ## Optimal weights:
    ##    Japanese      Polish     Russian 
    ## 0.004076753 0.972278821 0.023644425 
    ## 
    ## 21:39:41: Using Kabardian as treated unit now.
    ## 21:39:41: No 'sunny' donors!
    ## Final rmspe: 0.7285003, mspe (loss v): 0.5307127
    ## Optimal weights:
    ##       Altai     Bashkir      Korean        Mari       Tatar 
    ## 0.788942429 0.109603348 0.067628870 0.029598929 0.004226425 
    ## 
    ## 21:39:41: Using Kalmyk as treated unit now.
    ## 21:39:41: Number of 'sunny' donors: 9 out of 36
    ## 21:39:41: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:41: with RMSPE 0.906837145905573 and MSPE (loss v) 0.822353609194166 
    ## 21:39:41: is INFEASIBLE when respecting the predictors.
    ## 21:39:41: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:41: Optimization finished (32461 calls to inner optimizer), rmspe: 
    ## 21:39:41: 0.947641141818358, mspe: 0.898023733666801.
    ## Final rmspe: 0.9476411, mspe (loss v): 0.8980237
    ## Optimal weights:
    ##    Balkar Hungarian 
    ## 0.1814352 0.8185648 
    ## 
    ## 21:39:42: Using Karelian as treated unit now.
    ## 21:39:42: No 'sunny' donors!
    ## Final rmspe: 2.150421, mspe (loss v): 4.624312
    ## Optimal weights:
    ##      Altai   Japanese     Korean      Tatar      Uzbek 
    ## 0.86601730 0.01267572 0.06145833 0.02504105 0.03480760 
    ## 
    ## 21:39:42: Using Kazakh as treated unit now.
    ## 21:39:42: Number of 'sunny' donors: 8 out of 36
    ## 21:39:42: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:42: with RMSPE 0.182550505205933 and MSPE (loss v) 
    ## 21:39:42: 0.0333246869509413 is INFEASIBLE when respecting the predictors.
    ## 21:39:42: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:42: Optimization finished (41101 calls to inner optimizer), rmspe: 
    ## 21:39:42: 0.355986827212655, mspe: 0.126726621148932.
    ## Final rmspe: 0.3559868, mspe (loss v): 0.1267266
    ## Optimal weights:
    ##    Mordvin    Russian      Tatar      Uzbek 
    ## 0.10134700 0.01284425 0.63301712 0.25279163 
    ## 
    ## 21:39:42: Using Khakas as treated unit now.
    ## 21:39:42: Number of 'sunny' donors: 6 out of 36
    ## 21:39:42: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:42: with RMSPE 0.271855125972731 and MSPE (loss v) 
    ## 21:39:42: 0.0739052095176495 is INFEASIBLE when respecting the predictors.
    ## 21:39:42: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:42: Optimization finished (24841 calls to inner optimizer), rmspe: 
    ## 21:39:42: 0.872905822637892, mspe: 0.761964575195136.
    ## Final rmspe: 0.8729058, mspe (loss v): 0.7619646
    ## Optimal weights:
    ##     Altai  Japanese    Korean 
    ## 0.5289430 0.1834668 0.2875901 
    ## 
    ## 21:39:42: Using Komi as treated unit now.
    ## 21:39:43: No 'sunny' donors!
    ## Final rmspe: 1.72091, mspe (loss v): 2.96153
    ## Optimal weights:
    ##      Altai   Japanese       Mari      Tatar 
    ## 0.67086717 0.01088374 0.23249211 0.08575698 
    ## 
    ## 21:39:43: Using Korean as treated unit now.
    ## 21:39:43: Number of 'sunny' donors: 11 out of 36
    ## 21:39:43: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:43: with RMSPE 0.260310204114686 and MSPE (loss v) 
    ## 21:39:43: 0.0677614023662296 is INFEASIBLE when respecting the predictors.
    ## 21:39:43: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:43: Optimization finished (56941 calls to inner optimizer), rmspe: 
    ## 21:39:43: 0.344405467222257, mspe: 0.118615125852581.
    ## Final rmspe: 0.3444055, mspe (loss v): 0.1186151
    ## Optimal weights:
    ##     Estonian        Greek         Mari 
    ## 1.810485e-01 1.195434e-08 8.189515e-01 
    ## 
    ## 21:39:43: Using Latvian as treated unit now.
    ## 21:39:43: Number of 'sunny' donors: 5 out of 36
    ## 21:39:43: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:43: with RMSPE 0.379033432874932 and MSPE (loss v) 0.143666343236955 
    ## 21:39:43: is INFEASIBLE when respecting the predictors.
    ## 21:39:43: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:43: Optimization finished (36961 calls to inner optimizer), rmspe: 
    ## 21:39:43: 0.594579182293917, mspe: 0.353524404017303.
    ## Final rmspe: 0.5945792, mspe (loss v): 0.3535244
    ## Optimal weights:
    ##     Korean Lithuanian     Polish 
    ##  0.2241930  0.4782916  0.2975154 
    ## 
    ## 21:39:43: Using Lithuanian as treated unit now.
    ## 21:39:44: Number of 'sunny' donors: 12 out of 36
    ## 21:39:44: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:44: with RMSPE 0.244538664565769 and MSPE (loss v) 
    ## 21:39:44: 0.0597991584676094 is INFEASIBLE when respecting the predictors.
    ## 21:39:44: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:44: Optimization finished (49021 calls to inner optimizer), rmspe: 
    ## 21:39:44: 0.519982639236863, mspe: 0.270381945107733.
    ## Final rmspe: 0.5199826, mspe (loss v): 0.2703819
    ## Optimal weights:
    ## Bulgarian Hungarian    Jewish   Latvian 
    ## 0.3581765 0.2779580 0.0182884 0.3455771 
    ## 
    ## 21:39:44: Using Mari as treated unit now.
    ## 21:39:44: Number of 'sunny' donors: 11 out of 36
    ## 21:39:44: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:44: with RMSPE 0.100134192932982 and MSPE (loss v) 
    ## 21:39:44: 0.0100268565943397 is INFEASIBLE when respecting the predictors.
    ## 21:39:44: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:44: checking v: v contains NAs!
    ## 21:39:44: Optimization finished (44161 calls to inner optimizer), rmspe: 
    ## 21:39:44: 0.245403333513263, mspe: 0.060222796099422.
    ## Final rmspe: 0.2454033, mspe (loss v): 0.0602228
    ## Optimal weights:
    ##  Belorussian      Chuvash       Korean        Tatar 
    ## 4.567723e-11 9.156734e-02 7.735433e-01 1.348894e-01 
    ## 
    ## 21:39:44: Using Moldovan as treated unit now.
    ## 21:39:45: No 'sunny' donors!
    ## Final rmspe: 0.7624376, mspe (loss v): 0.5813112
    ## Optimal weights:
    ##       Altai   Bulgarian   Hungarian      Kalmyk     Russian 
    ## 0.404541294 0.329114288 0.036863971 0.226949020 0.002531427 
    ## 
    ## 21:39:45: Using Mordvin as treated unit now.
    ## 21:39:45: No 'sunny' donors!
    ## Final rmspe: 1.1084, mspe (loss v): 1.228549
    ## Optimal weights:
    ##      Altai     Kazakh       Mari      Tatar 
    ## 0.25448663 0.22898439 0.43590329 0.08062569 
    ## 
    ## 21:39:45: Using Ossetian as treated unit now.
    ## 21:39:45: No 'sunny' donors!
    ## Final rmspe: 0.6744222, mspe (loss v): 0.4548454
    ## Optimal weights:
    ##       Altai Belorussian   Bulgarian  Lithuanian     Russian 
    ## 0.642245786 0.016875020 0.245496329 0.093634465 0.001748401 
    ## 
    ## 21:39:45: Using Polish as treated unit now.
    ## 21:39:45: Number of 'sunny' donors: 9 out of 36
    ## 21:39:45: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:45: with RMSPE 0.159800710978158 and MSPE (loss v) 
    ## 21:39:45: 0.0255362672291247 is INFEASIBLE when respecting the predictors.
    ## 21:39:45: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:45: Optimization finished (47641 calls to inner optimizer), rmspe: 
    ## 21:39:45: 0.448424353128206, mspe: 0.20108440047845.
    ## Final rmspe: 0.4484244, mspe (loss v): 0.2010844
    ## Optimal weights:
    ## Belorussian      Jewish     Latvian  Lithuanian 
    ##  0.59500214  0.19000428  0.12650053  0.08849306 
    ## 
    ## 21:39:45: Using Russian as treated unit now.
    ## 21:39:45: Number of 'sunny' donors: 13 out of 36
    ## 21:39:46: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:46: is FEASIBLE even when respecting the predictors.
    ## Final rmspe: 3.250135, mspe (loss v): 10.56338
    ## Optimal weights:
    ## Belorussian   Ukrainian 
    ##    0.498493    0.501507 
    ## 
    ## 21:39:46: Using Tatar as treated unit now.
    ## 21:39:46: Number of 'sunny' donors: 9 out of 36
    ## 21:39:46: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:46: with RMSPE 0.442175126264263 and MSPE (loss v) 0.195518842286817 
    ## 21:39:46: is INFEASIBLE when respecting the predictors.
    ## 21:39:46: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:46: Optimization finished (39121 calls to inner optimizer), rmspe: 
    ## 21:39:46: 0.951403090896143, mspe: 0.905167841366735.
    ## Final rmspe: 0.9514031, mspe (loss v): 0.9051678
    ## Optimal weights:
    ##       Kazakh         Mari      Russian 
    ## 7.029024e-01 2.970976e-01 1.430253e-08 
    ## 
    ## 21:39:46: Using Udmurt as treated unit now.
    ## 21:39:46: No 'sunny' donors!
    ## Final rmspe: 1.127716, mspe (loss v): 1.271744
    ## Optimal weights:
    ##      Altai     Kazakh       Mari      Tatar 
    ## 0.73945276 0.07030004 0.14408425 0.04616295 
    ## 
    ## 21:39:46: Using Ukrainian as treated unit now.
    ## 21:39:46: Number of 'sunny' donors: 6 out of 36
    ## 21:39:46: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:46: with RMSPE 0.341993881028781 and MSPE (loss v) 0.116959814661128 
    ## 21:39:46: is INFEASIBLE when respecting the predictors.
    ## 21:39:46: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:47: Optimization finished (37201 calls to inner optimizer), rmspe: 
    ## 21:39:47: 0.604664416476258, mspe: 0.365619056552573.
    ## Final rmspe: 0.6046644, mspe (loss v): 0.3656191
    ## Optimal weights:
    ## Belorussian   Bulgarian        Mari     Russian 
    ## 0.234843070 0.376623901 0.002381826 0.386151204 
    ## 
    ## 21:39:47: Using Uzbek as treated unit now.
    ## 21:39:47: Number of 'sunny' donors: 7 out of 36
    ## 21:39:47: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:47: with RMSPE 0.572506904317076 and MSPE (loss v) 0.327764155490722 
    ## 21:39:47: is INFEASIBLE when respecting the predictors.
    ## 21:39:47: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:47: Optimization finished (61261 calls to inner optimizer), rmspe: 
    ## 21:39:47: 1.17092420173847, mspe: 1.37106348621688.
    ## Final rmspe: 1.170924, mspe (loss v): 1.371063
    ## Optimal weights:
    ##     Georgian       Kazakh      Russian 
    ## 7.244441e-01 2.755558e-01 4.329512e-08 
    ## 
    ## 21:39:47: Using Yakut as treated unit now.
    ## 21:39:47: No 'sunny' donors!
    ## Final rmspe: 1.708231, mspe (loss v): 2.918053
    ## Optimal weights:
    ##      Altai   Japanese       Mari      Tatar 
    ## 0.67759235 0.01633436 0.27264493 0.03342835

``` r
sc_results_mean <- mscmt(data_prep_mscmt, treatment.identifier, controls.identifier, times.dep, times.pred, agg.fns, seed=2019)
```

    ## 21:39:49: Number of 'sunny' donors: 6 out of 37
    ## 21:39:49: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:39:49: with RMSPE 0.147834274985798 and MSPE (loss v) 
    ## 21:39:49: 0.0218549728605767 is INFEASIBLE when respecting the predictors.
    ## 21:39:49: Starting optimization via DEoptC, random seed 2019.
    ## 21:39:49: Optimization finished (48121 calls to inner optimizer), rmspe: 
    ## 21:39:49: 0.294405107674494, mspe: 0.0866743674248306.
    ## Final rmspe: 0.2944051, mspe (loss v): 0.08667437
    ## Optimal weights:
    ##    Korean    Polish     Tatar 
    ## 0.1384854 0.4114940 0.4500206

``` r
mean_all <-  min_by_year %>% 
  filter(YEAR < 1933) %>% 
  summarize_at(c("log_n_pred_full_imp_date", "pop_total", "urb_rate_pct", "clad_sim"), mean) %>% 
  gather(key = "variable", value = "all_groups")

ethnicity_controls %>% 
  summarize_all(mean)
```

    ## Warning in mean.default(ethnicity): argument is not numeric or logical:
    ## returning NA

    ## # A tibble: 1 x 9
    ##   ethnicity pop_total clad_sim urban_pop rural_pop urb_rate ind_country
    ##       <dbl>     <dbl>    <dbl>     <dbl>     <dbl>    <dbl>       <dbl>
    ## 1        NA  3681250.    0.763   668940.  3012310.    0.178       0.289
    ## # ... with 2 more variables: ethnicity_id <dbl>, urb_rate_pct <dbl>

``` r
opts_current$set(label = "sc_predictor_means")
sc_results$combined %>% 
  map(tk_tbl) %>% 
  bind_rows(.id = "variable") %>% 
  filter(variable %in% c(dep_var,"pop_total", "clad_sim", "urb_rate_pct")) %>% 
  filter(index < 1933) %>% 
  group_by(variable) %>% 
  summarize_all(mean) %>% 
  select(-c(index, gaps)) %>% 
  left_join(mean_all, by = "variable") %>% 
  mutate(variable = recode_factor(variable, "log_n_pred_full_imp_date" =
                                    "Log(1 + arrests)", 
                                  "pop_total" = "Total population",
                                  "clad_sim" = "Ling. similarity to Russian",
                                  "urb_rate_pct" = "Urbanization rate", .ordered = TRUE) )%>% 
  slice(c(2, 3, 4, 1)) %>% 
  kable("latex", booktabs = T, digits = 2, linesep = "", format.args = list(big.mark = " "),
        col.names = c("Variable", "Actual", "Synthetic", 
                      "Mean of all 38 ethnicities"),
        caption = "Pre-treatment Predictor Means") %>%
  add_header_above(c(" " = 1, "German minority" = 2)) %>% 
  footnote(general = "Log(1 + arrests) is averaged over the pre-treatment period (1921-1932). All other predictor are time-invariant. Total population and urbanization rate are taken from 1926 Soviet census.",
           threeparttable = T) %>% 
  kable_styling(latex_options = c("hold_position")) %>% 
  write_file(here::here("tables/sc_predictor_means.tex"))  

v_weights <-  as_tibble(sc_results_mean$v, rownames = "variable") %>% 
  mutate(variable = str_sub(variable, end = -16),
         variable =  ifelse(variable == "urb_rate", "urb_rate_pct", variable)) %>% 
  dplyr::select(variable, v_weight = max.order)


mean_all <- mean_all %>% 
  left_join(v_weights, by = "variable")
  

  
opts_current$set(label = "sc_predictor_means_robustness")
sc_results_mean$combined %>% 
  map(tk_tbl) %>% 
  bind_rows(.id = "variable") %>% 
  filter(variable %in% c(dep_var,"pop_total", "clad_sim", "urb_rate_pct")) %>% 
  filter(index < 1933) %>% 
  group_by(variable) %>% 
  summarize_all(mean) %>% 
  select(-c(index, gaps)) %>% 
  left_join(mean_all, by = "variable") %>% 
  mutate(variable = recode_factor(variable, "log_n_pred_full_imp_date" =
                                    "Log(1 + arrests)", 
                                  "pop_total" = "Total population",
                                  "clad_sim" = "Ling. similarity to Russian",
                                  "urb_rate_pct" = "Urbanization rate", .ordered = TRUE) )%>% 
  slice(c(2, 3, 4, 1)) %>% 
  kable("latex", booktabs = T, digits = c(2, 2, 2, 3), linesep = "", format.args = list(big.mark = " "),
        col.names = c("Variable", "Actual", "Synthetic", 
                      "Mean of all ethnicities", "$V$ weights"), 
        caption = "Pre-treatment Predictor Means", escape = TRUE) %>%
  add_header_above(c(" " = 1, "German minority" = 2)) %>% 
  footnote(general = "Log(1 + arrests) is averaged over the pre-treatment period (1921-1932). All other predictor are time-invariant. Total population and urbanization rate are taken from 1926 Soviet census.",
           threeparttable = T) %>% 
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>% 
  write_file(here::here("tables/sc_predictor_means_robustness.tex"))  
```

``` r
placebo_highlight_all(sc_placebo_pred_full_imp_date, "log_n_pred_full_imp_date")
```

    ## Warning: You set use_group_by = TRUE, but grouped calculation failed.
    ## Falling back to ungrouped filter operation...

    ## label_key: ethnicity

``` r
ggsave(here::here("plots/synthetic_control/ethnicity_imputation/annual/placebo_highlight_all_imp_date.pdf"))
```

    ## Saving 7 x 5 in image

``` r
ggsave(here::here("plots/for_presentation/placebo_highlight_all_imp_date.pdf"), scale = 0.7)
```

    ## Saving 4.9 x 3.5 in image

``` r
placebo_highlight_all(sc_placebo_pred_full_imp_date_mean, "log_n_pred_full_imp_date")
```

    ## Warning: You set use_group_by = TRUE, but grouped calculation failed.
    ## Falling back to ungrouped filter operation...

    ## label_key: ethnicity

``` r
ggsave(here::here("plots/synthetic_control/ethnicity_imputation/annual/placebo_highlight_all_imp_date_robustnes.pdf"))
```

    ## Saving 7 x 5 in image

``` r
ggsave(here::here("plots/for_presentation/placebo_highlight_all_imp_date_robustnes.pdf"), scale = 0.7)
```

    ## Saving 4.9 x 3.5 in image

``` r
placebo_highlight_mspe(sc_placebo_pred_full_imp_date, "log_n_pred_full_imp_date", exclusion_ratio = 20)
```

    ## Warning: You set use_group_by = TRUE, but grouped calculation failed.
    ## Falling back to ungrouped filter operation...

    ## label_key: ethnicity

``` r
ggsave(here::here("plots/synthetic_control/ethnicity_imputation/annual/placebo_highlight_mspe_20lower_imp_date.pdf"))
```

    ## Saving 7 x 5 in image

``` r
ggsave(here::here("plots/for_presentation/placebo_highlight_mspe_20lower_imp_date.pdf"), scale = 0.7)
```

    ## Saving 4.9 x 3.5 in image

``` r
placebo_highlight_mspe(sc_placebo_pred_full_imp_date_mean, "log_n_pred_full_imp_date", exclusion_ratio = 20)
```

    ## Warning: You set use_group_by = TRUE, but grouped calculation failed.
    ## Falling back to ungrouped filter operation...

    ## label_key: ethnicity

``` r
ggsave(here::here("plots/synthetic_control/ethnicity_imputation/annual/placebo_highlight_mspe_20lower_imp_date_robustnss.pdf"))
```

    ## Saving 7 x 5 in image

``` r
placebo_mspe_barplot(sc_placebo_pred_full_imp_date, "log_n_pred_full_imp_date", range_post = c("1933", "1960"))
```

``` r
ggsave(here::here("plots/synthetic_control/ethnicity_imputation/annual/mspe_ratios_imp_date.pdf"))
```

    ## Saving 7 x 5 in image

``` r
ggsave(here::here("plots/for_presentation/mspe_ratios_imp_date.pdf"), scale = 0.7)
```

    ## Saving 4.9 x 3.5 in image

``` r
placebo_mspe_barplot(sc_placebo_pred_full_imp_date, "log_n_pred_full_imp_date", range_post = c("1933", "1939"))
```

``` r
ggsave(here::here("plots/synthetic_control/ethnicity_imputation/annual/mspe_ratios_imp_date_until_1939.pdf"))
```

    ## Saving 7 x 5 in image

``` r
ggsave(here::here("plots/for_presentation/mspe_ratios_imp_date_until_1939.pdf"), scale = 0.7)
```

    ## Saving 4.9 x 3.5 in image

``` r
placebo_mspe_barplot(sc_placebo_pred_full_imp_date_mean, "log_n_pred_full_imp_date", range_post = c("1933", "1960"))
```

``` r
ggsave(here::here("plots/synthetic_control/ethnicity_imputation/annual/mspe_ratios_imp_date_robustness.pdf"))
```

    ## Saving 7 x 5 in image

``` r
placebo_mspe_barplot(sc_placebo_pred_full_imp_date_mean, "log_n_pred_full_imp_date", range_post = c("1933", "1939"))
```

``` r
ggsave(here::here("plots/synthetic_control/ethnicity_imputation/annual/mspe_ratios_imp_date_until_1939_robustness.pdf"))
```

    ## Saving 7 x 5 in image

``` r
timetk::tk_tbl(sc_results$combined[[dep_var]]) %>% 
  mutate(date = as_date(str_c(index,"-01-01"))) %>% 
  gather("type", dep_var, treated, synth, -date) %>% 
  mutate(type = fct_recode(type, "Synthetic" = "synth", "Actual" = "treated") %>% 
          fct_relevel("Actual")) %>% 
  ggplot(aes(x = date, y = dep_var, col = type)) + theme_minimal() +
  theme(axis.line = element_line(size = 1), 
        #panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=12)) + 
  #annotate("rect", fill = "grey", alpha = 0.4, 
  #      xmin = as_date("1921-01-01"), xmax = as_date("1933-01-01"),
  #      ymin = -Inf, ymax = Inf) + 
  geom_line(size = 1) +
  geom_vline(xintercept= ymd(19330101), col = "black", linetype = "dashed", size = 1)+
  scale_x_date(date_minor_breaks = "2 year", date_breaks = "4 year", date_labels = "%Y", 
               limits = c(as_date("1921-01-01"), NA)) +
  labs(x = element_blank(), y = "log(1 + arrests)", col = "German arrests") 
```

``` r
ggsave(here::here("plots/synthetic_control/ethnicity_imputation/annual/comparison_plot.pdf"))
```

    ## Saving 7 x 5 in image

``` r
timetk::tk_tbl(sc_results$combined[[dep_var]]) %>% 
  mutate(date = as_date(str_c(index,"-01-01"))) %>% 
  gather("type", dep_var, treated, synth, -date) %>% 
  mutate(type = fct_recode(type, "Synthetic" = "synth", "Actual" = "treated") %>% 
           fct_relevel("Actual"),
         dep_var = exp(dep_var) - 1) %>% 
  ggplot(aes(x = date, y = dep_var, col = type)) + theme_minimal() +
  theme(axis.line = element_line(size = 1), 
        #panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        text = element_text(size=12)) + 
  #annotate("rect", fill = "grey", alpha = 0.4, 
  #      xmin = as_date("1921-01-01"), xmax = as_date("1933-01-01"),
  #      ymin = -Inf, ymax = Inf) + 
  geom_line(size = 1) +
  geom_vline(xintercept= ymd(19330101), col = "black", linetype = "dashed", size = 1)+
  scale_x_date(date_minor_breaks = "2 year", date_breaks = "4 year", date_labels = "%Y", 
               limits = c(as_date("1921-01-01"), NA)) +
  labs(x = element_blank(), y = "Number of arrests", col = "German arrests") +
  scale_y_continuous(trans = "mynaturallog" , breaks = c(0,2,10,100,1000,10000, 100000), labels = comma)
```

``` r
ggsave(here::here("plots/synthetic_control/ethnicity_imputation/annual/comparison_plot_scaled.pdf"))
```

    ## Saving 7 x 5 in image

``` r
ggsave(here::here("plots/for_presentation/comparison_plot_scaled.pdf"), scale = 0.7)
```

    ## Saving 4.9 x 3.5 in image

``` r
opts_current$set(label = "sc_weights")
tibble(ethnicity = names(sc_results$w), Weights = sc_results$w) %>% 
  filter(Weights != 0) %>% 
  arrange(desc(Weights)) %>% 
  kable("latex", booktabs = T, digits = 2, linesep = "", format.args = list(big.mark = " "),
        col.names = c("Ethnic group", "$W$-Weight"), escape = FALSE,
        caption = "Synthetic German minority weights") %>%
 # footnote(general = "Ethnicity and date of arrest were imputed. Full matrix adjustment was applied on ethnic group imputations. All 38 ethnic groups are included.", threeparttable = T) %>% 
    write_file(here::here("tables/sc_weights.tex"))
 

opts_current$set(label = "sc_weights_robustness")
tibble(ethnicity = names(sc_results_mean$w), Weights = sc_results_mean$w) %>% 
  filter(Weights != 0) %>% 
  arrange(desc(Weights)) %>% 
  kable("latex", booktabs = T, digits = 2, linesep = "", format.args = list(big.mark = " "),
        col.names = c("Ethnic group", "$W$-Weight"), escape = FALSE,
        caption = "Synthetic German minority weights") %>%
 # footnote(general = "Ethnicity and date of arrest were imputed. Full matrix adjustment was applied on ethnic group imputations. All 38 ethnic groups are included.", threeparttable = T) %>% 
    write_file(here::here("tables/sc_weights_robustness.tex")) 
  
opts_current$set(label = "sc_weights_robustness")
make_w_weights_table <- function(sc_data, table_title = "Synthetic German minority weights",
                                 file_name = "tables/sc_weights_robustness.tex"){
tibble(ethnicity = names(sc_data$w), Weights = sc_data$w) %>% 
  filter(Weights != 0) %>% 
  arrange(desc(Weights)) %>% 
  kable("latex", booktabs = T, digits = 2, linesep = "", format.args = list(big.mark = " "),
        col.names = c("Ethnic group", "$W$-Weight"), escape = FALSE,
        caption =  table_title) %>%
 # footnote(general = "Ethnicity and date of arrest were imputed. Full matrix adjustment was applied on ethnic group imputations. All 38 ethnic groups are included.", threeparttable = T) %>% 
    write_file(here::here(file_name)) 
}
```

### Robustness checks

Only rehabilitated individuals

``` r
dep_var <- "log_n_pred_full_imp_date_rehab"
times.dep  <- cbind("log_n_pred_full_imp_date_rehab" = c("1921","1932"))
times.pred <- cbind("log_n_pred_full_imp_date"       = c("1921","1932"),
                    "pop_total"             = c("1921","1932"),
                    "clad_sim"              = c("1921","1932"),
                    "urb_rate"              = c("1921","1932"))
agg.fns <- rep("id", ncol(times.pred))


sc_placebo_rehabs <- mscmt(data_prep_mscmt, treatment.identifier, controls.identifier, times.dep, times.pred, agg.fns, seed=2019, placebo = TRUE, single.v=TRUE)
```

    ## 21:40:09: Starting placebo study, excluding original treated unit.
    ## 21:40:09: Using German as treated unit now.
    ## 21:40:10: Number of 'sunny' donors: 37 out of 37
    ## 21:40:10: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:40:10: with RMSPE 0.344768266094008 and MSPE (loss v) 0.118865157305469 
    ## 21:40:10: is INFEASIBLE when respecting the predictors.
    ## 21:40:10: Starting optimization via DEoptC, random seed 2019.
    ## 21:40:18: Optimization finished (46021 calls to inner optimizer), rmspe: 
    ## 21:40:18: 0.50673726064527, mspe: 0.256782651326273.
    ## Final rmspe: 0.5067373, mspe (loss v): 0.2567827
    ## Optimal weights:
    ##      Greek     Korean       Mari     Polish      Tatar 
    ## 0.03217460 0.27153141 0.05393079 0.32260847 0.31975473 
    ## 
    ## 21:40:18: Using Altai as treated unit now.
    ## 21:40:18: Number of 'sunny' donors: 36 out of 36
    ## 21:40:18: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:40:18: with RMSPE 0.333998818701652 and MSPE (loss v) 0.111555210894099 
    ## 21:40:18: is INFEASIBLE when respecting the predictors.
    ## 21:40:18: Starting optimization via DEoptC, random seed 2019.
    ## 21:40:22: Optimization finished (35761 calls to inner optimizer), rmspe: 
    ## 21:40:22: 0.683567459541627, mspe: 0.467264471744194.
    ## Final rmspe: 0.6835675, mspe (loss v): 0.4672645
    ## Optimal weights:
    ##    Balkar   Chechen    Kalmyk      Mari 
    ## 0.4081418 0.1640474 0.1138234 0.3139874 
    ## 
    ## 21:40:22: Using Armenian as treated unit now.
    ## 21:40:22: Number of 'sunny' donors: 36 out of 36
    ## 21:40:22: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:40:22: with RMSPE 0.35417443634604 and MSPE (loss v) 0.125439531361035 
    ## 21:40:22: is INFEASIBLE when respecting the predictors.
    ## 21:40:22: Starting optimization via DEoptC, random seed 2019.
    ## 21:40:26: Optimization finished (30361 calls to inner optimizer), rmspe: 
    ## 21:40:26: 0.564765908338354, mspe: 0.318960531221246.
    ## Final rmspe: 0.5647659, mspe (loss v): 0.3189605
    ## Optimal weights:
    ##   Hungarian     Chinese        Komi     Latvian  Lithuanian    Ossetian 
    ## 0.134969256 0.142549334 0.139320017 0.003017607 0.236805247 0.257567091 
    ##       Yakut 
    ## 0.085771448 
    ## 
    ## 21:40:26: Using Balkar as treated unit now.
    ## 21:40:26: Number of 'sunny' donors: 36 out of 36
    ## 21:40:26: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:40:26: with RMSPE 0.349440342198261 and MSPE (loss v) 0.122108552755638 
    ## 21:40:26: is INFEASIBLE when respecting the predictors.
    ## 21:40:26: Starting optimization via DEoptC, random seed 2019.
    ## 21:40:28: checking v: v contains NAs!
    ## 21:40:28: Optimization finished (28861 calls to inner optimizer), rmspe: 
    ## 21:40:28: 0.418597753803887, mspe: 0.17522407948966.
    ## Final rmspe: 0.4185978, mspe (loss v): 0.1752241
    ## Optimal weights:
    ##      Altai  Bulgarian     Kalmyk      Uzbek 
    ## 0.25631184 0.03344617 0.20697507 0.50326693 
    ## 
    ## 21:40:28: Using Bashkir as treated unit now.
    ## 21:40:28: Number of 'sunny' donors: 36 out of 36
    ## 21:40:28: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:40:28: with RMSPE 0.624190327100665 and MSPE (loss v) 0.389613564446035 
    ## 21:40:28: is INFEASIBLE when respecting the predictors.
    ## 21:40:28: Starting optimization via DEoptC, random seed 2019.
    ## 21:40:31: Optimization finished (42301 calls to inner optimizer), rmspe: 
    ## 21:40:31: 0.717294419012076, mspe: 0.514511283545871.
    ## Final rmspe: 0.7172944, mspe (loss v): 0.5145113
    ## Optimal weights:
    ##      Greek    Mordvin      Tatar     Udmurt 
    ## 0.25078264 0.24612943 0.03180498 0.47128295 
    ## 
    ## 21:40:31: Using Belorussian as treated unit now.
    ## 21:40:31: Number of 'sunny' donors: 36 out of 36
    ## 21:40:31: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:40:31: with RMSPE 0.540800482018329 and MSPE (loss v) 0.292465161351257 
    ## 21:40:31: is INFEASIBLE when respecting the predictors.
    ## 21:40:31: Starting optimization via DEoptC, random seed 2019.
    ## 21:40:35: Optimization finished (59161 calls to inner optimizer), rmspe: 
    ## 21:40:35: 0.71224351921596, mspe: 0.507290830665136.
    ## Final rmspe: 0.7122435, mspe (loss v): 0.5072908
    ## Optimal weights:
    ##    Chinese    Mordvin     Polish    Russian 
    ## 0.21011199 0.03300613 0.46642533 0.29045655 
    ## 
    ## 21:40:35: Using Bulgarian as treated unit now.
    ## 21:40:35: Number of 'sunny' donors: 36 out of 36
    ## 21:40:35: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:40:35: with RMSPE 0.379594613154469 and MSPE (loss v) 0.144092070335891 
    ## 21:40:35: is INFEASIBLE when respecting the predictors.
    ## 21:40:35: Starting optimization via DEoptC, random seed 2019.
    ## 21:40:40: Optimization finished (53281 calls to inner optimizer), rmspe: 
    ## 21:40:40: 0.46856584863275, mspe: 0.219553954504929.
    ## Final rmspe: 0.4685658, mspe (loss v): 0.219554
    ## Optimal weights:
    ##     Balkar    Finnish    Chuvash   Moldovan    Mordvin   Ossetian 
    ## 0.43118209 0.03491439 0.03469727 0.33122723 0.02010515 0.14787387 
    ## 
    ## 21:40:40: Using Buryat as treated unit now.
    ## 21:40:40: Number of 'sunny' donors: 36 out of 36
    ## 21:40:40: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:40:40: with RMSPE 0.426557046882312 and MSPE (loss v) 0.181950914244959 
    ## 21:40:40: is INFEASIBLE when respecting the predictors.
    ## 21:40:40: Starting optimization via DEoptC, random seed 2019.
    ## 21:40:43: Optimization finished (36001 calls to inner optimizer), rmspe: 
    ## 21:40:43: 0.758978547416605, mspe: 0.576048435438619.
    ## Final rmspe: 0.7589785, mspe (loss v): 0.5760484
    ## Optimal weights:
    ##       Altai      Balkar     Chuvash      Kalmyk    Karelian        Komi 
    ## 0.131139829 0.192719504 0.366127889 0.216937706 0.003701008 0.015948755 
    ##     Mordvin       Yakut 
    ## 0.045792608 0.027632702 
    ## 
    ## 21:40:43: Using Estonian as treated unit now.
    ## 21:40:43: Number of 'sunny' donors: 36 out of 36
    ## 21:40:43: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:40:43: with RMSPE 0.375997721480315 and MSPE (loss v) 0.141374286558389 
    ## 21:40:43: is INFEASIBLE when respecting the predictors.
    ## 21:40:43: Starting optimization via DEoptC, random seed 2019.
    ## 21:40:46: Optimization finished (30301 calls to inner optimizer), rmspe: 
    ## 21:40:46: 0.587854656546171, mspe: 0.345573097223017.
    ## Final rmspe: 0.5878547, mspe (loss v): 0.3455731
    ## Optimal weights:
    ## Belorussian    Georgian   Hungarian      Jewish    Karelian      Korean 
    ##  0.08861582  0.02683244  0.35297821  0.09106246  0.05867295  0.27893700 
    ##     Russian 
    ##  0.10290112 
    ## 
    ## 21:40:46: Using Finnish as treated unit now.
    ## 21:40:46: Number of 'sunny' donors: 36 out of 36
    ## 21:40:46: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:40:46: with RMSPE 0.252124305636167 and MSPE (loss v) 
    ## 21:40:46: 0.0635666654925195 is INFEASIBLE when respecting the predictors.
    ## 21:40:46: Starting optimization via DEoptC, random seed 2019.
    ## 21:40:49: Optimization finished (27241 calls to inner optimizer), rmspe: 
    ## 21:40:49: 0.555607682852629, mspe: 0.308699897244868.
    ## Final rmspe: 0.5556077, mspe (loss v): 0.3086999
    ## Optimal weights:
    ##   Bulgarian     Chechen    Japanese      Khakas        Komi     Latvian 
    ## 0.381192330 0.060864852 0.064862348 0.257840578 0.009947105 0.203780525 
    ##       Tatar 
    ## 0.021512261 
    ## 
    ## 21:40:49: Using Georgian as treated unit now.
    ## 21:40:49: Number of 'sunny' donors: 36 out of 36
    ## 21:40:49: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:40:49: with RMSPE 0.3742202276151 and MSPE (loss v) 0.140040778756297 
    ## 21:40:49: is INFEASIBLE when respecting the predictors.
    ## 21:40:49: Starting optimization via DEoptC, random seed 2019.
    ## 21:40:52: Optimization finished (37261 calls to inner optimizer), rmspe: 
    ## 21:40:52: 0.625727258389281, mspe: 0.391534601891366.
    ## Final rmspe: 0.6257273, mspe (loss v): 0.3915346
    ## Optimal weights:
    ##      Altai     Balkar    Chinese    Chuvash  Kabardian     Kalmyk 
    ## 0.07006948 0.05269979 0.22707825 0.06663252 0.12398097 0.17147705 
    ##     Korean   Moldovan 
    ## 0.03970131 0.24836062 
    ## 
    ## 21:40:52: Using Greek as treated unit now.
    ## 21:40:52: Number of 'sunny' donors: 36 out of 36
    ## 21:40:52: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:40:52: with RMSPE 0.435240207267607 and MSPE (loss v) 0.189434038022349 
    ## 21:40:52: is INFEASIBLE when respecting the predictors.
    ## 21:40:53: Starting optimization via DEoptC, random seed 2019.
    ## 21:40:57: checking v: v contains NAs!
    ## 21:40:57: Optimization finished (57901 calls to inner optimizer), rmspe: 
    ## 21:40:57: 0.517382734349165, mspe: 0.267684893802619.
    ## Final rmspe: 0.5173827, mspe (loss v): 0.2676849
    ## Optimal weights:
    ##     Khakas    Russian      Tatar  Ukrainian      Yakut 
    ## 0.70933999 0.06666015 0.15681935 0.05430229 0.01287822 
    ## 
    ## 21:40:57: Using Hungarian as treated unit now.
    ## 21:40:57: Number of 'sunny' donors: 36 out of 36
    ## 21:40:57: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:40:57: with RMSPE 0.633555203987449 and MSPE (loss v) 0.401392196499578 
    ## 21:40:57: is INFEASIBLE when respecting the predictors.
    ## 21:40:57: Starting optimization via DEoptC, random seed 2019.
    ## 21:40:59: Optimization finished (25981 calls to inner optimizer), rmspe: 
    ## 21:40:59: 0.748083909491235, mspe: 0.559629535639691.
    ## Final rmspe: 0.7480839, mspe (loss v): 0.5596295
    ## Optimal weights:
    ##    Chinese     Jewish     Kalmyk 
    ## 0.07860859 0.08548922 0.83590219 
    ## 
    ## 21:40:59: Using Chechen as treated unit now.
    ## 21:40:59: Number of 'sunny' donors: 36 out of 36
    ## 21:40:59: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:40:59: with RMSPE 0.397266301517907 and MSPE (loss v) 0.157820514321717 
    ## 21:40:59: is INFEASIBLE when respecting the predictors.
    ## 21:40:59: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:03: Optimization finished (47341 calls to inner optimizer), rmspe: 
    ## 21:41:03: 0.631037608088098, mspe: 0.398208462821548.
    ## Final rmspe: 0.6310376, mspe (loss v): 0.3982085
    ## Optimal weights:
    ##   Bashkir   Finnish    Kalmyk    Khakas  Ossetian    Udmurt 
    ## 0.1511948 0.1967785 0.1642394 0.1258499 0.1081963 0.2537412 
    ## 
    ## 21:41:03: Using Chinese as treated unit now.
    ## 21:41:04: Number of 'sunny' donors: 36 out of 36
    ## 21:41:04: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:04: with RMSPE 0.775509626793275 and MSPE (loss v) 0.601415181249044 
    ## 21:41:04: is INFEASIBLE when respecting the predictors.
    ## 21:41:04: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:07: Optimization finished (46561 calls to inner optimizer), rmspe: 
    ## 21:41:07: 0.83983617031038, mspe: 0.705324792961605.
    ## Final rmspe: 0.8398362, mspe (loss v): 0.7053248
    ## Optimal weights:
    ##   Armenian   Georgian  Hungarian  Kabardian   Karelian 
    ## 0.06256793 0.07146337 0.20833719 0.47116714 0.18646436 
    ## 
    ## 21:41:07: Using Chuvash as treated unit now.
    ## 21:41:07: Number of 'sunny' donors: 36 out of 36
    ## 21:41:07: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:07: with RMSPE 0.447640846870158 and MSPE (loss v) 0.200382327786632 
    ## 21:41:07: is INFEASIBLE when respecting the predictors.
    ## 21:41:07: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:10: checking v: v contains NAs!
    ## 21:41:10: Optimization finished (40381 calls to inner optimizer), rmspe: 
    ## 21:41:10: 0.844062882485098, mspe: 0.712442149589051.
    ## Final rmspe: 0.8440629, mspe (loss v): 0.7124421
    ## Optimal weights:
    ##     Buryat     Kazakh       Komi     Udmurt 
    ## 0.23344431 0.70120707 0.03955385 0.02579477 
    ## 
    ## 21:41:11: Using Japanese as treated unit now.
    ## 21:41:11: Number of 'sunny' donors: 36 out of 36
    ## 21:41:11: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:11: with RMSPE 0.277158490609131 and MSPE (loss v) 
    ## 21:41:11: 0.0768168289167315 is INFEASIBLE when respecting the predictors.
    ## 21:41:11: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:14: Optimization finished (44101 calls to inner optimizer), rmspe: 
    ## 21:41:14: 0.562041631522489, mspe: 0.315890795564461.
    ## Final rmspe: 0.5620416, mspe (loss v): 0.3158908
    ## Optimal weights:
    ##       Balkar    Bulgarian    Hungarian      Chinese       Khakas 
    ## 2.920900e-02 6.639391e-07 7.661241e-02 2.311385e-01 6.630394e-01 
    ## 
    ## 21:41:14: Using Jewish as treated unit now.
    ## 21:41:14: Number of 'sunny' donors: 36 out of 36
    ## 21:41:14: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:14: with RMSPE 0.592162011931867 and MSPE (loss v) 0.350655848375197 
    ## 21:41:14: is INFEASIBLE when respecting the predictors.
    ## 21:41:15: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:18: Optimization finished (57901 calls to inner optimizer), rmspe: 
    ## 21:41:18: 0.689060343995905, mspe: 0.474804157667754.
    ## Final rmspe: 0.6890603, mspe (loss v): 0.4748042
    ## Optimal weights:
    ##  Hungarian   Karelian    Russian 
    ## 0.44207859 0.07594633 0.48197508 
    ## 
    ## 21:41:18: Using Kabardian as treated unit now.
    ## 21:41:18: Number of 'sunny' donors: 36 out of 36
    ## 21:41:18: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:18: with RMSPE 0.616904877216925 and MSPE (loss v) 0.380571627534029 
    ## 21:41:18: is INFEASIBLE when respecting the predictors.
    ## 21:41:18: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:21: checking v: v contains NAs!
    ## 21:41:21: Optimization finished (31921 calls to inner optimizer), rmspe: 
    ## 21:41:21: 0.794984320491808, mspe: 0.632000069827822.
    ## Final rmspe: 0.7949843, mspe (loss v): 0.6320001
    ## Optimal weights:
    ##        Altai       Balkar      Bashkir       Kazakh     Ossetian 
    ## 3.260166e-01 2.318070e-01 8.575200e-02 1.594214e-01 4.455806e-06 
    ##       Udmurt        Uzbek        Yakut 
    ## 1.674799e-02 5.507546e-02 1.251752e-01 
    ## 
    ## 21:41:21: Using Kalmyk as treated unit now.
    ## 21:41:21: Number of 'sunny' donors: 36 out of 36
    ## 21:41:21: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:21: with RMSPE 0.388529833245097 and MSPE (loss v) 0.150955431321463 
    ## 21:41:21: is INFEASIBLE when respecting the predictors.
    ## 21:41:21: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:23: Optimization finished (37081 calls to inner optimizer), rmspe: 
    ## 21:41:23: 0.57543937625445, mspe: 0.331130475744111.
    ## Final rmspe: 0.5754394, mspe (loss v): 0.3311305
    ## Optimal weights:
    ##     Balkar  Hungarian   Moldovan 
    ## 0.09650665 0.29386741 0.60962594 
    ## 
    ## 21:41:23: Using Karelian as treated unit now.
    ## 21:41:23: Number of 'sunny' donors: 36 out of 36
    ## 21:41:23: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:23: with RMSPE 1.10429962281813 and MSPE (loss v) 1.21947765695626 
    ## 21:41:23: is INFEASIBLE when respecting the predictors.
    ## 21:41:23: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:27: Optimization finished (47401 calls to inner optimizer), rmspe: 
    ## 21:41:27: 1.94061523487588, mspe: 3.76598748983236.
    ## Final rmspe: 1.940615, mspe (loss v): 3.765987
    ## Optimal weights:
    ##  Estonian    Kalmyk      Komi 
    ## 0.2826674 0.2645289 0.4528036 
    ## 
    ## 21:41:27: Using Kazakh as treated unit now.
    ## 21:41:27: Number of 'sunny' donors: 36 out of 36
    ## 21:41:27: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:27: with RMSPE 0.303797588460247 and MSPE (loss v) 
    ## 21:41:27: 0.0922929747542618 is INFEASIBLE when respecting the predictors.
    ## 21:41:27: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:34: Optimization finished (95461 calls to inner optimizer), rmspe: 
    ## 21:41:34: 0.524777704291969, mspe: 0.275391638921949.
    ## Final rmspe: 0.5247777, mspe (loss v): 0.2753916
    ## Optimal weights:
    ##     Chuvash     Mordvin     Russian       Tatar       Uzbek 
    ## 0.150101270 0.189626303 0.000718312 0.474073786 0.185480328 
    ## 
    ## 21:41:34: Using Khakas as treated unit now.
    ## 21:41:35: Number of 'sunny' donors: 36 out of 36
    ## 21:41:35: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:35: with RMSPE 0.309806328938157 and MSPE (loss v) 
    ## 21:41:35: 0.0959799614501375 is INFEASIBLE when respecting the predictors.
    ## 21:41:35: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:38: Optimization finished (39061 calls to inner optimizer), rmspe: 
    ## 21:41:38: 0.358240983138767, mspe: 0.12833660200023.
    ## Final rmspe: 0.358241, mspe (loss v): 0.1283366
    ## Optimal weights:
    ##    Balkar     Greek  Japanese    Korean 
    ## 0.2111182 0.1872875 0.4892446 0.1123497 
    ## 
    ## 21:41:38: Using Komi as treated unit now.
    ## 21:41:38: Number of 'sunny' donors: 36 out of 36
    ## 21:41:38: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:38: with RMSPE 1.04935488110962 and MSPE (loss v) 1.10114566650859 
    ## 21:41:38: is INFEASIBLE when respecting the predictors.
    ## 21:41:38: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:42: Optimization finished (35941 calls to inner optimizer), rmspe: 
    ## 21:41:42: 2.52055702023974, mspe: 6.35320769227986.
    ## Final rmspe: 2.520557, mspe (loss v): 6.353208
    ## Optimal weights:
    ##  Bulgarian    Chuvash   Karelian       Mari   Moldovan      Yakut 
    ## 0.08608674 0.14565029 0.43405143 0.08676428 0.01852737 0.22891990 
    ## 
    ## 21:41:42: Using Korean as treated unit now.
    ## 21:41:42: Number of 'sunny' donors: 36 out of 36
    ## 21:41:42: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:42: with RMSPE 0.174088111904992 and MSPE (loss v) 0.030306670706645 
    ## 21:41:42: is INFEASIBLE when respecting the predictors.
    ## 21:41:42: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:46: Optimization finished (41461 calls to inner optimizer), rmspe: 
    ## 21:41:46: 0.523412233340938, mspe: 0.273960366010949.
    ## Final rmspe: 0.5234122, mspe (loss v): 0.2739604
    ## Optimal weights:
    ##   Estonian      Greek     Khakas       Mari 
    ## 0.44868668 0.11956117 0.06915228 0.36259987 
    ## 
    ## 21:41:46: Using Latvian as treated unit now.
    ## 21:41:46: Number of 'sunny' donors: 36 out of 36
    ## 21:41:46: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:46: with RMSPE 0.40037897352328 and MSPE (loss v) 0.160303322439555 
    ## 21:41:46: is INFEASIBLE when respecting the predictors.
    ## 21:41:46: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:51: Optimization finished (48241 calls to inner optimizer), rmspe: 
    ## 21:41:51: 0.560141119426413, mspe: 0.313758073672275.
    ## Final rmspe: 0.5601411, mspe (loss v): 0.3137581
    ## Optimal weights:
    ##   Armenian  Hungarian    Chinese     Jewish   Karelian Lithuanian 
    ## 0.11754609 0.18132834 0.02260302 0.01294599 0.01268297 0.08917283 
    ##     Polish 
    ## 0.56372075 
    ## 
    ## 21:41:51: Using Lithuanian as treated unit now.
    ## 21:41:51: Number of 'sunny' donors: 36 out of 36
    ## 21:41:51: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:51: with RMSPE 0.235951344840838 and MSPE (loss v) 0.0556730371322 
    ## 21:41:51: is INFEASIBLE when respecting the predictors.
    ## 21:41:51: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:56: Optimization finished (53761 calls to inner optimizer), rmspe: 
    ## 21:41:56: 0.319407624507443, mspe: 0.102021230593488.
    ## Final rmspe: 0.3194076, mspe (loss v): 0.1020212
    ## Optimal weights:
    ##      Greek    Chinese     Kalmyk   Karelian    Latvian   Ossetian 
    ## 0.08845991 0.11463414 0.33371692 0.04246745 0.17171656 0.07819538 
    ##      Yakut 
    ## 0.17080964 
    ## 
    ## 21:41:56: Using Mari as treated unit now.
    ## 21:41:56: Number of 'sunny' donors: 36 out of 36
    ## 21:41:56: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:56: with RMSPE 0.739023871671138 and MSPE (loss v) 0.546156282899799 
    ## 21:41:56: is INFEASIBLE when respecting the predictors.
    ## 21:41:56: Starting optimization via DEoptC, random seed 2019.
    ## 21:41:59: Optimization finished (37681 calls to inner optimizer), rmspe: 
    ## 21:41:59: 0.819727407019677, mspe: 0.671953021819204.
    ## Final rmspe: 0.8197274, mspe (loss v): 0.671953
    ## Optimal weights:
    ##     Altai   Chuvash    Kazakh 
    ## 0.5195358 0.3312054 0.1492588 
    ## 
    ## 21:41:59: Using Moldovan as treated unit now.
    ## 21:41:59: Number of 'sunny' donors: 36 out of 36
    ## 21:41:59: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:41:59: with RMSPE 0.383095904236146 and MSPE (loss v) 0.14676247184251 
    ## 21:41:59: is INFEASIBLE when respecting the predictors.
    ## 21:41:59: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:05: Optimization finished (57181 calls to inner optimizer), rmspe: 
    ## 21:42:05: 0.906292268595506, mspe: 0.821365676115989.
    ## Final rmspe: 0.9062923, mspe (loss v): 0.8213657
    ## Optimal weights:
    ##  Bulgarian   Georgian     Kalmyk       Komi     Korean     Udmurt 
    ## 0.18970434 0.16299338 0.38828062 0.07716916 0.02483371 0.10852394 
    ##      Yakut 
    ## 0.04849485 
    ## 
    ## 21:42:05: Using Mordvin as treated unit now.
    ## 21:42:05: Number of 'sunny' donors: 36 out of 36
    ## 21:42:05: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:05: with RMSPE 0.858071385393742 and MSPE (loss v) 0.736286502431536 
    ## 21:42:05: is INFEASIBLE when respecting the predictors.
    ## 21:42:06: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:08: Optimization finished (28561 calls to inner optimizer), rmspe: 
    ## 21:42:08: 0.98156238346982, mspe: 0.963464712642954.
    ## Final rmspe: 0.9815624, mspe (loss v): 0.9634647
    ## Optimal weights:
    ##      Bashkir      Chuvash       Kazakh       Khakas      Russian 
    ## 5.611022e-01 8.104250e-02 9.149717e-02 1.012201e-01 6.428530e-07 
    ##        Tatar 
    ## 1.651374e-01 
    ## 
    ## 21:42:08: Using Ossetian as treated unit now.
    ## 21:42:08: Number of 'sunny' donors: 36 out of 36
    ## 21:42:08: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:08: with RMSPE 0.289711521624117 and MSPE (loss v) 
    ## 21:42:08: 0.0839327657617613 is INFEASIBLE when respecting the predictors.
    ## 21:42:08: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:12: Optimization finished (41641 calls to inner optimizer), rmspe: 
    ## 21:42:12: 1.52577429903279, mspe: 2.32798721158901.
    ## Final rmspe: 1.525774, mspe (loss v): 2.327987
    ## Optimal weights:
    ##    Armenian      Balkar   Bulgarian     Chechen   Kabardian       Uzbek 
    ## 0.293570576 0.040529421 0.235475657 0.032184316 0.337352513 0.009928402 
    ##       Yakut 
    ## 0.050959114 
    ## 
    ## 21:42:12: Using Polish as treated unit now.
    ## 21:42:12: Number of 'sunny' donors: 36 out of 36
    ## 21:42:12: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:12: with RMSPE 0.278592959017847 and MSPE (loss v) 
    ## 21:42:12: 0.0776140368143197 is INFEASIBLE when respecting the predictors.
    ## 21:42:13: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:17: Optimization finished (50401 calls to inner optimizer), rmspe: 
    ## 21:42:17: 0.314188577701209, mspe: 0.0987144623579089.
    ## Final rmspe: 0.3141886, mspe (loss v): 0.09871446
    ## Optimal weights:
    ## Belorussian       Greek      Jewish    Karelian   Ukrainian       Yakut 
    ##  0.45883614  0.25137182  0.21958038  0.01564668  0.03340034  0.02116465 
    ## 
    ## 21:42:17: Using Russian as treated unit now.
    ## 21:42:17: Number of 'sunny' donors: 36 out of 36
    ## 21:42:17: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:17: with RMSPE 2.54002764100656 and MSPE (loss v) 6.45174041707734 
    ## 21:42:17: is INFEASIBLE when respecting the predictors.
    ## 21:42:17: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:18: Optimization finished (24961 calls to inner optimizer), rmspe: 
    ## 21:42:18: 2.91689404735359, mspe: 8.5082708834868.
    ## Final rmspe: 2.916894, mspe (loss v): 8.508271
    ## Optimal weights:
    ## Belorussian   Ukrainian 
    ##   0.4984924   0.5015076 
    ## 
    ## 21:42:18: Using Tatar as treated unit now.
    ## 21:42:18: Number of 'sunny' donors: 36 out of 36
    ## 21:42:18: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:18: with RMSPE 0.705920443233042 and MSPE (loss v) 0.498323672174335 
    ## 21:42:18: is INFEASIBLE when respecting the predictors.
    ## 21:42:18: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:22: checking v: v contains NAs!
    ## 21:42:22: Optimization finished (59281 calls to inner optimizer), rmspe: 
    ## 21:42:22: 0.711345515891849, mspe: 0.50601244297944.
    ## Final rmspe: 0.7113455, mspe (loss v): 0.5060124
    ## Optimal weights:
    ##    Kazakh   Russian 
    ## 0.8577537 0.1422463 
    ## 
    ## 21:42:22: Using Udmurt as treated unit now.
    ## 21:42:22: Number of 'sunny' donors: 36 out of 36
    ## 21:42:22: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:22: with RMSPE 0.527006209258271 and MSPE (loss v) 0.277735544596773 
    ## 21:42:22: is INFEASIBLE when respecting the predictors.
    ## 21:42:22: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:26: Optimization finished (44041 calls to inner optimizer), rmspe: 
    ## 21:42:26: 0.689845884985891, mspe: 0.475887345031966.
    ## Final rmspe: 0.6898459, mspe (loss v): 0.4758873
    ## Optimal weights:
    ##        Altai      Bashkir      Chechen      Chinese      Chuvash 
    ## 8.206900e-02 5.513472e-01 1.875278e-01 7.726747e-02 5.643181e-02 
    ##     Moldovan        Uzbek 
    ## 2.305708e-06 4.535433e-02 
    ## 
    ## 21:42:26: Using Ukrainian as treated unit now.
    ## 21:42:26: Number of 'sunny' donors: 36 out of 36
    ## 21:42:26: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:26: with RMSPE 0.219839087171743 and MSPE (loss v) 
    ## 21:42:26: 0.0483292242485051 is INFEASIBLE when respecting the predictors.
    ## 21:42:26: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:29: Optimization finished (39001 calls to inner optimizer), rmspe: 
    ## 21:42:29: 0.317615944427688, mspe: 0.100879888154692.
    ## Final rmspe: 0.3176159, mspe (loss v): 0.1008799
    ## Optimal weights:
    ##   Chechen      Komi   Mordvin   Russian     Tatar 
    ## 0.2276201 0.1238946 0.0818575 0.3175175 0.2491103 
    ## 
    ## 21:42:29: Using Uzbek as treated unit now.
    ## 21:42:30: Number of 'sunny' donors: 36 out of 36
    ## 21:42:30: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:30: with RMSPE 0.413640681568314 and MSPE (loss v) 0.171098613448299 
    ## 21:42:30: is INFEASIBLE when respecting the predictors.
    ## 21:42:30: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:34: Optimization finished (62101 calls to inner optimizer), rmspe: 
    ## 21:42:34: 0.487603509344964, mspe: 0.237757182325525.
    ## Final rmspe: 0.4876035, mspe (loss v): 0.2377572
    ## Optimal weights:
    ##     Balkar     Kazakh    Russian     Udmurt 
    ## 0.71925164 0.14667859 0.02398998 0.11007979 
    ## 
    ## 21:42:34: Using Yakut as treated unit now.
    ## 21:42:34: Number of 'sunny' donors: 36 out of 36
    ## 21:42:34: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:34: with RMSPE 0.84905559642025 and MSPE (loss v) 0.720895405812547 
    ## 21:42:34: is INFEASIBLE when respecting the predictors.
    ## 21:42:35: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:38: Optimization finished (38461 calls to inner optimizer), rmspe: 
    ## 21:42:38: 1.39631779476584, mspe: 1.94970338397974.
    ## Final rmspe: 1.396318, mspe (loss v): 1.949703
    ## Optimal weights:
    ##   Armenian       Komi    Russian 
    ## 0.81020652 0.07132729 0.11846619

``` r
sc_results_rehabs <- mscmt(data_prep_mscmt, treatment.identifier, controls.identifier,
                                                  times.dep, times.pred, agg.fns, seed=2019,  single.v=TRUE)
```

    ## 21:42:40: Number of 'sunny' donors: 37 out of 37
    ## 21:42:40: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:40: with RMSPE 0.344768266094008 and MSPE (loss v) 0.118865157305469 
    ## 21:42:40: is INFEASIBLE when respecting the predictors.
    ## 21:42:40: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:44: Optimization finished (46021 calls to inner optimizer), rmspe: 
    ## 21:42:44: 0.50673726064527, mspe: 0.256782651326273.
    ## Final rmspe: 0.5067373, mspe (loss v): 0.2567827
    ## Optimal weights:
    ##      Greek     Korean       Mari     Polish      Tatar 
    ## 0.03217460 0.27153141 0.05393079 0.32260847 0.31975473

``` r
placebo_highlight_all(sc_placebo_rehabs, "log_n_pred_full_imp_date_rehab")
```

    ## Warning: You set use_group_by = TRUE, but grouped calculation failed.
    ## Falling back to ungrouped filter operation...

    ## label_key: ethnicity

``` r
ggsave(here::here("plots/final/placebo_highlight_all_rehabs.pdf"))
```

    ## Saving 7 x 5 in image

``` r
ggsave(here::here("plots/for_presentation/placebo_highlight_all_rehabs.pdf"), scale = 0.7)
```

    ## Saving 4.9 x 3.5 in image

``` r
opts_current$set(label = "sc_weights_rehabs")
make_w_weights_table(sc_data = sc_results_rehabs, 
                     table_title = "Synthetic German minority weights, Only rehabilitated individuals",
                                 file_name = "tables/sc_weights_rehabs.tex")

placebo_mspe_barplot(sc_placebo_rehabs, 
                     "log_n_pred_full_imp_date_rehab", range_post = c("1933", "1960"))
```

``` r
ggsave(here::here("plots/final/mspe_ratios_rehabs.pdf"))
```

    ## Saving 7 x 5 in image

``` r
placebo_mspe_barplot(sc_placebo_rehabs, 
                     "log_n_pred_full_imp_date_rehab", range_post = c("1933", "1939"))
```

``` r
ggsave(here::here("plots/final/mspe_ratios_rehabs_until_1939.pdf"))
```

    ## Saving 7 x 5 in image

Only ethnicites without independent state

``` r
ethnicities_without_ind_state <- ethnicity_controls %>% 
  filter(ind_country != 1) %>% 
  pull(ethnicity)
  
controls.identifier  <- setdiff(ethnicities_without_ind_state,
                                 treatment.identifier)
dep_var <- "log_n_pred_full_imp_date"
times.dep  <- cbind("log_n_pred_full_imp_date" = c("1921","1932"))

sc_placebo_ethnicities_without_ind_state <- mscmt(data_prep_mscmt, treatment.identifier, controls.identifier, times.dep, times.pred, agg.fns, seed=2019, placebo = TRUE, single.v=TRUE)
```

    ## 21:42:48: Starting placebo study, excluding original treated unit.
    ## 21:42:48: Using German as treated unit now.
    ## 21:42:48: Number of 'sunny' donors: 27 out of 27
    ## 21:42:49: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:49: with RMSPE 0.292445945065624 and MSPE (loss v) 
    ## 21:42:49: 0.0855246307853259 is INFEASIBLE when respecting the predictors.
    ## 21:42:49: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:52: Optimization finished (37441 calls to inner optimizer), rmspe: 
    ## 21:42:52: 0.29244594506564, mspe: 0.0855246307853352.
    ## Final rmspe: 0.2924459, mspe (loss v): 0.08552463
    ## Optimal weights:
    ##     Jewish     Korean      Tatar  Ukrainian    Chuvash     Khakas 
    ## 0.18988509 0.13010902 0.53021288 0.11133226 0.01307907 0.02538168 
    ## 
    ## 21:42:52: Using Armenian as treated unit now.
    ## 21:42:52: Number of 'sunny' donors: 26 out of 26
    ## 21:42:52: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:52: with RMSPE 0.348699794386694 and MSPE (loss v) 0.121591546605323 
    ## 21:42:52: is INFEASIBLE when respecting the predictors.
    ## 21:42:52: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:54: Optimization finished (28921 calls to inner optimizer), rmspe: 
    ## 21:42:54: 0.348699794386795, mspe: 0.121591546605393.
    ## Final rmspe: 0.3486998, mspe (loss v): 0.1215915
    ## Optimal weights:
    ##     Jewish     Kalmyk   Ossetian   Karelian       Komi      Yakut 
    ## 0.05703199 0.23132750 0.42537110 0.02762858 0.04681199 0.21182884 
    ## 
    ## 21:42:54: Using Belorussian as treated unit now.
    ## 21:42:54: Number of 'sunny' donors: 26 out of 26
    ## 21:42:54: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:54: is FEASIBLE even when respecting the predictors.
    ## Final rmspe: 0.6433718, mspe (loss v): 0.4139272
    ## Optimal weights:
    ##       Jewish        Tatar    Ukrainian         Mari 
    ## 3.815475e-01 3.914895e-01 2.269630e-01 7.008991e-14 
    ## 
    ## 21:42:54: Using Chechen as treated unit now.
    ## 21:42:54: Number of 'sunny' donors: 26 out of 26
    ## 21:42:55: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:55: with RMSPE 0.384180448112309 and MSPE (loss v) 0.147594616711775 
    ## 21:42:55: is INFEASIBLE when respecting the predictors.
    ## 21:42:55: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:56: Optimization finished (29821 calls to inner optimizer), rmspe: 
    ## 21:42:56: 0.384180448112311, mspe: 0.147594616711776.
    ## Final rmspe: 0.3841804, mspe (loss v): 0.1475946
    ## Optimal weights:
    ##     Kalmyk   Ossetian    Bashkir  Bulgarian     Udmurt 
    ## 0.22951513 0.19209421 0.46485671 0.02856198 0.08497197 
    ## 
    ## 21:42:56: Using Jewish as treated unit now.
    ## 21:42:56: Number of 'sunny' donors: 26 out of 26
    ## 21:42:56: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:56: with RMSPE 1.30709954975177 and MSPE (loss v) 1.70850923296128 
    ## 21:42:56: is INFEASIBLE when respecting the predictors.
    ## 21:42:56: Starting optimization via DEoptC, random seed 2019.
    ## 21:42:58: Optimization finished (29821 calls to inner optimizer), rmspe: 
    ## 21:42:58: 1.30709954975179, mspe: 1.70850923296133.
    ## Final rmspe: 1.3071, mspe (loss v): 1.708509
    ## Optimal weights:
    ## Belorussian      Korean    Karelian 
    ##   0.5592102   0.2345799   0.2062099 
    ## 
    ## 21:42:58: Using Kabardian as treated unit now.
    ## 21:42:58: Number of 'sunny' donors: 26 out of 26
    ## 21:42:58: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:42:58: with RMSPE 0.508795998019183 and MSPE (loss v) 0.258873367600336 
    ## 21:42:58: is INFEASIBLE when respecting the predictors.
    ## 21:42:58: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:00: Optimization finished (30601 calls to inner optimizer), rmspe: 
    ## 21:43:00: 0.508795998019184, mspe: 0.258873367600338.
    ## Final rmspe: 0.508796, mspe (loss v): 0.2588734
    ## Optimal weights:
    ##   Ossetian      Tatar      Altai    Bashkir      Uzbek 
    ## 0.36263050 0.12619623 0.33967514 0.03019007 0.14130806 
    ## 
    ## 21:43:00: Using Kalmyk as treated unit now.
    ## 21:43:00: Number of 'sunny' donors: 26 out of 26
    ## 21:43:00: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:00: is FEASIBLE even when respecting the predictors.
    ## Final rmspe: 1.533282, mspe (loss v): 2.350954
    ## Optimal weights:
    ## Moldovan 
    ##        1 
    ## 
    ## 21:43:00: Using Korean as treated unit now.
    ## 21:43:00: Number of 'sunny' donors: 26 out of 26
    ## 21:43:00: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:00: with RMSPE 0.303305405312354 and MSPE (loss v) 
    ## 21:43:00: 0.0919941688916911 is INFEASIBLE when respecting the predictors.
    ## 21:43:00: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:02: Optimization finished (29881 calls to inner optimizer), rmspe: 
    ## 21:43:02: 0.303305405312357, mspe: 0.0919941688916933.
    ## Final rmspe: 0.3033054, mspe (loss v): 0.09199417
    ## Optimal weights:
    ##     Jewish      Altai       Mari 
    ## 0.16391445 0.08990843 0.74617711 
    ## 
    ## 21:43:02: Using Ossetian as treated unit now.
    ## 21:43:02: Number of 'sunny' donors: 26 out of 26
    ## 21:43:02: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:02: with RMSPE 0.285054851626993 and MSPE (loss v) 
    ## 21:43:02: 0.0812562684360867 is INFEASIBLE when respecting the predictors.
    ## 21:43:02: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:04: Optimization finished (30181 calls to inner optimizer), rmspe: 
    ## 21:43:04: 0.28505485162703, mspe: 0.0812562684361082.
    ## Final rmspe: 0.2850549, mspe (loss v): 0.08125627
    ## Optimal weights:
    ##     Armenian      Chechen    Kabardian       Balkar    Bulgarian 
    ## 3.549921e-01 5.096318e-02 2.803543e-01 1.368658e-01 3.974459e-02 
    ##      Chuvash       Udmurt        Uzbek        Yakut 
    ## 6.580914e-05 1.034844e-02 7.127507e-02 5.539065e-02 
    ## 
    ## 21:43:04: Using Tatar as treated unit now.
    ## 21:43:04: Number of 'sunny' donors: 26 out of 26
    ## 21:43:04: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:04: is FEASIBLE even when respecting the predictors.
    ## Final rmspe: 0.5363151, mspe (loss v): 0.2876339
    ## Optimal weights:
    ## Ukrainian    Kazakh 
    ## 0.6220586 0.3779414 
    ## 
    ## 21:43:04: Using Ukrainian as treated unit now.
    ## 21:43:04: Number of 'sunny' donors: 26 out of 26
    ## 21:43:04: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:04: with RMSPE 0.49924462168491 and MSPE (loss v) 0.249245192281309 
    ## 21:43:04: is INFEASIBLE when respecting the predictors.
    ## 21:43:04: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:05: Optimization finished (25381 calls to inner optimizer), rmspe: 
    ## 21:43:05: 0.499244621686118, mspe: 0.249245192282515.
    ## Final rmspe: 0.4992446, mspe (loss v): 0.2492452
    ## Optimal weights:
    ## Belorussian      Jewish       Tatar 
    ##   0.1366668   0.1354290   0.7279041 
    ## 
    ## 21:43:05: Using Altai as treated unit now.
    ## 21:43:05: Number of 'sunny' donors: 26 out of 26
    ## 21:43:05: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:05: with RMSPE 0.454608443653105 and MSPE (loss v) 0.206668837040698 
    ## 21:43:05: is INFEASIBLE when respecting the predictors.
    ## 21:43:05: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:07: checking v: v contains NAs!
    ## 21:43:07: Optimization finished (25981 calls to inner optimizer), rmspe: 
    ## 21:43:07: 0.454608443653106, mspe: 0.206668837040699.
    ## Final rmspe: 0.4546084, mspe (loss v): 0.2066688
    ## Optimal weights:
    ##  Kabardian     Kalmyk     Korean     Balkar   Georgian     Udmurt 
    ## 0.15974098 0.08101252 0.18635469 0.37492960 0.11380853 0.08415369 
    ## 
    ## 21:43:07: Using Balkar as treated unit now.
    ## 21:43:07: Number of 'sunny' donors: 26 out of 26
    ## 21:43:07: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:07: with RMSPE 0.422238773128067 and MSPE (loss v) 0.178285581532695 
    ## 21:43:07: is INFEASIBLE when respecting the predictors.
    ## 21:43:07: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:09: Optimization finished (31141 calls to inner optimizer), rmspe: 
    ## 21:43:09: 0.422238773128074, mspe: 0.178285581532701.
    ## Final rmspe: 0.4222388, mspe (loss v): 0.1782856
    ## Optimal weights:
    ##    Kalmyk     Altai Bulgarian     Uzbek 
    ## 0.1750580 0.2419899 0.1524354 0.4305167 
    ## 
    ## 21:43:09: Using Bashkir as treated unit now.
    ## 21:43:09: Number of 'sunny' donors: 26 out of 26
    ## 21:43:09: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:09: with RMSPE 0.374130333457015 and MSPE (loss v) 0.139973506412657 
    ## 21:43:09: is INFEASIBLE when respecting the predictors.
    ## 21:43:09: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:10: Optimization finished (27121 calls to inner optimizer), rmspe: 
    ## 21:43:10: 0.374130333457035, mspe: 0.139973506412672.
    ## Final rmspe: 0.3741303, mspe (loss v): 0.1399735
    ## Optimal weights:
    ##    Chechen      Tatar  Ukrainian     Khakas    Mordvin     Udmurt 
    ## 0.02492198 0.12195927 0.03421849 0.05874161 0.28650039 0.47365827 
    ## 
    ## 21:43:10: Using Bulgarian as treated unit now.
    ## 21:43:10: Number of 'sunny' donors: 26 out of 26
    ## 21:43:10: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:10: with RMSPE 0.437590166915484 and MSPE (loss v) 0.191485154181121 
    ## 21:43:10: is INFEASIBLE when respecting the predictors.
    ## 21:43:10: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:12: Optimization finished (28561 calls to inner optimizer), rmspe: 
    ## 21:43:12: 0.437590166915561, mspe: 0.191485154181188.
    ## Final rmspe: 0.4375902, mspe (loss v): 0.1914852
    ## Optimal weights:
    ##    Chechen   Ossetian     Balkar    Chuvash       Komi   Moldovan 
    ## 0.01599819 0.02990436 0.44204543 0.04441858 0.03597987 0.31682176 
    ##      Uzbek 
    ## 0.11483181 
    ## 
    ## 21:43:12: Using Buryat as treated unit now.
    ## 21:43:12: Number of 'sunny' donors: 26 out of 26
    ## 21:43:12: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:12: with RMSPE 0.22877474859725 and MSPE (loss v) 0.052337885595735 
    ## 21:43:12: is INFEASIBLE when respecting the predictors.
    ## 21:43:12: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:14: checking v: v contains NAs!
    ## 21:43:14: Optimization finished (28801 calls to inner optimizer), rmspe: 
    ## 21:43:14: 0.228774748597256, mspe: 0.0523378855957378.
    ## Final rmspe: 0.2287747, mspe (loss v): 0.05233789
    ## Optimal weights:
    ## Belorussian      Kalmyk       Altai      Balkar     Chuvash    Karelian 
    ## 0.024427737 0.302123301 0.024656947 0.006985831 0.354601904 0.008627624 
    ##     Mordvin       Uzbek       Yakut 
    ## 0.082263205 0.192307440 0.004006009 
    ## 
    ## 21:43:14: Using Georgian as treated unit now.
    ## 21:43:14: Number of 'sunny' donors: 26 out of 26
    ## 21:43:14: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:14: with RMSPE 0.421323185124841 and MSPE (loss v) 0.177513226323741 
    ## 21:43:14: is INFEASIBLE when respecting the predictors.
    ## 21:43:14: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:16: Optimization finished (36061 calls to inner optimizer), rmspe: 
    ## 21:43:16: 0.421323185124867, mspe: 0.177513226323763.
    ## Final rmspe: 0.4213232, mspe (loss v): 0.1775132
    ## Optimal weights:
    ##      Jewish   Kabardian      Kalmyk       Altai     Chuvash    Moldovan 
    ## 0.006935126 0.179026484 0.289186857 0.243715194 0.112806597 0.116148310 
    ##       Yakut 
    ## 0.052181430 
    ## 
    ## 21:43:16: Using Chuvash as treated unit now.
    ## 21:43:17: Number of 'sunny' donors: 26 out of 26
    ## 21:43:17: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:17: with RMSPE 0.89048520515359 and MSPE (loss v) 0.792963900597431 
    ## 21:43:17: is INFEASIBLE when respecting the predictors.
    ## 21:43:17: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:18: Optimization finished (26341 calls to inner optimizer), rmspe: 
    ## 21:43:18: 0.89048520515359, mspe: 0.792963900597431.
    ## Final rmspe: 0.8904852, mspe (loss v): 0.7929639
    ## Optimal weights:
    ##       Tatar      Buryat    Karelian        Komi 
    ## 0.543318202 0.412063345 0.002624655 0.041993798 
    ## 
    ## 21:43:18: Using Karelian as treated unit now.
    ## 21:43:18: Number of 'sunny' donors: 26 out of 26
    ## 21:43:18: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:18: with RMSPE 1.17927472955291 and MSPE (loss v) 1.39068888776209 
    ## 21:43:18: is INFEASIBLE when respecting the predictors.
    ## 21:43:18: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:19: Optimization finished (26761 calls to inner optimizer), rmspe: 
    ## 21:43:19: 1.17927472955291, mspe: 1.39068888776209.
    ## Final rmspe: 1.179275, mspe (loss v): 1.390689
    ## Optimal weights:
    ##    Jewish    Kalmyk      Komi 
    ## 0.1459100 0.2672769 0.5868131 
    ## 
    ## 21:43:19: Using Kazakh as treated unit now.
    ## 21:43:19: Number of 'sunny' donors: 26 out of 26
    ## 21:43:19: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:19: with RMSPE 0.182550505205933 and MSPE (loss v) 
    ## 21:43:19: 0.0333246869509413 is INFEASIBLE when respecting the predictors.
    ## 21:43:20: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:22: Optimization finished (31141 calls to inner optimizer), rmspe: 
    ## 21:43:22: 0.182550505205936, mspe: 0.0333246869509425.
    ## Final rmspe: 0.1825505, mspe (loss v): 0.03332469
    ## Optimal weights:
    ##   Kabardian      Korean       Tatar     Bashkir   Bulgarian     Chuvash 
    ## 0.071109539 0.192456574 0.472227315 0.035029834 0.004386141 0.083568819 
    ##     Mordvin       Uzbek 
    ## 0.044077900 0.097143877 
    ## 
    ## 21:43:22: Using Khakas as treated unit now.
    ## 21:43:22: Number of 'sunny' donors: 26 out of 26
    ## 21:43:22: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:22: with RMSPE 0.38025687933314 and MSPE (loss v) 0.144595294280178 
    ## 21:43:22: is INFEASIBLE when respecting the predictors.
    ## 21:43:22: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:24: checking v: v contains NAs!
    ## 21:43:24: Optimization finished (29221 calls to inner optimizer), rmspe: 
    ## 21:43:24: 0.380256879333371, mspe: 0.144595294280354.
    ## Final rmspe: 0.3802569, mspe (loss v): 0.1445953
    ## Optimal weights:
    ##      Kalmyk      Korean   Ukrainian      Balkar     Bashkir     Mordvin 
    ## 0.023885745 0.004661854 0.134160606 0.469740499 0.125960751 0.241590546 
    ## 
    ## 21:43:24: Using Komi as treated unit now.
    ## 21:43:24: Number of 'sunny' donors: 26 out of 26
    ## 21:43:24: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:24: with RMSPE 0.658636954790221 and MSPE (loss v) 0.433802638215336 
    ## 21:43:24: is INFEASIBLE when respecting the predictors.
    ## 21:43:24: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:25: Optimization finished (26701 calls to inner optimizer), rmspe: 
    ## 21:43:25: 0.658636954790373, mspe: 0.433802638215536.
    ## Final rmspe: 0.658637, mspe (loss v): 0.4338026
    ## Optimal weights:
    ##     Kalmyk  Ukrainian    Chuvash   Karelian      Yakut 
    ## 0.18527847 0.26328510 0.09051489 0.33650148 0.12442006 
    ## 
    ## 21:43:25: Using Mari as treated unit now.
    ## 21:43:26: Number of 'sunny' donors: 26 out of 26
    ## 21:43:26: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:26: with RMSPE 0.118934134604143 and MSPE (loss v) 
    ## 21:43:26: 0.0141453283740364 is INFEASIBLE when respecting the predictors.
    ## 21:43:26: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:27: Optimization finished (27481 calls to inner optimizer), rmspe: 
    ## 21:43:27: 0.118934134604183, mspe: 0.0141453283740459.
    ## Final rmspe: 0.1189341, mspe (loss v): 0.01414533
    ## Optimal weights:
    ## Belorussian      Korean       Tatar     Mordvin       Uzbek       Yakut 
    ##  0.07279039  0.62719612  0.16345929  0.07665849  0.01897393  0.04092178 
    ## 
    ## 21:43:27: Using Moldovan as treated unit now.
    ## 21:43:27: Number of 'sunny' donors: 26 out of 26
    ## 21:43:27: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:27: with RMSPE 0.561782048956042 and MSPE (loss v) 0.315599070529249 
    ## 21:43:27: is INFEASIBLE when respecting the predictors.
    ## 21:43:28: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:30: Optimization finished (30721 calls to inner optimizer), rmspe: 
    ## 21:43:30: 0.561782048956059, mspe: 0.315599070529268.
    ## Final rmspe: 0.561782, mspe (loss v): 0.3155991
    ## Optimal weights:
    ##     Kalmyk  Bulgarian   Georgian       Komi    Mordvin     Udmurt 
    ## 0.39005537 0.12562831 0.23632041 0.07857017 0.04231630 0.08896961 
    ##      Yakut 
    ## 0.03813984 
    ## 
    ## 21:43:30: Using Mordvin as treated unit now.
    ## 21:43:30: Number of 'sunny' donors: 26 out of 26
    ## 21:43:30: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:30: with RMSPE 0.633836948700272 and MSPE (loss v) 0.401749277537671 
    ## 21:43:30: is INFEASIBLE when respecting the predictors.
    ## 21:43:30: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:31: Optimization finished (28561 calls to inner optimizer), rmspe: 
    ## 21:43:31: 0.63383694870037, mspe: 0.401749277537796.
    ## Final rmspe: 0.6338369, mspe (loss v): 0.4017493
    ## Optimal weights:
    ##      Tatar  Ukrainian    Bashkir    Chuvash     Khakas 
    ## 0.01786527 0.15658741 0.61382258 0.11484939 0.09687535 
    ## 
    ## 21:43:31: Using Udmurt as treated unit now.
    ## 21:43:32: Number of 'sunny' donors: 26 out of 26
    ## 21:43:32: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:32: with RMSPE 0.567319128010479 and MSPE (loss v) 0.321850993006571 
    ## 21:43:32: is INFEASIBLE when respecting the predictors.
    ## 21:43:32: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:34: Optimization finished (28261 calls to inner optimizer), rmspe: 
    ## 21:43:34: 0.567319128010479, mspe: 0.32185099300657.
    ## Final rmspe: 0.5673191, mspe (loss v): 0.321851
    ## Optimal weights:
    ##    Chechen      Altai    Bashkir    Chuvash   Moldovan      Uzbek 
    ## 0.18779153 0.12759211 0.52174651 0.07547374 0.02750820 0.05988793 
    ## 
    ## 21:43:34: Using Uzbek as treated unit now.
    ## 21:43:34: Number of 'sunny' donors: 26 out of 26
    ## 21:43:34: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:34: with RMSPE 0.572506904317076 and MSPE (loss v) 0.327764155490722 
    ## 21:43:34: is INFEASIBLE when respecting the predictors.
    ## 21:43:34: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:36: checking v: v contains NAs!
    ## 21:43:36: Optimization finished (52201 calls to inner optimizer), rmspe: 
    ## 21:43:36: 0.572506904317078, mspe: 0.327764155490724.
    ## Final rmspe: 0.5725069, mspe (loss v): 0.3277642
    ## Optimal weights:
    ##      Tatar     Balkar    Bashkir 
    ## 0.02470161 0.71313655 0.26216184 
    ## 
    ## 21:43:36: Using Yakut as treated unit now.
    ## 21:43:36: Number of 'sunny' donors: 26 out of 26
    ## 21:43:36: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:36: with RMSPE 0.982932417661721 and MSPE (loss v) 0.966156137690317 
    ## 21:43:36: is INFEASIBLE when respecting the predictors.
    ## 21:43:37: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:38: Optimization finished (26761 calls to inner optimizer), rmspe: 
    ## 21:43:38: 0.982932417661788, mspe: 0.966156137690447.
    ## Final rmspe: 0.9829324, mspe (loss v): 0.9661561
    ## Optimal weights:
    ##   Armenian     Jewish      Tatar       Komi 
    ## 0.65262877 0.09503534 0.14073481 0.11160107

``` r
sc_results_ethnicities_without_ind_state <- mscmt(data_prep_mscmt, treatment.identifier, controls.identifier,
                                                  times.dep, times.pred, agg.fns, seed=2019,  single.v=TRUE)
```

    ## 21:43:39: Number of 'sunny' donors: 27 out of 27
    ## 21:43:39: Unrestricted outer optimum (obtained by ignoring all predictors) 
    ## 21:43:39: with RMSPE 0.292445945065624 and MSPE (loss v) 
    ## 21:43:39: 0.0855246307853259 is INFEASIBLE when respecting the predictors.
    ## 21:43:39: Starting optimization via DEoptC, random seed 2019.
    ## 21:43:41: Optimization finished (37441 calls to inner optimizer), rmspe: 
    ## 21:43:41: 0.29244594506564, mspe: 0.0855246307853352.
    ## Final rmspe: 0.2924459, mspe (loss v): 0.08552463
    ## Optimal weights:
    ##     Jewish     Korean      Tatar  Ukrainian    Chuvash     Khakas 
    ## 0.18988509 0.13010902 0.53021288 0.11133226 0.01307907 0.02538168

``` r
placebo_highlight_all(sc_placebo_ethnicities_without_ind_state, "log_n_pred_full_imp_date")
```

    ## Warning: You set use_group_by = TRUE, but grouped calculation failed.
    ## Falling back to ungrouped filter operation...

    ## label_key: ethnicity

``` r
ggsave(here::here("plots/final/placebo_highlight_all_ethnicities_without_ind_state.pdf"))
```

    ## Saving 7 x 5 in image

``` r
ggsave(here::here("plots/for_presentation/placebo_highlight_all_ethnicities_without_ind_state.pdf"), scale = 0.7)
```

    ## Saving 4.9 x 3.5 in image

``` r
opts_current$set(label = "sc_weights_without_ind_state")
make_w_weights_table(sc_data = sc_results_ethnicities_without_ind_state, 
                     table_title = "Synthetic German minority weights, Only ethnicities without ind. state",
                                 file_name = "tables/sc_weights_without_ind_state.tex")

placebo_mspe_barplot(sc_placebo_ethnicities_without_ind_state, 
                     "log_n_pred_full_imp_date", range_post = c("1933", "1960"))
```

``` r
ggsave(here::here("plots/final/mspe_ratios_ethnicities_without_ind_state.pdf"))
```

    ## Saving 7 x 5 in image

``` r
placebo_mspe_barplot(sc_placebo_ethnicities_without_ind_state, 
                     "log_n_pred_full_imp_date", range_post = c("1933", "1939"))
```

``` r
ggsave(here::here("plots/final/mspe_ratios_ethnicities_without_ind_state_until_1939.pdf"))
```

    ## Saving 7 x 5 in image
