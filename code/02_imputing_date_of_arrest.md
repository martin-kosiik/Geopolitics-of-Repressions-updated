Imputing date of arrest
================
Martin Kosík
March 10, 2019

``` r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(broom)
library(lubridate)
library(scales)
library(stargazer)
library(data.table)
library(here)
```

Import and pre-process the data

``` r
selected_vars <- c("person_id", "arest_date", "process_date")
memorial_lists <- fread(here::here("memo_list/memorial_lists.tsv"), encoding="UTF-8", sep="\t", 
                        select = selected_vars, quote="")

#memorial_lists <- memorial_lists[surname != "None",]

memorial_lists <- memorial_lists[, no_date := ifelse(process_date == "None" & arest_date == "None", 1, 0)]

memorial_lists <- memorial_lists %>% 
  separate(arest_date, sep = "\\.", into = c("DAY", "MONTH", "YEAR"), remove = FALSE) %>% 
  separate(process_date, sep = "\\.", into = c("DAY_PROCESS", "MONTH_PROCESS", "YEAR_PROCESS"), remove = FALSE) %>%
  mutate_at(c("DAY", "MONTH", "YEAR", "DAY_PROCESS", "MONTH_PROCESS", "YEAR_PROCESS"),
             funs(as.numeric(ifelse(. %in% c("None", "_"), NA, .))))
```

    ## Warning: Expected 3 pieces. Missing pieces filled with `NA` in 1650941
    ## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    ## 20, ...].

    ## Warning: Expected 3 pieces. Missing pieces filled with `NA` in 943124 rows
    ## [105, 395, 454, 497, 549, 769, 8992, 9011, 9014, 9021, 9026, 9028, 9029,
    ## 9040, 9047, 9060, 9071, 9076, 9080, 9102, ...].

Get relative frequency of trials by months

``` r
month_freq <- memorial_lists %>% 
  filter(!is.na(MONTH_PROCESS)) %>% 
  add_count() %>% 
  group_by(MONTH_PROCESS) %>% 
  summarise(month_freq = n()/n[1]) %>% 
  pull(month_freq)
```

We subset only the data where some date ,either of arrest or trial (called process in the data) is available. We then sample month of trial where it is missing based in relative frequency of trials by month.

``` r
set.seed(2019)

some_date <-  memorial_lists %>% 
  filter(no_date == 0) %>% 
  mutate(MONTH_PROCESS_simple_imp = ifelse(is.na(MONTH_PROCESS),
                                           base::sample(1:12, size = 1, prob = rep(1/12, 12)),
                                           MONTH_PROCESS),
         MONTH_PROCESS_freq_imp = ifelse(is.na(MONTH_PROCESS),
                                           base::sample(1:12, size = 1, prob = month_freq),
                                           MONTH_PROCESS),
         DAY_PROCESS_simple_imp = ifelse(is.na(DAY_PROCESS),
                                           case_when(MONTH_PROCESS_simple_imp %in% c(1, 3, 5, 7, 8, 10, 12) ~ 
                                                       base::sample(1:31, size = 1, prob = rep(1/31, 31)),
                                                     MONTH_PROCESS_simple_imp %in% c(4, 6, 9, 11) ~ 
                                                       base::sample(1:30, size = 1, prob = rep(1/30, 30)),
                                                     MONTH_PROCESS_simple_imp == 2 ~
                                                       base::sample(1:28, size = 1, prob = rep(1/28, 28))),
                                           DAY_PROCESS),
         DAY_PROCESS_freq_imp = ifelse(is.na(DAY_PROCESS),
                                           case_when(MONTH_PROCESS_freq_imp %in% c(1, 3, 5, 7, 8, 10, 12) ~ 
                                                       base::sample(1:31, size = 1, prob = rep(1/31, 31)),
                                                     MONTH_PROCESS_freq_imp %in% c(4, 6, 9, 11) ~ 
                                                       base::sample(1:30, size = 1, prob = rep(1/30, 30)),
                                                     MONTH_PROCESS_freq_imp == 2 ~
                                                       base::sample(1:28, size = 1, prob = rep(1/28, 28))),
                                           DAY_PROCESS))
```

Save the subset of the data where both date of arrest and date of trial are available

``` r
both_dates <- some_date %>% 
  filter((!is.na(MONTH)) & (!is.na(MONTH_PROCESS))) %>% 
  mutate(date_arrest = parse_date_time(arest_date, "dmy"), 
         date_process = parse_date_time(process_date, "dmy")) %>% 
  filter(!(is.na(date_arrest) | is.na(date_process))) %>% 
  mutate(diff_days = interval(date_arrest, date_process) / ddays(1)) %>% 
  filter(YEAR_PROCESS >1920, YEAR_PROCESS < 1961) %>% 
  filter(diff_days >= 0)
```

    ## Warning: 5775 failed to parse.

    ## Warning: 2440 failed to parse.

Here we fit only simple log-linear model with only interept that we later plot over histogram.

``` r
lm_model_simple <-  lm(log10(1 +diff_days) ~ 1, data = both_dates)
lm_model_simple_intercept <- lm_model_simple$coefficients[[1]]
lm_model_simple_sigma <- summary(lm_model_simple)$sigma
```

Below we fit the two-step model described in the paper

``` r
glm.sign <- glm(I(diff_days > 0) ~ as.factor(YEAR_PROCESS), 
                data = both_dates, family = binomial(link = logit))

lm.ifpos <- lm(I(log(diff_days)) ~ as.factor(YEAR_PROCESS),
               data = both_dates, subset = diff_days > 0) 
```

Save the table with results of the model

``` r
var_labels <- names(glm.sign$coefficients) %>% 
  str_replace_all("as.factor\\(YEAR_PROCESS\\)", "Year of Trial - ")


stargazer(glm.sign, lm.ifpos, 
          omit.stat = c("ser", "f"), label = "tab:date_imp_results", font.size = "small", no.space = T, 
          table.placement = "!h", single.row = T, header = F,
          covariate.labels = var_labels, title = "Arrest Date Imputation - Model Results",
          dep.var.labels = c("$I^y$", "$log(y^{\\text{pos}})$")) %>% 
 cat(sep = '\n', file = here::here("tables/date_imp_results.tex"))
```

    ## 
    ## \begin{table}[!h] \centering 
    ##   \caption{Arrest Date Imputation - Model Results} 
    ##   \label{tab:date_imp_results} 
    ## \small 
    ## \begin{tabular}{@{\extracolsep{5pt}}lcc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{2}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-3} 
    ## \\[-1.8ex] & $I^y$ & $log(y^{\text{pos}})$ \\ 
    ## \\[-1.8ex] & \textit{logistic} & \textit{OLS} \\ 
    ## \\[-1.8ex] & (1) & (2)\\ 
    ## \hline \\[-1.8ex] 
    ##  (Intercept) & $-$0.771$^{***}$ (0.161) & 0.888$^{***}$ (0.025) \\ 
    ##   Year of Trial - 1922 & 1.630$^{***}$ (0.586) & 1.038$^{***}$ (0.033) \\ 
    ##   Year of Trial - 1923 & 0.955$^{*}$ (0.510) & 0.976$^{***}$ (0.039) \\ 
    ##   Year of Trial - 1924 & 2.128$^{**}$ (1.004) & 1.122$^{***}$ (0.043) \\ 
    ##   Year of Trial - 1925 & 1.019$^{*}$ (0.586) & 1.112$^{***}$ (0.043) \\ 
    ##   Year of Trial - 1926 & 0.508$^{*}$ (0.284) & 0.972$^{***}$ (0.027) \\ 
    ##   Year of Trial - 1927 & 0.346 (0.234) & 0.904$^{***}$ (0.024) \\ 
    ##   Year of Trial - 1928 & $-$0.260$^{**}$ (0.123) & 0.422$^{***}$ (0.016) \\ 
    ##   Year of Trial - 1929 & $-$0.209$^{**}$ (0.101) & 0.307$^{***}$ (0.012) \\ 
    ##   Year of Trial - 1930 & 0.012 (0.103) & 0.754$^{***}$ (0.012) \\ 
    ##   Year of Trial - 1931 & 0.166 (0.111) & 0.695$^{***}$ (0.013) \\ 
    ##   Year of Trial - 1932 & 0.299$^{***}$ (0.106) & 0.463$^{***}$ (0.012) \\ 
    ##   Year of Trial - 1933 & 0.193 (0.139) & 0.457$^{***}$ (0.016) \\ 
    ##   Year of Trial - 1934 & $-$0.198$^{*}$ (0.116) & 0.602$^{***}$ (0.014) \\ 
    ##   Year of Trial - 1935 & $-$0.206$^{*}$ (0.112) & 0.874$^{***}$ (0.014) \\ 
    ##   Year of Trial - 1936 & 0.858$^{***}$ (0.099) & $-$0.298$^{***}$ (0.011) \\ 
    ##   Year of Trial - 1937 & 1.116$^{***}$ (0.101) & 0.486$^{***}$ (0.012) \\ 
    ##   Year of Trial - 1938 & 1.845$^{***}$ (0.151) & 2.010$^{***}$ (0.013) \\ 
    ##   Year of Trial - 1939 & 1.560$^{***}$ (0.162) & 1.579$^{***}$ (0.013) \\ 
    ##   Year of Trial - 1940 & 0.463$^{***}$ (0.113) & 0.705$^{***}$ (0.013) \\ 
    ##   Year of Trial - 1941 & 0.282$^{***}$ (0.109) & 0.641$^{***}$ (0.013) \\ 
    ##   Year of Trial - 1942 & 0.234$^{**}$ (0.114) & 0.833$^{***}$ (0.013) \\ 
    ##   Year of Trial - 1943 & $-$0.422$^{***}$ (0.118) & 0.804$^{***}$ (0.015) \\ 
    ##   Year of Trial - 1944 & 0.175 (0.127) & 0.936$^{***}$ (0.015) \\ 
    ##   Year of Trial - 1945 & 0.264$^{*}$ (0.142) & 1.182$^{***}$ (0.016) \\ 
    ##   Year of Trial - 1946 & 0.164 (0.161) & 0.987$^{***}$ (0.018) \\ 
    ##   Year of Trial - 1947 & 0.231 (0.179) & 0.860$^{***}$ (0.020) \\ 
    ##   Year of Trial - 1948 & 0.810$^{***}$ (0.192) & 0.735$^{***}$ (0.017) \\ 
    ##   Year of Trial - 1949 & 0.512$^{***}$ (0.186) & 0.953$^{***}$ (0.019) \\ 
    ##   Year of Trial - 1950 & 0.532$^{***}$ (0.188) & 0.908$^{***}$ (0.019) \\ 
    ##   Year of Trial - 1951 & $-$0.080 (0.197) & 0.844$^{***}$ (0.024) \\ 
    ##   Year of Trial - 1952 & 0.077 (0.269) & 0.619$^{***}$ (0.031) \\ 
    ##   Year of Trial - 1953 & 0.003 (0.589) & 1.680$^{***}$ (0.070) \\ 
    ##   Year of Trial - 1954 & $-$0.526 (0.462) & 2.253$^{***}$ (0.071) \\ 
    ##   Year of Trial - 1955 & $-$0.713$^{*}$ (0.425) & 1.324$^{***}$ (0.071) \\ 
    ##   Year of Trial - 1956 & 0.950$^{***}$ (0.367) & 0.683$^{***}$ (0.029) \\ 
    ##   Year of Trial - 1957 & 0.595 (0.367) & 0.813$^{***}$ (0.034) \\ 
    ##   Year of Trial - 1958 & $-$0.232 (0.333) & 1.036$^{***}$ (0.044) \\ 
    ##   Year of Trial - 1959 & $-$0.716 (0.516) & 1.042$^{***}$ (0.087) \\ 
    ##   Year of Trial - 1960 & 4.292$^{***}$ (0.094) & 3.697$^{***}$ (0.011) \\ 
    ##  \hline \\[-1.8ex] 
    ## Observations & 812,592 & 805,800 \\ 
    ## R$^{2}$ &  & 0.235 \\ 
    ## Adjusted R$^{2}$ &  & 0.235 \\ 
    ## Log Likelihood & $-$38,300.970 &  \\ 
    ## Akaike Inf. Crit. & 76,681.930 &  \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{2}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

Finally, we impute the missing values and save the results

``` r
missing_date_of_arrest <- some_date %>% 
  filter(is.na(YEAR) & !is.na(YEAR_PROCESS)) %>% 
  dplyr::select(person_id, YEAR_PROCESS) %>% 
  filter(YEAR_PROCESS >1920, YEAR_PROCESS < 1961)

set.seed(2019)

missing_date_of_arrest <-missing_date_of_arrest %>% 
  mutate(diff_day_imp = round(exp(rnorm(n(), mean = lm_model_simple_intercept, sd = lm_model_simple_sigma)) - 1),
         diff_day_imp = ifelse(diff_day_imp < 0, 0, diff_day_imp))


missing_date_of_arrest <- missing_date_of_arrest %>% 
  mutate(pred.sign = rbinom(n(), 1, predict(glm.sign, ., type="response")),
         pred.pos.log = rnorm(n(), mean = predict(lm.ifpos, .), sd = summary(lm.ifpos)$sigma),
         diff_day_imp_complex = round(exp(pred.pos.log) * pred.sign))


complex_imp <- some_date %>% 
  left_join(missing_date_of_arrest %>% dplyr::select(person_id, diff_day_imp_complex), by = "person_id") %>% 
  mutate(date_process_freq_imp = as_date(str_c(YEAR_PROCESS, MONTH_PROCESS_freq_imp, DAY_PROCESS_simple_imp,
                                         sep = "-")),
         date_arrest_complex_imp = date_process_freq_imp - days(diff_day_imp_complex),
         year_arrest_complex_imp = year(date_arrest_complex_imp), 
         source_date_arrest = case_when(!is.na(YEAR) ~ "label",
                                        !is.na(year_arrest_complex_imp) ~ "prediction",
                                        TRUE ~ "missing"),
         YEAR = ifelse(!is.na(year_arrest_complex_imp), year_arrest_complex_imp, YEAR)) 
```

    ## Warning: 177 failed to parse.

``` r
imputation <- complex_imp %>% 
  filter(source_date_arrest != "missing") %>% 
  dplyr::select(person_id, YEAR, source_date_arrest)

saveRDS(imputation, file = here::here("data/arrest_date_imputation.RData"))
```

Plots
-----

``` r
mylog10_trans <- function (base = 10) 
{
  trans <- function(x) log(x + 1, base)
  inv <- function(x) base^(x) - 1 
  trans_new(paste0("log-", format(base)), trans, inv, log_breaks(base = base), 
            domain = c(0, Inf))
}


missing_date_of_arrest %>% 
  mutate(log_day = log(1 + diff_day_imp_complex)) %>% 
  ggplot(aes(x = diff_day_imp_complex)) + geom_histogram(aes(y=..density..),col = "white") + theme_minimal()+
  scale_x_continuous(trans = "mylog10", breaks = c(0,2,10,100,1000,10000))+
  labs(x = "Number of days between arrest and trail") + 
  theme(axis.line = element_line(size = 1), 
       # panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        text = element_text(size=12)) + expand_limits(x = -0.01)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

``` r
ggsave(here::here("plots/imputing_arrest_date/mixed_model_preds_hist.pdf"))
```

    ## Saving 7 x 5 in image
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

``` r
both_dates %>% 
  #filter(abs(diff_days) < 5000) %>% 
  mutate(log_diff_days = log(1 + diff_days)) %>% 
  ggplot(aes(x = diff_days)) + geom_histogram(aes(y=..density..), col = "white") + 
    #geom_density(aes(y=..density.., position="stack"), col = "red") +
    stat_function(fun = function(x, mean, sd) dnorm(log10(x + 1) , mean, sd), 
                  args = list(mean = lm_model_simple$coefficients[1], sd = summary(lm_model_simple)$sigma), 
                  col = "blue", size=1.5) + 
  scale_x_continuous(trans = "mylog10", breaks = c(0,2,10,100,1000,10000)) +
  theme_minimal() + labs(x = "Number of days between arrest and trial") +
  theme(axis.line = element_line(size = 1), 
       # panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        text = element_text(size=12))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

``` r
ggsave(here::here("plots/imputing_arrest_date/simple_lm_hist.pdf"))
```

    ## Saving 7 x 5 in image
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
