library(readr)
library(tidyverse)
library(broom)
library(stargazer)
guess_encoding(events)

guess_encoding(head(events$NAME, 1000))

by_year <- events %>%
  count(YEAR, ETHNIC_GER) 
by_year_minorities <- events %>%
  filter_at(vars(ETHNIC_BEL:ETHNIC_CHECHEN), any_vars(. == 1)) %>% 
  count(YEAR, ETHNIC_GER) 
names(events)


events <- events %>%
  rowwise() %>% 
  mutate(ethnicity = case_when(ETHNIC_GER == 1 ~ "German",
                               any(!!paste0("c(", paste0(names(events)[38:54], collapse = ", "), ")") == 1) ~ "Other minority",
                                                             TRUE ~ "Russian"))
events_since_1923 <- filter(events, YEAR >= 1923)

indiv_model_no_controls <- lm(ETHNIC_GER ~ factor(YEAR), data = events_since_1923)
stargazer(indiv_model_no_controls, omit.stat = c("f", "res.dev"), type = "html", out =  "C:\\Users\\Martin\\Desktop\\Thesis2\\reg_summary.html")
table(events$ethnicity)
paste0("c(", paste0(names(events)[38:54], collapse = ", "), ")")
by_year$YEAR

paste0("v_" ,years, collapse = ", ")

total_by_ethnicity <- events %>% 
  group_by_at(vars(ETHNIC_RUS:ETHNIC_CHECHEN)) %>% 
  count()

total_by_ethnicity %>% 
  rowid_to_column() %>% 
  mutate(ethnic = case_when(ETHNIC_GER == 1 ~ "German",
                            ETHNIC_RUS == 1~ "Russian",
                            rowid == 1 ~ "Unknown",
                            TRUE ~ "Other minorities")) %>% 
  group_by(ethnic) %>% 
  summarize(total = sum(n)) %>% 
  ggplot(aes(x = fct_reorder(ethnic, total, desc = T), y= total)) + geom_bar(stat="identity") +
  theme_bw() +xlab(element_blank()) + scale_y_continuous(labels = comma)

pop_df <- data.frame(total_pop = c( 1238549, 2599973 + 4738923 +272272  + 31194976 + 4738923 + 2916536 + 1567568, 	77791124))
library(scales)
total_by_ethnicity %>% 
  rowid_to_column() %>% 
  filter(rowid != 1) %>% 
  mutate(ethnic = case_when(ETHNIC_GER == 1 ~ "German",
                            ETHNIC_RUS == 1~ "Russian",
                            TRUE ~ "Other minorities")) %>% 
  group_by(ethnic) %>% 
  summarize(total = sum(n)) %>% 
  bind_cols(pop_df) %>% 
  mutate(persecution_pct = total/total_pop * 100) %>% 
  ggplot(aes(x = fct_reorder(ethnic, persecution_pct, desc = T), y= persecution_pct)) + geom_bar(stat="identity") +
  theme_bw() +xlab(element_blank()) +ylab("Percent of population persecuted")

ggsave("barchart_pct.pdf")

by_year %>% 
  group_by_at(vars(v_1929:v_1940)) %>% 
  count()
years <- 1929:1945


year_dummies <- map_dfc(years, ~  as.numeric(by_year$YEAR >= .)) %>% 
  rename_all( funs( c(paste0("v_" ,years))))

by_year <-bind_cols(by_year, year_dummies) %>% 
  mutate(log_n = log(n))
 
fmla <- as.formula(paste("log_n ~ ", paste0("v_", years, ":ETHNIC_GER", collapse = " + "),  "+ as.factor(YEAR) + ETHNIC_GER"))

summary(lm(fmla,  data = by_year))
dif_model_all <- lm(fmla,  data = by_year)

ggplot(by_year, aes(x=log_n))+ geom_histogram()+ facet_wrap(~ ETHNIC_GER) 
ggsave("hist_all.pdf")

out_comp <- tidy(lm(fmla,  data = by_year),  conf.int = TRUE) %>% 
  filter(str_detect(term, "ETHNIC_GER:")) %>% 
  mutate(nice_labs = str_replace(term,"ETHNIC_GER:v_",""))
out_comp 
ggplot(out_comp, mapping = aes(x = nice_labs, y = estimate, ymin = conf.low, ymax = conf.high, group = 1))+ 
  geom_pointrange()+  geom_hline(yintercept= 0) + theme_bw() + labs(x = "year", title = "Dependent variable: Logarithm of all persecutions")

ggsave("did_all.pdf")

# minorities
year_dummies_min <- map_dfc(years, ~  as.numeric(by_year_minorities$YEAR >= .)) %>% 
  rename_all( funs( c(paste0("v_" ,years))))


by_year_minorities <-bind_cols(by_year_minorities, year_dummies_min) %>% 
  mutate(log_n = log(n))
fmla <- as.formula(paste("log_n ~ ", paste0("v_", years, ":ETHNIC_GER", collapse = " + "),  "+ as.factor(YEAR) + ETHNIC_GER"))

nrow(by_year_minorities)

summary(lm(fmla,  data = by_year_minorities))

out_comp_min <- tidy(lm(fmla,  data = by_year_minorities),  conf.int = TRUE) %>% 
  filter(str_detect(term, "ETHNIC_GER:")) %>% 
  mutate(nice_labs = str_replace(term,"ETHNIC_GER:v_",""))


dif_model_min <- lm(fmla,  data = by_year_minorities)

ggplot(out_comp_min, mapping = aes(x = nice_labs, y = estimate, ymin = conf.low, ymax = conf.high, group = 1))+ 
  geom_pointrange() + theme_bw() + geom_hline(yintercept= 0) +
  labs(x = "year", title = "Dependent variable: Logarithm of persecutions against ethnic minorities")
ggsave("did_min.pdf")

stargazer(dif_model_all, dif_model_min,labels=c('Log of all per.','Log of per. of minorities') , type = "html", out = "reg_summary.html")

events %>%
  count(YEAR, ETHNIC_GER) %>% 
  filter(ETHNIC_GER == 1) %>% 
  ggplot(aes(x= YEAR, y= n, group = ETHNIC_GER)) + geom_line() + scale_x_continuous(breaks=seq(1920, 1960, 2))


events %>%
  count(YEAR, ETHNIC_GER) %>% 
  filter(ETHNIC_GER == 1) %>% 
  ggplot(aes(x= YEAR, y= n, group = ETHNIC_GER)) + geom_line() + scale_x_continuous(breaks=seq(1920, 1960, 2))
