Missing data summary tables and ethnicity imputations
================
Martin Kosík
March 18, 2019

Missing data summary tables
---------------------------

Import data

``` r
selected_vars <- c("person_id", "nation", "arest_date", "process_date", "surname", "name")
memorial_lists <- fread(here::here("memo_list/memorial_lists.tsv"), encoding="UTF-8", sep="\t", 
                        select = selected_vars, quote="")

memorial_lists <- memorial_lists[surname != "None",]
memorial_lists <- memorial_lists[, no_date := ifelse(process_date == "None" & arest_date == "None", 1, 0)]
memorial_lists <- memorial_lists[, no_ethnicity := ifelse(nation == "None", 1, 0)]
memorial_lists <- memorial_lists[, no_date_of_arrest := ifelse(arest_date == "None", 1, 0)]
memorial_lists <- memorial_lists[, no_date_of_process := ifelse(process_date == "None", 1, 0)]
```

Contingency tables of missing ethnicity and dates af arrest and process

``` r
opts_current$set(label = "missingness_of_ethnicity_and_date")
memorial_lists %>% 
  count(no_date, no_ethnicity) %>% 
  mutate(no_date = recode_factor(no_date, `1` = "Missing",  `0` = "Present"),
         no_ethnicity = recode_factor(no_ethnicity, `1` = "Missing", `0` = "Present")) %>% 
  spread(key = no_date, value = n) %>% 
    kable("latex", booktabs = T, caption = "Missingness of Ethnicity and Dates of Arrest and Trial",
          linesep = "",
        col.names = c("Ethnicity", "Missing", "Present"), format.args = list(big.mark = " ")) %>% 
  add_header_above(c(" " = 1, "Date of Arrest or Trial" = 2)) %>% 
  #kable_styling(font_size = 8) %>% 
  write_file(here::here("tables/missingness_of_ethnicity_and_date.tex"))

opts_current$set(label = "missing_date_of_arrest_process")
memorial_lists %>% 
  count(no_date_of_arrest, no_date_of_process) %>% 
  mutate(no_date_of_arrest = recode_factor(no_date_of_arrest, `1` = "Missing",  `0` = "Present"),
         no_date_of_process = recode_factor(no_date_of_process, `1` = "Missing", `0` = "Present")) %>% 
  spread(key = no_date_of_arrest, value = n) %>% 
  kable("latex", booktabs = T, caption = "Missing Dates of Arrest and Trial", linesep = "",
        col.names = c("Date of Trial", "Missing", "Present"), format.args = list(big.mark = " ")) %>% 
  add_header_above(c(" " = 1, "Date of Arrest" = 2)) %>% 
  write_file(here::here("tables/missing_date_of_arrest_process.tex"))
```

Create LaTeX table showing proportion of missing values by variable

``` r
n_of_obs <- nrow(memorial_lists)

opts_current$set(label = "missing_data_count")
memorial_lists %>% 
  dplyr::select(nation, arest_date, process_date, name) %>% 
  summarise_all(funs(sum(. == "None"))) %>% 
  gather(key = "Variable", value = "missing_count") %>% 
  mutate(missing_pct = missing_count/n_of_obs * 100,
         Variable = recode_factor(Variable, "nation" = "Ethnicity","arest_date" = "Date of Arrest", 
                       "process_date" =  "Date of Trial", "name" = "First Name", "surname" = "Surname")) %>% 
  kable("latex", booktabs = T, caption = "Missing Data by Variable", linesep = "", digits = 2,
        col.names = c("Variable", "Number of Missing Obs.", "Percent of Missing Obs."),
        format.args = list(big.mark = " ")) %>% 
  write_file(here::here("tables/missing_data_count.tex"))
```

Ethnicity imputation
--------------------

Re-import data

``` r
rm(list = ls())
selected_vars <- c("person_id", "nation", "surname", "name", "patronimic")


memorial_lists <- fread(here::here("memo_list/memorial_lists.tsv"), encoding="UTF-8", sep="\t", 
                        select = selected_vars, quote="", stringsAsFactors = TRUE)

translations <- read_excel(here::here("data/ethnicity_translations.xlsx")) %>% 
  mutate(ethnicity = ifelse(ethnicity == "Uyghur", NA, ethnicity))
```

Split the data based on missingness of information on ethnicity

``` r
memorial_lists <-
  memorial_lists %>% 
  left_join(translations, by = "nation") %>% 
  mutate(ethnicity = as.factor(ifelse(is.na(ethnicity), "Other", ethnicity)))  
```

    ## Warning: Column `nation` joining factor and character vector, coercing into
    ## character vector

``` r
data_labeled <- memorial_lists[!memorial_lists$ethnicity %in% c("Missing", "Other"),]
data_missing <- memorial_lists[memorial_lists$ethnicity  == "Missing",]
rm(memorial_lists)
rm(translations)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  2609957 139.4    4182832 223.4  3591513 191.9
    ## Vcells 45798751 349.5   94885937 724.0 94687351 722.5

``` r
data_labeled_person_id <- data_labeled$person_id
data_labeled$ethnicity <- droplevels(data_labeled$ethnicity)

data_labeled <- data_labeled %>% 
  dplyr::select(ethnicity, surname, name , patronimic)
```

Perform 10-fold cross validation and save the resulting confusion matrix

``` r
set.seed(2019)
folds <- sample(1:10, size = nrow(data_labeled), replace = T)

CV_nb <- lapply(1:10, function(x){ 
  nb <- nb('ethnicity', data_labeled)
  model <- lp(nb, data_labeled[folds != x,] , smooth = 0.005)
  preds <- predict(model,  data_labeled[folds == x,])
  return(data.frame(preds, ethnicity = data_labeled$ethnicity[folds == x]))
})

CV_nb <- do.call(rbind, CV_nb)
conf_matrix <- confusionMatrix(CV_nb$preds, CV_nb$ethnicity)

saveRDS(conf_matrix, file = here::here("data/conf_matrix.RData"))
rm(list = c("CV_nb", "folds", "conf_matrix"))
gc()
```

    ##            used  (Mb) gc trigger  (Mb)  max used  (Mb)
    ## Ncells  2632400 140.6    4182832 223.4   3591513 191.9
    ## Vcells 42289017 322.7  113943124 869.4 113930083 869.3

Train the model on all data with known ethnicity and use it to predict the missing ethnicity

``` r
set.seed(2019)

nb <- nb('ethnicity', data_labeled)
nb_model <- lp(nb, data_labeled , smooth = 0.005)

data_missing$ethnicity <- predict(nb_model, data_missing)
data_labeled$person_id <- data_labeled_person_id
data_missing <- data_missing %>% 
  dplyr::select(ethnicity, surname, name , patronimic, person_id)
```

Join the datasets and save the results

``` r
events_preds <- 
  bind_rows(data_labeled, data_missing, .id = "id") %>% 
  mutate(prediction = ifelse(id == 1, 0, 1)) %>% 
  dplyr::select(person_id, -id, everything())

fwrite(events_preds, here::here("data/events_predictions.csv"))
```
