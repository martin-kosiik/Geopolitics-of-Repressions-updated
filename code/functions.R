
# Logarithmic transformations
mylog10_trans <- function (base = 10) {
  trans <- function(x) log(x + 1, base)
  inv <- function(x) base^(x) - 1 
  trans_new(paste0("log-", format(base)), trans, inv, log_breaks(base = base), 
            domain = c(0, Inf))
}

mylog10_abs_sign_trans <- function (base = 10) {
  trans <- function(x) sign(x) * log(abs(x) + 1, base)
  inv <- function(x) sign(x)* ( base^(abs(x)) - 1 )
  trans_new(paste0("log-", format(base)), trans, inv, log_breaks(base = base), 
            domain = c(-Inf, Inf))
}


mynaturallog_trans <- function (base = exp(1)) 
{
  trans <- function(x) log(x + 1, base)
  inv <- function(x) base^(x) - 1 
  trans_new(paste0("log-", format(base)), trans, inv, log_breaks(base = base), 
            domain = c(0, Inf))
}


# Plot coefficients of difference-in-differences with robust standard errors

plot_effects_robust_se <- function(data = min_by_year, formula = fmla, vcov = "CR2", level = 0.95, 
                                   x_axis_breaks = seq(1922,1960,1), x_labels_angle = 60, x_labels_hjust = 1){
  model <- lm(formula,  data = data)
  model %>% 
    conf_int(vcov = vcov,  level = level,
             cluster = data$ethnicity,  test = "Satterthwaite") %>% 
    as_tibble(rownames = "term") %>% 
    filter(str_detect(term, "german:")) %>% 
    rename(conf.low = CI_L, conf.high = CI_U, estimate = beta) %>% 
    mutate(nice_labs = str_replace(term,"german:year_","")
           , nice_labs = as.numeric(nice_labs)
           #,nice_labs = ifelse(as.numeric(nice_labs)== max(as.numeric(nice_labs)), str_c(nice_labs, "+"), nice_labs)
    )%>% 
    ggplot(mapping = aes(x = nice_labs, y = estimate, ymin = conf.low, ymax = conf.high, group = 1))+ 
    geom_vline(xintercept= 1932.5, col = "red", linetype = "dashed", size = 1)+
    geom_pointrange()+  geom_hline(yintercept= 0) + theme_minimal() + 
    theme(axis.line = element_line(size = 1), 
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          text = element_text(size=14),
          axis.text.x = element_text(angle = x_labels_angle, hjust = x_labels_hjust)) +
    scale_x_continuous(breaks=x_axis_breaks)+
    labs(x = "Year", 
         #caption = "error bars show 95% confidence intervals \n 
         #                      SE are based on the cluster-robust estimator by Pustejovsky and Tipton (2018)", 
         y = "Coefficient")
}


# Synthetic control method - plots

placebo_highlight_all <- function(placebo_data = sc_placebo, dep_var = dep_var){
  tk_tbl(placebo_data$placebo[[dep_var]]$gaps) %>% 
    mutate(date = as_date(str_c(index,"-01-01"))) %>% 
    gather("ethnicity", dep_var, -c(date, index)) %>% 
    ggplot(aes(date, dep_var, col = ethnicity)) + geom_line(size = 1)  + gghighlight(ethnicity == "German") +
    theme_minimal() +
    theme(axis.line = element_line(size = 1), 
          #panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
          text = element_text(size=12)) + 
    labs(x = element_blank(), y = "gaps in log(1 + arrests)") +
    geom_vline(xintercept= ymd(19330101), col = "black", linetype = "dashed", size = 1)+
    scale_x_date(date_minor_breaks = "2 year", date_breaks = "4 year", date_labels = "%Y")
}


placebo_highlight_mspe <- function(placebo_data = sc_placebo, dep_var = dep_var, exclusion_ratio = 5){
  
  rmspe_placebo <- placebo_data[-length(placebo_data)] %>% 
    map("rmspe") %>% 
    map_dbl(1) 
  
  rmspe_placebo_tibble <- tibble(ethnicity = names(rmspe_placebo), rmspe = rmspe_placebo) %>% 
    mutate(mspe = rmspe^2) %>% 
    mutate( mspe_german = mspe[ethnicity == "German"],
            mspe_xtimes_higher = ifelse(exclusion_ratio * mspe_german < mspe, 1, 0))
  
  tk_tbl(placebo_data$placebo[[dep_var]]$gaps) %>% 
    mutate(date = as_date(str_c(index,"-01-01"))) %>% 
    gather("ethnicity", dep_var, -c(date, index)) %>% 
    left_join(rmspe_placebo_tibble, by = "ethnicity") %>% 
    filter(!mspe_xtimes_higher) %>% 
    ggplot(aes(date, dep_var, col = ethnicity)) + geom_line(size = 1)  + gghighlight(ethnicity == "German") +
    theme_minimal() +
    theme(axis.line = element_line(size = 1), 
          #panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
          text = element_text(size=12)) + 
    labs(x = element_blank(), y = "gaps in log(1 + arrests)") +
    geom_vline(xintercept= ymd(19330101), col = "black", linetype = "dashed", size = 1)+
    scale_x_date(date_minor_breaks = "2 year", date_breaks = "4 year", date_labels = "%Y") 
}


placebo_mspe_barplot <- function(placebo_data = sc_placebo, dep_var = dep_var, range_post = c("1933","1939")){
  
  
  mspe_ratios <-  ppratio(placebo_data, dep_var, range.pre = c("1921","1932"),
                          range.post = range_post, type = "mspe")
  
  
  tibble(mspe_ratios, ethnicity = names(mspe_ratios)) %>%
    mutate(ethnicity = fct_reorder(ethnicity, mspe_ratios), 
           german = as.factor(ifelse(ethnicity == "German", 1, 0))) %>% 
    ggplot(aes(x = ethnicity, y = mspe_ratios, fill = german))+ geom_col() + coord_flip() + theme_minimal() +
    labs(x = element_blank(), y = "MSPE ratio", fill = element_blank()) + guides(fill = FALSE)+
    scale_fill_manual(breaks = c("0", "1"),  values=c("gray", "#FF3300"))
  
}

