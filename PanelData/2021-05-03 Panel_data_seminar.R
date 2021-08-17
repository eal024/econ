


# # packages and data -----------------------------------------------------

library(tidyverse)
female_labour <-haven::read_dta("data/femalelabour.dta")

# How many individuals?
female_labour %>% summarise( unike = n_distinct(nr))
female_labour %>%  group_by(nr) %>% add_count() %>% group_by(year,n ) %>% count() %>% arrange( year ) %>% pivot_wider( names_from = n, values_from = nn)

# Unbalaned panel
female_labour %>%  group_by(nr) %>% add_count() %>% group_by(n) %>% count() %>%
  ggplot(aes(y = nn, x =n)) + geom_col() +
  labs( title = "numer of individ that has N# observation", y = "freq", x = "nr#", subtitle = "number on top shows how many indi. with n observation")  +
  geom_text( aes( label = round(nn,1)/round(n,1), ), vjust = -1) 

# Between, Between: shows the variation between groups.
female_labour %>%
  group_by( i ) %>%
  summarise( m = mean(wage), .groups = "drop") %>% 
  summarise( mean_of_mean_w = mean(m),
             sd_of_mean = sd(m),
             min_of_mean = min(m)
             )
# Between individs
# Within variation: xi - hatxi + hat_hatx
# hat_hat_x -> mean for all indiv. and time: global mean
# Within variation: It shows how much a variable varies within each group(deviation from its average). 
# For example, how yearly wage varies with time for an individual i

# Bigger variation between individual than within.

# Graphical vizu -----------------------------------------------------------

female_labour %>% 
  mutate( xx = mean(wage)) %>% 
  group_by( i) %>% 
  mutate( m_o_w = mean(wage) ) %>%
  ungroup() %>% 
  mutate( w = wage - m_o_w + xx) %>% 
  summarise( mean_of_mean = mean(w),
             sd_of_mean = sd(w),
             min_of_mean = min(w)
  )



# Models ------------------------------------------------------------------

## b) 

# Variables
tibble( names = names(female_labour), label = map_chr( female_labour, ~attr(.x, "label") )) %>% tail(20) 


pooled_ols <- female_labour %>% lm( wage ~ mar + black + hisp + school + exper + ex2   , data = . )
summary(pooled_ols)

# Model1: OLS pooling (cluster individ) 
library(plm)
ols_pooling <- female_labour %>% plm(formula =  wage ~ mar + black + hisp + school + exper + ex2 + rur + uwage, model =   "pooling")

model1 <-  clubSandwich::coef_test( ols_pooling, cluster = female_labour$nr, vcov = "CR1")

model1

ols_pooling_cl_nr_time_dummies <- female_labour %>% plm(formula =  wage ~ mar + black + hisp + school + exper + ex2 + rur + uwage +I(factor(year)), model =   "pooling") 
  
summary(ols_pooling)

model2 <- clubSandwich::coef_test(ols_pooling_cl_nr_time_dummies, cluster = female_labour$nr, vcov = "CR1")

model2

# Fixed effects model:
model3_within <- female_labour %>% plm(formula =  wage ~ mar + black + hisp + school + exper + ex2 + rur + uwage, model =   "within")  

model3_within_se <- clubSandwich::coef_test(model3_within, cluster = female_labour$nr, vcov = "CR1")

model3_within %>% summary()

# Random effects
model4_random <- female_labour %>% plm(formula =  wage ~ mar + black + hisp + school + exper + ex2 + rur + uwage, model =   "random")  

# Between
model4_between <- female_labour %>% plm(formula =  wage ~ mar + black + hisp + school + exper + ex2 + rur + uwage, model =   "between")  
summary(model4_random)
summary(model4_between)


stargazer::stargazer(models = list(ols_pooling, model3_within, model4_between, model4_random) , type = "text" )






