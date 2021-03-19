

# 
library(tidyverse)


# Exercise 1 --------------------------------------------------------------

library(rdrobust)
data("rdrobust_RDsenate")
as_tibble(rdrobust_RDsenate) %>% 
  # Margin above 0 Democrates win, 0> loose
  # dependent vote: The Democratic vote share in the following election (6 years after)
  ggplot( aes( x = margin, y = vote)) + 
  geom_point( alpha = .5) +
  geom_smooth( data = as_tibble(rdrobust_RDsenate) %>% filter( margin < 0), aes( margin, vote), method = "lm", se = F, color ="red") + 
  geom_smooth( data = as_tibble(rdrobust_RDsenate) %>% filter( margin > 0), aes( margin, vote), method = "lm", se = F, color = "yellow") +
  geom_smooth( data = as_tibble(rdrobust_RDsenate) %>% filter( margin < 0), aes( margin, vote),    se = F, color ="red") + 
  geom_smooth( data = as_tibble(rdrobust_RDsenate) %>% filter( margin > 0), aes( margin, vote),  se = F, color = "red")


rdrobust( y = rdrobust_RDsenate$vote, x = rdrobust_RDsenate$margin)

#
rdplot(y = rdrobust_RDsenate$vote, x = rdrobust_RDsenate$margin)


as_tibble(rdrobust_RDsenate) %>% mutate(margin = round(margin)) %>% group_by( margin) %>% summarise( vote = mean(vote)) %>% 
  ggplot( aes(x = margin, y = vote)) + 
  geom_point( ) +
  geom_smooth( data = as_tibble(rdrobust_RDsenate) %>% filter( margin < 0), aes( margin, vote), method = "lm", se = F, color ="red") + 
  geom_smooth( data = as_tibble(rdrobust_RDsenate) %>% filter( margin > 0), aes( margin, vote), method = "lm", se = F, color = "yellow") +
  geom_smooth( data = as_tibble(rdrobust_RDsenate) %>% filter( margin < 0), aes( margin, vote),    se = F, color ="red", linetype = 2) + 
  geom_smooth( data = as_tibble(rdrobust_RDsenate) %>% filter( margin > 0), aes( margin, vote),  se = F, color = "red" , linetype = 2) +
  theme_bw()


# Exercise 2 --------------------------------------------------------------

# gifted and talented program
# Students are assigned based on a cutoff score in cognitive aptitude-test

gift_and_talented <- haven::read_dta("data/econ4137sem06rd.dta")

gift_and_talented <- gift_and_talented %>% 
  # normalize the runningvariable from 0 which is the cutoff value
  mutate( x = ist_raw - x0)

# If larger than a threshold (x0), student were eligible for the program.

# Tests core : ist_raw,
# Score from primary school (exit test) = cito_raw

# Interested in causal effect of the treatment

# a) Descriptive data
gift_and_talented %>%
  mutate(  x = ist_raw - x0) %>% 
  mutate( x_group = x - x%%5 ) %>% 
  group_by(cohort, treat) %>% 
  summarise( n = n(),
             age = mean(age),
             male = sum(male)/n,
             cito_raw = mean(cito_raw),
             reswis_raw = mean(reswis_raw, na.rm = T),
             treat = sum(treat)/n
             ) %>% 
  pivot_longer( -c(cohort, treat)) %>% 
  pivot_wider( names_from = treat, values_from = value)
  

gift_and_talented %>% mutate( x_s = ist_raw - x0 ) %>% group_by(treat) %>% summarise_all( function(x) mean(x,na.rm = T))
  

# b) Estimate the Treatment effect from OLS

gifted_ols <- gift_and_talented %>% lm( reswis_raw ~ treat, data = .)
gifted_ols_c <- gift_and_talented %>% lm( reswis_raw ~ treat + male +  age + I(cohort) + cito_raw, data = .)
gifted_ols_c_below <- gift_and_talented %>% mutate( x = ist_raw - x0) %>% 
  filter( x < 0) %>% 
  lm( reswis_raw ~ treat + male +  age + I(cohort) + cito_raw, data = .)

stargazer::stargazer(gifted_ols, gifted_ols_c, gifted_ols_c_below , type = "text")

# c) 
gift_and_talented_balance <- gift_and_talented %>%
  mutate(  x = ist_raw - x0) 

balanced_nested <- tibble( interval = c(1,2,5,10), data =  list(gift_and_talented_balance, gift_and_talented_balance,gift_and_talented_balance,gift_and_talented_balance) ) %>% 
  # Filter variable x, near given by interval 2 , +-2 from 0
  mutate( data = map2( interval, data ,function(inter , data) { data %>% filter( near(x,0, tol =inter )   )} )) 


var

# t-test on each data
balanced_nested_t_test <- balanced_nested %>%
  #filter( interval == 1) %>%
  mutate( t_test = map(data, function(x){
    df <- as.data.frame(x) 
    t_test <- lapply(var, function(v){ t.test( df[, v] ~ df[,"treat"]) } ) 
    t_test
    
  }) )       

# a <- balanced_nested_t_test$t_test[[1]]
# str(a)[[1]]$statistic

balanced_nested_t_test %>% 
  mutate( t_test_sum =  map( t_test, function(x){
    tibble( 
      name = var,
      t_value = map_dbl(x, ~.x$statistic ),
      gr_1 = map_dbl(x, ~.x$estimate[1]),
      gr_2 = map_dbl(x, ~.x$estimate[2])
    ) %>% 
      mutate( diff = gr_1 - gr_2)
  })) %>% 
  select(-data, -t_test) %>% 
  unnest( t_test_sum)



df <- gift_and_talented %>% 
  mutate( x = ist_raw-x0) %>% 
  pivot_longer( -c(cohort,x)) %>% 
  mutate( hoy = ifelse(
    name %in% c("ist_raw", "cito_raw", "x0"), T, F)) %>% 
  group_by( x, name, hoy) %>%
  summarise( snitt = mean(value)) %>% 
  arrange( name,x) %>% 
  ungroup() %>% 
  mutate( hoy = factor(hoy , levels = c("TRUE", "FALSE") ) )

df %>% 
  ggplot( aes( y = snitt, x = x, group = name)) +
  geom_line( alpha = 0.5) +
  geom_point( alpha = 0.5) +
  geom_smooth( data = df %>% filter( x < 0), method = "lm" , aes( y = snitt, x = x, fill = name, color = name), se = F) +
  geom_smooth( data = df %>% filter( x > 0), method = "lm" , aes( y = snitt, x = x, fill = name, color = name), se = F) +
  facet_wrap( ~hoy, ncol = 1, scales = "free")


## Density check for the running variable 
gift_and_talented %>% 
  mutate( x = ist_raw - x0) %>% 
  ggplot( aes( x = x)) +
  geom_density() +
  geom_vline( xintercept = 0, color = "red")


## Regression discontinuously  

# d) how eligibility affects program participation

# Endogenous regressor: Treatment
# Instrument cutoff value X0 for treatment

first_stage <- gift_and_talented %>% 
  mutate( x = (ist_raw - x0),
          z =  ifelse( ist_raw > x0, 1, 0) ) %>% 
  lm( treat ~ x + I(x^2) +  z  + I(factor(male)) + age + cito_raw + I(factor(cohort)) , data = .) 

first_stage %>% stargazer::stargazer( type = "text") 

# Graphical presentation
gift_and_talented$fitted_fs <- fitted( first_stage)

df_snitt_fitted_fs <- gift_and_talented %>% 
  mutate( x = ist_raw - x0) %>% 
  group_by(x) %>% 
  summarise( fitted = mean(fitted_fs) ) 

df_snitt_fitted_fs %>% 
  ggplot( aes( x = x, y = fitted)) + 
  geom_point() +
  geom_smooth( data = df_snitt_fitted_fs %>% filter( x < 0),
               aes(x = x, y = fitted), method = "lm") +
  geom_smooth( data = df_snitt_fitted_fs %>% filter( x > 0),
               aes(x = x, y = fitted), method = "lm") +
  theme_bw() +
  labs( y = "fitted first stage Treatment linear",
        x = "x = runningvariable (below and above cutoff value)")


gift_and_talented %>% 
  mutate( x = ist_raw - x0) %>% 
  ggplot( aes( y = fitted_fs, x = x, color =  factor(treat) )) + geom_point(alpha = 0.5) +
  geom_smooth( data = df_snitt_fitted_fs %>% filter( x < 0),
               aes(x = x, y = fitted),  inherit.aes = F) +
  geom_smooth( data = df_snitt_fitted_fs %>% filter( x > 0),
               aes(x = x, y = fitted),  inherit.aes = F) +
  theme_light( base_size = 14)
  

# e) the reduce form of the effect eligibility on math performance

math_effcet_rf <- gift_and_talented %>% 
  mutate( x = ist_raw - x0,
          z =  ifelse( ist_raw > x0, 1, 0) 
          ) %>% 
  lm( reswis_raw ~ x + I(x*x) + z  + I(factor(male)) + age + cito_raw + I(factor(cohort)), data = .) 

math_effcet_rf %>% summary()


# f) Second stage
second_stage <- gift_and_talented %>% 
  mutate( x = ist_raw - x0,
          fitted = fitted(first_stage)
  ) %>% 
  lm( reswis_raw ~ x + I(x*x) + I(fitted*x) + fitted +I(factor(male)) + age + cito_raw + I(factor(cohort)), data = .   )

second_stage %>% summary()

# g) How sensitive are

gift_and_talented_w_fs <- gift_and_talented %>% 
  mutate( x = ist_raw - x0,
          fitted = fitted(first_stage)
  )

tbl_models <- tibble( 
  interval = c(3,7,8,15),
  data = list(gift_and_talented_w_fs,gift_and_talented_w_fs,gift_and_talented_w_fs,gift_and_talented_w_fs)
  ) %>% 
  mutate( data = map2( interval, data, function(int,dat) { dat %>% filter( near( x,0, tol = int )  )} ),
          model = map( data, function(data) { data %>%
              lm( reswis_raw ~ x + I(x*x) + I(fitted*x) + fitted +I(factor(male)) + age + cito_raw + I(factor(cohort)), data = .   ) %>% 
              broom::tidy()
              }) )

tbl_models$model[[1]]

tbl_models %>% 
  mutate( m = map(model, ~.x %>% filter( term == "fitted")) ) %>% 
  unnest( m)













