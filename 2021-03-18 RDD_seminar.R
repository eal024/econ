

# packages and data
library(tidyverse)
library(rdrobust) # Used in exercise 1- stata equval. package
theme_set( theme_light( base_size = 14))

# Exercise 1 --------------------------------------------------------------

# Data used
data("rdrobust_RDsenate")

# Expl. and running variable: Margin above 0 Democrats win  if  vote > 0.   
# Depending variable: The Democratic vote share in the following election (6 years after)

rdrobust_RDsenate <- as_tibble(rdrobust_RDsenate)


rdrobust_RDsenate %>% 
  ggplot( aes( x = margin, y = vote)) + 
  geom_point( alpha = .5) +
  geom_smooth( data = rdrobust_RDsenate %>% filter( margin < 0), aes( margin, vote), method = "lm", se = F, color ="red",   size = 1.5) + 
  geom_smooth( data = rdrobust_RDsenate %>% filter( margin > 0), aes( margin, vote), method = "lm", se = F, color = "blue", size = 1.5) +
  geom_smooth( data = rdrobust_RDsenate %>% filter( margin < 0), aes( margin, vote),    se = F, color ="red",               size = 1.5) + 
  geom_smooth( data = rdrobust_RDsenate %>% filter( margin > 0), aes( margin, vote),  se = F, color = "red")

# Alternative calculation with means
as_tibble(rdrobust_RDsenate) %>% mutate(margin = round(margin)) %>% group_by( margin) %>% summarise( vote = mean(vote)) %>% 
  ggplot( aes(x = margin, y = vote)) + 
  geom_point( ) +
  geom_smooth( data = as_tibble(rdrobust_RDsenate) %>% filter( margin < 0), aes( margin, vote), method = "lm", se = F, color ="red") + 
  geom_smooth( data = as_tibble(rdrobust_RDsenate) %>% filter( margin > 0), aes( margin, vote), method = "lm", se = F, color = "yellow") +
  geom_smooth( data = as_tibble(rdrobust_RDsenate) %>% filter( margin < 0), aes( margin, vote),    se = F, color ="red", linetype = 2) + 
  geom_smooth( data = as_tibble(rdrobust_RDsenate) %>% filter( margin > 0), aes( margin, vote),  se = F, color = "red" , linetype = 2) +
  theme_bw()


rdrobust_RDsenate %>% 
  mutate( margin_int = round( margin, 1)  ) %>% 
  group_by( margin_int) %>% 
  summarise( vote = mean(vote, na.rm = T)) %>% 
  drop_na() -> df 

df %>%   
  ggplot( aes( x = margin_int, y = vote)) +
  geom_point() +
  geom_smooth( data = df %>% filter( margin_int < 0), aes( margin_int, vote), method = "lm", se = F, color ="red",   size = 1.5) + 
  geom_smooth( data = df %>% filter( margin_int > 0), aes( margin_int, vote), method = "lm", se = F, color = "blue", size = 1.5) 
  


# a)  Test of packages
rdrobust( y = rdrobust_RDsenate$vote, x = rdrobust_RDsenate$margin, kernel = "uniform")
rdrobust( y = rdrobust_RDsenate$vote, x = rdrobust_RDsenate$margin, kernel = "uniform") %>% summary()


## OLS
rdrobust_RDsenate %>% 
  mutate( D = ifelse( margin > 0, 1, 0) ) %>%
  lm( vote ~ margin + D + I(D*margin), data = .)

rdrobust_RDsenate %>% 
  mutate( D = ifelse( margin >= 0, 1, 0) ) %>%
  lm( vote ~ margin  + D + I(margin^2)  + D + I(D*I(margin^2)) + I(D*margin) + I(D*I(margin^3)) , data = .) %>% summary( )


rdrobust_RDsenate %>% 
  filter( between(margin, -3, 3)) %>% 
  mutate( D = ifelse( margin >= 0, 1, 0) ) %>%
  lm( vote ~ margin + I(margin^2)  + D + I(D*I(margin^2)) + I(D*margin), data = .) %>% summary( )


#
rdplot(y = rdrobust_RDsenate$vote, x = rdrobust_RDsenate$margin)


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

# a) Descriptive data: The main thing, is to check differences between treated and non-treated. Can we compare them? naiv mean-mean wont work, big differences between groups.
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
  pivot_wider( names_from = treat, values_from = value) %>% 
  janitor::clean_names() %>%
  mutate( delta = x1-x0) %>% head(20)

#gift_and_talented  %>% pivot_longer( -c(cohort, treat)) %>% ggplot( aes(x = value, fill = factor(treat), color = factor(treat))) + geom_histogram() + facet_grid( cohort ~name, scales = "free") 

gift_and_talented %>% mutate( x_s = ist_raw - x0 ) %>% group_by( cohort, treat) %>% summarise_all( function(x) mean(x,na.rm = T))
  
# ii) always-takers, never-takers
gift_and_talented %>% 
  # each cohort has the same x0
  group_by(cohort) %>% 
  mutate( x = ist_raw - x0) %>% 
  group_by( z = ifelse(x >= 0, 1, 0 ), treat) %>% 
  count() %>% 
  pivot_wider( names_from = treat, values_from = n)


## iii) distribution of dependent variables
gift_and_talented %>% 
  ggplot( aes(x = reswis_raw , fill = factor(treat))) + geom_density( ) + facet_wrap( ~cohort, scales = "free" ) 

## iv) Try mean-diff in small range from cutoff
gift_and_talented %>%
  mutate(  x = ist_raw - x0) %>% 
  filter( between(cohort, 1998, 2000)) %>% 
  pivot_longer( -c(cohort, treat)) %>% 
  group_by( cohort, name) %>% 
  mutate( value = value/max(value,na.rm = T) ) %>% 
  drop_na() %>% 
  group_by( cohort, treat) %>% 
  ggplot( aes( y = value, x = fct_reorder(name, -value), fill = factor(treat), color = factor(treat) ) ) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap( ~ cohort, scales = "free", ncol =  1) +theme( legend.position =  "bottom")


# b) Estimate the Treatment effect from OLS

# OLS hole sample
gifted_ols <- gift_and_talented %>% lm( reswis_raw ~ treat, data = .)
gifted_ols_re <- gift_and_talented %>% filter(x < 0) %>% lm( reswis_raw ~ treat, data = .)

# OLS 
gifted_ols_c <- gift_and_talented %>% lm( reswis_raw ~ treat + male +  age + I(cohort) , data = .)

# sample below
gifted_ols_c_below <- gift_and_talented %>% mutate( x = ist_raw - x0) %>% 
  filter( x < 0) %>% 
  lm( reswis_raw ~ treat + male +  age + I(cohort) , data = .)

# Above
gifted_ols_c_above_c <- gift_and_talented %>% mutate( x = ist_raw - x0) %>% 
  filter( x > 0) %>% 
  lm( reswis_raw ~ treat + male +  age + I(cohort) , data = .)

gifted_ols_above <- gift_and_talented %>% mutate( x = ist_raw - x0) %>% 
  filter( x > 0) %>% 
  lm( reswis_raw ~ treat , data = .) 

# In certain range for the running variable
gifted_ols_range_2 <- gift_and_talented %>% mutate( x = ist_raw - x0) %>% 
  filter( between(x, -1.9999, 1.9999)) %>% 
  lm( reswis_raw ~ treat + male +  age + I(cohort) , data = .)


library(sandwich)
rob_se <- list(sqrt(diag( vcovHC(gifted_ols, type = "HC1") )),
               sqrt(diag( vcovHC(gifted_ols_c, type = "HC1") )),
               sqrt(diag( vcovHC(gifted_ols_re, type = "HC1") ) ),
               sqrt(diag(vcovHC(gifted_ols_c_below, type = "HC1") ) ),
               sqrt(diag(vcovHC(gifted_ols_above, type = "HC1") ) ),
               sqrt(diag(vcovHC(gifted_ols_c_above_c, type = "HC1"))),
               sqrt(diag(vcovHC(gifted_ols_c_above_c, type = "HC1")))
)


# Result for the models 
stargazer::stargazer( list(gifted_ols, gifted_ols_c, gifted_ols_re, gifted_ols_c_below, gifted_ols_above,gifted_ols_c_above_c, gifted_ols_range_2 ) ,
                      type = "text", column.labels = c("ols", "ols c", "(ols re)", "(ols re. c) ", "above", "above c", "range2"),
                      se = rob_se, notes = "S.e robust")



# c) 
gift_and_talented_balance <- gift_and_talented %>%
  mutate(  x = ist_raw - x0) 

balanced_nested <- tibble( interval = c(1,2,5,10),
                           data =  list(gift_and_talented_balance, gift_and_talented_balance,gift_and_talented_balance,gift_and_talented_balance) ) %>% 
  # Filter variable x, near given by interval 2 , +-2 from 0
  mutate( data = map2( interval, data ,function(inter , data) { data %>% filter( near(x,0, tol =inter )   )} )) 


var <- c("")

# t-test on each data
balanced_nested_t_test <- balanced_nested %>%
  #filter( interval == 1) %>%
  mutate( t_test = map(data, function(x){
    df <- as.data.frame(x) 
    t_test <- lapply(var, function(v){ t.test( df[, v] ~ df[,"treat"]) } ) 
+    t_test
    
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
  geom_text( aes(label = ifelse(x == 0, name, " "), x =-50), size = 6) +
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
          z =  ifelse( ist_raw >= x0, 1, 0) ) %>% 
  lm( treat ~ z +  x + I(factor(male) ) + age + cito_raw + I(factor(cohort)) , data = .) 

first_stage %>% stargazer::stargazer( type = "text") 

# Fitted from 1.firste stage
gift_and_talented$fitted_fs <- fitted( first_stage)

df_snitt_fitted_fs <- gift_and_talented %>% 
  mutate( x = ist_raw - x0) %>% 
  group_by(x) %>% 
  summarise( fitted = mean(fitted_fs) ) 

rf_model <- gift_and_talented %>% mutate(z = ifelse(x >= 0, 1, 0 ) ) %>% lm( reswis_raw ~ z + x + age + male + cito_raw + I( factor(cohort) ), data = .)

stargazer::stargazer( rf_model, type = "text")

second_div_first_stage <- 0.364/0.587


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




# f) Using IV
gift_and_talented %>% mutate(z = ifelse(x >=0,1, 0) )  %>%
  estimatr::iv_robust( reswis_raw ~ x + age + male + cito_raw + I(factor(cohort) ) + treat| z + x + age + male + I(factor(cohort)) + cito_raw , data = .) %>% 
  summary()


# g) How sensitive are
# 
# gift_and_talented_w_fs <- gift_and_talented %>% 
#   mutate( x = ist_raw - x0,
#           fitted = fitted(first_stage)
#   )
# 
# tbl_models <- tibble( 
#   interval = c(3,7,8,15),
#   data = list(gift_and_talented_w_fs,gift_and_talented_w_fs,gift_and_talented_w_fs,gift_and_talented_w_fs)
#   ) %>% 
#   mutate( data = map2( interval, data, function(int,dat) { dat %>% filter( near( x,0, tol = int )  )} ),
#           model = map( data, function(data) { data %>%
#               lm( reswis_raw ~ x + I(x*x) + I(fitted*x) + fitted +I(factor(male)) + age + cito_raw + I(factor(cohort)), data = .   ) %>% 
#               broom::tidy()
#               }) )
# 
# tbl_models$model[[1]]
# 
# tbl_models %>% 
#   mutate( m = map(model, ~.x %>% filter( term == "fitted")) ) %>% 
#   unnest( m)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
