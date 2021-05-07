

# Panel Data

library(tidyverse)


# repeated observation for the same units
# - Robust for certain types of omitted variables bias
# - Learn about dynamics

## Topics random sampling (in the cross-section)
## balanced panel (Ti = T)
## large N, small T


# Example: fatality rate and beertax --------------------------------------


df_beer_accid <- haven::read_dta("data/beertax.dta") 

# Fatalities per 1000 population
df_beer_accid %>%
  filter( state < 10) %>% 
  ggplot( aes( y = fatalityrate, x = (year) ) ) +
  geom_line( ) +
  facet_wrap(~state, scales = "free_y")

# Will increase beer tax reduce fatality rate from car accidents?
df_beer_accid %>% lm( fatalityrate ~beertax, data = .) %>% summary()

df_beer_accid %>% filter(year == 1982) %>% lm( fatalityrate ~beertax, data = .) %>% summary()
df_beer_accid %>% filter(year == 1988) %>% lm( fatalityrate ~beertax, data = .) %>% summary()

# Beertax seems to be correlated with fatalities. Seems to be wrong. 
# Contrary of what we expected. Beertax should decrease deaths
# This may be explained by OVB 
# Use of Multivar OLS -> can not account for unobservables 


# First difference 
# First diff: Difference between year 1988 and 1982
# The difference for each state, between 1988 and 1982
# Change in the taxrate (+), decrease the fatality-rate. As expeceded.
df_beer_accid_diff <- 
  df_beer_accid %>% 
  filter( year %in% c(1982, 1988) ) %>% 
  select( state, year, fatalityrate, beertax ) %>% 
  group_by(state) %>% 
  mutate( d.fatalityrate = fatalityrate- lag(fatalityrate),
          d.beertax = beertax - lag(beertax)
  ) %>% 
  filter( year == 1988)  

# Increase beertax with 1 decrease rate (per 10000) by 1.04 unit
fatal_diff_mod <- df_beer_accid_diff %>% lm( d.fatalityrate ~d.beertax, data = .)
summary(fatal_diff_mod)



# This is a rater big impact: mean is 2 per 10000
mean(df_beer_accid$fatalityrate)

# Coefftest: d.beertax robust st. significant-
lmtest::coeftest(fatal_diff_mod, vcov = sandwich::vcovHC, type = "HC1")

## The within model:




# Between and within variation --------------------------------------------



# Data per state
df_beer_accid %>% 
  select(state, year, fatalityrate, beertax)

# Higher tax-rate, higher beertax: positive correlation?
df_beer_accid %>% 
  select(state, year, frate = fatalityrate, beertax) %>% 
  filter( year %in% c(1982, 1988)) %>%  
  ggplot( aes(y = frate, x = beertax ) ) +
  geom_point( ) +
  geom_smooth( method = "lm" , se = F, color = "blue") +
  facet_wrap( ~year, scales ="free_y")



df <- 
  df_beer_accid %>% 
  select(state, year, frate = fatalityrate, beertax) %>% 
  group_by( state) %>% 
  mutate( mean_beertax = mean(beertax)) %>% 
  ungroup() %>% 
  mutate( cat = case_when( mean_beertax > 0.75 ~ "Large",
                           between(mean_beertax, 0.5, 0.75) ~ "M",
                           T~ "small" 
                           ) )
df %>%
  ggplot( aes( x = beertax, y = frate )) + geom_point() + geom_smooth( method = "lm", se = F) +
  geom_smooth( data = df %>% filter( cat == "Large"), aes(y = frate, x = beertax), method = "lm", se = F, inherit.aes = F ) +
  geom_smooth( data = df %>% filter( cat  == "small"), aes(y = frate, x = beertax), method = "lm", se = F, inherit.aes = F , color = "red")
    


# Fixed effects model ----------------------------------------------------

# 1) First difference

#  The cost from 1.st diff
df_sim <- tibble(  id =  rep(seq( from = 1, to = 1000), each =2) , t = rep( x = c(1,2), times = 1000) ) %>% 
  mutate( 
    x = runif(n = 2000),
    x = ifelse( t == 1, 0.8*t + rnorm(n  =2000,  mean = 0, sd = 1), x),
    y = 2*x + rnorm(n = 2000, 0 ,1)
          ) %>% 
  group_by( id) %>% 
  mutate( d.y = y - lag(y),
          d.x = x - lag(x)
          ) 


# Cost in form of decreased t
df_sim %>% lm( y ~ x, data = .)  %>% summary()
df_sim %>% na.omit() %>% lm( d.y ~ d.x  , data = .) %>% summary()



## example 2: Accidents and Beers
# OLS: Positive relation between beertax and fatality-rate. We expect the relation to be neg.
df_beer_accid %>% 
  filter( year %in% c(1982, 1988) ) %>%
  lm( fatalityrate ~ beertax, data = .) %>%
  summary()


# First diff: Difference between year 1988 and 1982
# The difference for each state, between 1988 and 1982
# Change in the taxrate (+), decrease the fatality-rate. As expeceded.
df_beer_accid_diff <- 
  df_beer_accid %>% 
  filter( year %in% c(1982, 1988) ) %>% 
  select( state, year, fatalityrate, beertax ) %>% 
  group_by(state) %>% 
  mutate( d.fatalityrate = fatalityrate- lag(fatalityrate),
          d.beertax = beertax - lag(beertax)
          ) %>% 
  filter( year == 1988)  

df_beer_accid_diff %>% lm(  d.fatalityrate ~ 0 + d.beertax , data = .  ) %>% summary( ) 
# The result -0.87 per 10000 people. 

# Graphical presentation
df_beer_accid_diff %>% ggplot( aes(y = d.fatalityrate, x = d.beertax) ) + geom_point() +
  geom_smooth( method = "lm", se = F)



# The between Estimator ---------------------------------------------------

# Between estimator: If RE-assumption holds: E[a|x] = 0 => (cov(a,x) = 0):

model_between01 <- df_beer_accid %>% 
  select( state, year, fatalityrate, beertax) %>% 
  group_by( state) %>% 
  mutate( m.fatalityrate = mean( fatalityrate),
          m.beertax = mean(beertax) 
          ) %>% 
  filter( year == 1988) %>% 
  lm( m.fatalityrate ~ m.beertax, data = .)

# Overall R-sq  
model_between01 %>%  summary()

model_between02 <- plm::plm( fatalityrate ~beertax, data = df_beer_accid, model = "between"  )

model_between02 %>% summary()

ercomp( model_between02)


tibble( predicted = fitted(model_between01), df_beer_accid %>% group_by(state) %>% summarise( m_fatal = mean(fatalityrate))) %>% 
  mutate( a = (predicted - m_fatal)^2/48 ) %>% summarise( a = sum(a)^2)

# Between variation R2: in Within
summary(model_between01)$r.squared %>% format( digits = 4)

# Within-state variance
plm::plm( fatalityrate ~beertax, data = df_beer_accid, model = "within" ) %>% summary()


# The fixed effects model -------------------------------------------------

# 
fe_model <- df_beer_accid %>% 
  select( state, year, fatalityrate, beertax) %>% 
  group_by(state) %>% 
  mutate( m.fatalityrate = mean(fatalityrate),
          m.beertax = mean(beertax)
          ) %>% 
  ungroup( ) %>% 
  mutate( yhat =  fatalityrate - m.fatalityrate,
           xhat =  beertax -      m.beertax 
           ) %>% 
  lm( yhat  ~0 +xhat, data = .)

summary(fe_model)

# Get correct t-value from the plm-function.
lmtest::coeftest( fe_model, vcov = sandwich::vcovHC, type = "HC1")
lmtest::coeftest( plm::plm(fatalityrate ~beertax, model = "within",data = df_beer_accid), vcov = sandwich::vcovHC, type = "HC1")

tibble(a = residuals(fe_model), x = df_beer_accid$beertax  ) %>% 
  summarise( cor = cor(a,x)  )

# Least Square Dummy variables : LSDV
df_beer_accid %>% 
  lm( fatalityrate ~ beertax + factor(state), data = .) %>% 
  summary() %>% 
  broom::tidy() %>% 
  filter( ! str_detect(term, "fact") )


model_within <- plm::plm( data = df_beer_accid, formula = fatalityrate ~ beertax ,model = "within" )

summary(model_within )
 
#
model_fe <- df_beer_accid %>% lm( fatalityrate ~ beertax + factor(state), data = .)

summary(model_fe)


# 
library(plm)
model_random <- plm( dat = df_beer_accid, formula = fatalityrate ~ beertax, model = "random")

model_random %>% summary( ) # 



# Hausman test ------------------------------------------------------------

plm::phtest( model_within_plm, model_random )




# Cluster robust standard errors ------------------------------------------

rob_se <- list( sqrt(diag(sandwich::vcovHC( model_within, type = "HC2"))),
                sqrt(diag(sandwich::vcovHC( model_random, type = "HC2")))
                )


stargazer::stargazer( model_fe, 
                      model_random,
                      digits = 3,
                      se = rob_se,
                      type = "text",
                      keep = c("beertax")
                      )








m_ols <- df_beer_accid %>% 
  lm( fatalityrate ~ beertax, data = .)

N <- unique(df_beer_accid$state) %>% length()
time <- unique(df_beer_accid$year) %>% length()

tibble( yhat = predict(m_ols), y = df_beer_accid$fatalityrate ) %>% 
  mutate( e_2 = (y-yhat)^2 ) %>% 
  summarise( sigma_a_simgan_e = (sum(e_2)/(N*time-2))^.5 )

0.03605+0.26604


model_re <- df_beer_accid %>% plm::plm( data = ., formula = fatalityrate ~ beertax , model = "random")

summary(model_re)














