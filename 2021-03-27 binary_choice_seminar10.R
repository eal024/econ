

library(tidyverse)

dir("data")

# Model the choice between fishing from a boat or from a pier.
# Data
mus15 <- haven::read_dta("data/mus15data.dta")

# a) 1. Descriptive info
tibble( name = names(mus15),
        label = map_chr(mus15, ~ .x %>%  attr("label")),
        mean = map_dbl( mus15, ~mean(.x, na.rm = T)),
        nr_na = map_dbl(mus15, ~sum(is.na(.x)))
)

mus15_1 <- mus15 %>% mutate( mode2 = case_when(
  mode == 1 ~ "beach",
  mode == 2 ~ "pier",
  mode == 3 ~ "private",
  T ~ "charter") %>%
    as_factor(),
  mode3 = ifelse( mode2 == "charter", 1, ifelse( mode2 == "pier", 0, 3)),
  relative_price = log(pcharter/ppier),
  x = log(pcharter/ppier)
  ) %>%
  relocate( mode2, mode3 ,.after = mode)

# Data filter
mus15_2 <- mus15_1 %>% filter( mode2 %in% c("charter", "pier") ) 
  
  
# a) 2: What you want to know:
# Catch rate, price and relative price-rate and may be:) income->
mus15_1 %>% group_by( mode2) %>% summarise_all(mean)



# b) Estimate the models: LPM, Logit and probit
lpm <- mus15_2 %>% lm( mode3 ~ x , data = .)
logit <- mus15_2 %>% glm( mode3 ~x, data = ., family = binomial(link = "logit"))
probit <- mus15_2 %>% glm( mode3 ~x, data = ., family = binomial(link = "probit")) 

# Models
stargazer::stargazer( lpm, logit, probit, type = "text")

df_pred <- tibble( x = seq(from = -1, to = 2.5, length.out = 15) ) 

# Predicted  
df_pred <- df_pred %>%
  mutate( 
    logit  = predict( logit , type = "response", newdata = df_pred),
    probit = predict( probit , type = "response", newdata = df_pred),
    lpm    = predict( lpm  ,  newdata = df_pred)
  )


mus15_2 <- mus15_2 %>% mutate( fitted_logit =  predict(logit, type = "response"),
                               fitted_probit = predict( probit, type = "response"),
                               lpm_fitted = predict( lpm )
                               ) 

# Graph
theme_set( theme_light( base_size = 12))



graph_1 <- mus15_2 %>% ggplot( aes( y = mode3, x = x)) +
  geom_point( position = position_jitter(height =  0.01), alpha = .1) +
  geom_line( data = df_pred, aes( y = lpm, x = x), color = "black")     +
  geom_line( data = df_pred, aes( y = logit, x = x), color = "blue" )   +
  geom_line( data = df_pred, aes( y = probit, x = x), color = "red", linetype = 2) +
  lims( x = c(-1,2.5), y = c(0,1))

graph_1
# b) Compare the slope for each model:
mus15_2 %>% summarise( across( .cols = c(mode3, fitted_logit, fitted_probit), mean) )

# 1) sign 
# 2) Scale g(0)
# 3) PEA
# 4) APE

# 3)
#LPM
lpm$coefficients[[2]]
# Logit
logit_at_mean <- predict(logit, type = "response", newdata = mus15_2 %>% dplyr::select(x) %>% summarise(x = mean(x)))
logit_at_mean*(1-logit_at_mean)*coef(logit)[[2]]
# Probit
dnorm( predict( probit, type = "response", newdata = mus15_2 %>% summarise( x = mean(x))))*coef(probit)[2]




# ii) Compare the predicted probability
mus15_2 %>% 
  dplyr::select( mode3, contains("fitted")) %>% 
  pivot_longer( cols = everything() ) %>% 
  group_by(name) %>% 
  summarise_all( list(mean = mean,sd = sd, min =min,max = max) )




## ii) Statistical significance (https://daviddalpiaz.github.io/appliedstats/logistic-regression.html#confidence-intervals-1)

# Wald Test (with Maximum likelihood): Z

# Test H0: Bx = 0; 
coef(summary(logit))

# Perform the test Likelihood-Ratio Test:  2( ℓ(^β_Full)−ℓ(^β_Null)) ~ Chi (df = )

LR_test <-  -2*as.numeric( logLik(logit) - logLik(  glm( mode3 ~1, data = mus15_2, family = binomial(link = "logit")) ) )
LR_test

# Degree of freedom = 1 (number of Xs)
## From the automatic generated table anova:
anova( logit, model_null, test = "LRT")
qchisq(p = (1-2.2e-16), df = 1)


# Alternative
model_null <- glm( mode3 ~1, data = mus15_2, family = binomial(link = "logit"))

anova( logit, model_null, test = "LRT")

# Confidence Intervals: Note that we could create intervals by rearranging the results of the Wald test to obtain the Wald confidence interval. This would be given by
confint( logit, level = 0.99)
summary(logit)

# Probit
LR_test_pro <-  -2*as.numeric( logLik(probit) - logLik(  glm( mode3 ~1, data = mus15_2, family = binomial(link = "probit")) ) )
abs(LR_test_pro) > qchisq( p = 0.99, df = 1)

model_probit_2 <- glm( mode3 ~1, data = mus15_2, family = binomial(link = "probit"))
anova( probit, model_probit_2, test = "LRT")

#LPM
lpm %>% summary()

# Wald
aod::wald.test( b = coef(logit), Sigma = vcov(logit), Terms = c(2))
aod::wald.test( b = coef(probit), Sigma = vcov(probit), Terms = c(2))


## iii) Compare models by predicted probabilites
df <- tibble( x = seq( from = 0.1, to = 2.9, by = 0.1) ) 

df <- df %>%   mutate( p_logit = predict( logit , type = "response", newdata = df),
                 p_probit = predict( probit , type = "response", newdata = df),
                 p_lpm = predict( lpm, newdata = df)
                 ) 
  
df %>% mutate( diff = (p_logit - p_lpm) ) %>% head(20) 



## iv) Loglikehood rate
logLik(logit)
logLik(probit)



# Partial effects ---------------------------------------------------------

# Logit 
Gx_m <- predict( logit,  newdata = mus15_2 %>% summarise(x = mean(x)), type = "response" )

x_m <- mean(mus15_2$x)
coef(logit)[2]
G <- exp( x_m*coef(logit)[2]+coef(logit)[1] ) / ( 1+ exp(x_m*coef(logit)[2]+coef(logit)[1]) )

G*(1-G)*coef(logit)[2]
Gx_m*(1-Gx_m)*coef(logit)[2]
# Packages:
margins::margins( mus15_2, model= logit , variables = "x", at = list(x = 0.2745))

margins::dydx(logit,data = mus15_2 , variable = "x") %>% as_tibble() %>% summarise( mean = mean(dydx_x))



# Probit
at_mean <- coef(probit)[[1]] + coef(probit)[[2]]*x_m

# Marginal effect
# By hand:
dnorm( at_mean)*coef(probit)[2] # -0.27

# By function
margins::margins( mus15_2, model= probit , variables = "x", at = list(x = 0.2745))

graph_1 + geom_vline( xintercept =  x_m) + coord_cartesian( x = c(x_m - 5*0.1, x_m + 5*0.1))


## Evaluate over the complete sample (Average partial effects) APEs
mus15_2 %>% 
  dplyr::select( mode3, contains("fitted")) %>% 
  pivot_longer( cols = everything() ) %>% 
  group_by(name) %>% 
  summarise_all( list(mean = mean,sd = sd, min =min,max = max) )


mus15_2 %>% mutate( pred_logit = predict( logit, type = "response")* (1-predict( logit, type = "response"))*coef(logit)[[2]],
                    pred_probit = dnorm( coef(probit)[[1]] + coef(probit)[[2]]*x)*coef(probit)[[2]],
                    pred_lpm = coef(lpm)[[2]]
                    ) %>% 
  dplyr::select( contains("pred")) %>% 
  summarise_all( mean) %>% 
  pivot_longer( cols = everything())


## Average over those that chose pier

G_logit <- predict( logit , newdata = mus15_2 %>% filter( mode2 == "pier"), type = "response")

mus15_2 %>%
  filter( mode2 == "pier" ) %>% 
  mutate( predlogit = G_logit *(1-G_logit )*coef(logit)[[2]],
          predprobit = dnorm( coef(probit)[[1]] + coef(probit)[[2]]*x)*coef(probit)[[2]],
          predlpm = coef(lpm)[[2]])  %>% 
  dplyr::select( contains("pred")) %>% 
  summarise_all( list( mean = mean, sd = sd, min = min, max = max, median = median)) %>% 
  pivot_longer( cols =  everything(),
                names_to = c("model", "stat"),
                names_sep = "_",
                values_to = "value"
                )  %>% 
  pivot_wider( names_from = stat, values_from = value)
  

## Choosing boat
mus15_2 %>% distinct(mode2)

G_logit_chart <- predict( logit , newdata = mus15_2 %>% filter( mode2 == "charter"), type = "response")

mus15_2 %>%
  filter( mode2 == "charter" ) %>% 
  mutate( pred_logit = G_logit_chart *(1-G_logit_chart )*coef(logit)[[2]],
          pred_probit = dnorm( coef(probit)[[1]] + coef(probit)[[2]]*x)*coef(probit)[[2]],
          pred_lpm = coef(lpm)[[2]])  %>% 
  dplyr::select( contains("pred")) %>% 
  summarise_all( mean)


# d)  ---------------------------------------------------------------------

# Create a new variable









# Appendix ----------------------------------------------------------------

round_fun <- function(x,digits) {round(x, digits = 2)}
a <- as_mapper(~round(.x, digits = 2) )
round_coef <- compose( a, coef)
round_coef(logit)  
