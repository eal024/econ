

library(tidyverse)

# Belgium wage diff. between male and female

wage <- read_delim( "data/data_verbeek/bwages.dat", delim = "\t" ) %>% janitor::clean_names()

wage %>%
  group_by( factor(male) ) %>%
  summarise( across( .cols = everything() , .fns = list( mean = ~mean(.x, na.rm = T) , sd = ~sd(.x, na.rm = T))))
# Female has lower wage, but experience is higher among men



# Model 1 - linear model
model1 <- wage %>%
  lm( wage ~ male + educ + exper, data = . )

# Adjusting for educ and exper does not change the expected wage differential betw. males and females
model1 %>% summary()

## Heteroskedasticity
tibble( fitted = model1$fitted.values, resid = model1$residuals) %>%
  ggplot( aes(y = resid, x = fitted) ) + geom_point( alpha = 0.5)

# Model 2
model2 <- wage %>%
  mutate( log_wage = log(wage), log_educ = log(educ), log_exper = log(exper), log_exper_2 = log_exper^2) %>%
  lm( lnwage  ~ lneduc + lnexper + I(lnexper^2) +  male, data = . )

#
summary( model2)

# Hetero'
# While there appear to be some traces of heteroskedasticity still, the graph
# is much less pronounced than for the additive model

tibble( fitted = model2$fitted.values, resid = model2$residuals) %>%
  ggplot( aes(y = resid, x = fitted) ) + geom_point( alpha = 0.5)

## Test Two, one or non variables for experience.
model3 <- wage %>% lm( lnwage  ~ lneduc  +  male, data = . )

r2_unr <- summary(model2)$r.squared
r2_r <- summary(model3)$r.squared

# F-statistic for the restriction. H0 can be rejected
((r2_unr-r2_r)/2)/( (1-r2_unr)/(nrow(wage)-5))

summary(model2)
# Low t-value -> may imply that one can be dropped
model4 <- wage %>% lm( lnwage  ~ lneduc + lnexper  +  male, data = . )

# R2 decrease only with 0.01
summary(model4)

## Model 5 -> education
unique(wage$educ)
table(wage$educ)

# Alternativ
model5 <- wage %>% lm( lnwage  ~ I(factor(educ)) + lnexper  +  male, data = . )

summary(model5)

summary(model4)

# Should we use factor or countinoues values for education?
r2_unr <- summary(model5)$r.squared
r2_r <- summary(model4)$r.squared
((r2_unr-r2_r)/3)/( (1-r2_unr)/(nrow(wage)-5))

# Reject H0: educ -factor() -> implies use factor
# That is, specification 5 with educational dummies is a significant improvement over specification 4 with the log
# education level.

## Model 6: Effect of gender

# Until now the gender effect is linear. But it may be that men are rewared differently

model6 <- wage %>%
  lm( lnwage  ~ I(factor(educ)) + I(as.factor( educ*male ))  + lnexper + I(lnexper*male), data = . )

model6 %>% summary()
# Low diff. male*experience
# To calculate the effect of gender - we also need to include other variables were male occur











