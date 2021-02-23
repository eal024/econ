

# 
library(tidyverse)
library(Ecdat)

data(Schooling, package = "Ecdat")

?Schooling

Schooling <- as_tibble(Schooling)

# Table 1
Schooling %>% lm( lwage76 ~ ed76 + exp76  + I(exp76^2) + black + smsa76 + south76, data = .) %>% summary()

# Interpretation of schoolings effect on wage
100*(exp(0.0074)-1)

# If schooling is endogenous 
Schooling %>% names()
Schooling %>% lm( ed76  ~ age76 + I(age76^2) + black + smsa76 + south76 + nearc4 , data =. ) %>% summary()


# Using IV;
Schooling %>% estimatr::iv_robust( lwage76 ~  exp76  + I(exp76^2) + black + smsa76 + south76 + ed76 | nearc4 +  age76 + I(age76^2) + black + smsa76 + south76 , data = .) %>% summary()

