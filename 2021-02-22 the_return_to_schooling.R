

# 
library(tidyverse)
library(Ecdat)

data(Schooling, package = "Ecdat")

?Schooling

Schooling <- as_tibble(Schooling)

# Table 1
Schooling %>% lm( lwage76 ~ ed76 + exp76  + I(exp76^2) + black + smsa76 + south76, data = .) %>% summary()