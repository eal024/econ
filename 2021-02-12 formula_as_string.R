

# Passing formula to lm()

library(tidyverse)

var <- mtcars %>% names()

outcome <- var[1]
c_var <- var[2:length(var)]

f <- as.formula( paste( outcome , paste(c_var, collapse = " +"), sep = "~") )

lm( formula =  f, data = mtcars)

model <- lm( formula =  f, data = mtcars)

format(terms(model))

lm( formula =  format(terms(model)), data = mtcars)

