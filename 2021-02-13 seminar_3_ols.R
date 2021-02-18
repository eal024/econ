

library(tidyverse)

mus <- haven::read_dta("data/mus03.dta")

# income ~ education + log_medical_expenditure. Control = gender, private insurance

str(mus) 
attr(mus$year03, "label")

tibble( var = names(mus), label =  map( mus, ~ attr(.x, "label"))) %>% unnest(label) %>% head(20)


# 1) Model 1: log-linear
model1 <- mus %>% 
  filter( totexp != 0) %>%
  lm( I(log(totexp)) ~ income  + female + private , data = .)

# 2) Education, not income
model2 <- mus %>% 
  filter( totexp != 0) %>%
  lm( I(log(totexp)) ~ educyr  + female + private , data = .)

# 3) Education and income
model3 <- mus %>% 
  filter( totexp != 0) %>%
  lm( I(log(totexp)) ~ income +  educyr  + female + private , data = .)


# Models
stargazer::stargazer(model1, model2, model3 , type = "text") 

# Interpretation: 
  # 1% income, decrease expenditure on medical by 0,03%*expend.on medical
    # Significant at alfa = 0.05 - level, when controlling for education.
  # Year education, increase expenditure with 3.2¤ of tot.expenditure.
  # Female is cant conclude, private insured uses more. 
  
# iii) Specification test
library(data.table)
mus_dt <- data.table::as.data.table(mus)[ totexp != 0,,][ ,y2_hat := (fitted(model3))^2,][]

model_RESET <- mus_dt %>% lm( I(log(totexp)) ~ income +  educyr  + female + private + y2_hat, data = .)

summary(model_RESET)


stargazer::stargazer(model_RESET, type = "text")

# Cant reject H0. result in favor the model. 
var.test(model3, model_RESET)

# c) 
model4 <- mus %>% 
  filter( totexp != 0) %>%
  mutate( educyr_2 = educyr^2
          ) %>%
  lm( I(log(totexp)) ~ income +  educyr + educyr_2  + female + private , data = .)

stargazer::stargazer(model3, model4, type = "text")

# The marginal-effect at year education mean
summary(model3)$coefficients[3,1]
summary(model4)$coefficients[3,1] + summary(model4)$coefficients[4,1]*mean(mus_dt$educyr) 
# Model4 has lower marginal effect of education.








