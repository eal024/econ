


library(tidyverse)

dir("data")

lalonde_expr <- haven::read_dta("data/lalonde.dta") %>% 
  # Keep the experiment data
  filter( sample == 3)


# a) linear and log-linear models
model1 <- lalonde_expr %>% 
  lm( earnings78 ~ age + earnings75 + nodegree + education + married + black, data = .)
  
summary(model1)

model2_log_linear <-  lalonde_expr %>% 
  mutate( log_re78 = log(earnings78+0.01)) %>% 
  lm( log_re78 ~ age + earnings75 + nodegree + education + married + black, data = .)

summary(model2_log_linear)


# b) Breusch-Pagan test
lalonde_expr$linear_residual_2 <- (model1$residuals)^2

BP_test <- lalonde_expr %>% 
  lm( linear_residual_2 ~ age + earnings75 + nodegree + education + married + black,
      data = .)

# Cant reject H0: Hetro
summary(BP_test)$r.squared*nrow(lalonde_expr) 







