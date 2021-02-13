


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

## c) Assume multiplicative heteroskedasticity - related to all x`s -> Test for hetro
# 4.4.2 Testing for Multiplicative Heteroskedasticity

# Test for: σ²(i) = σ² exp{z`alfa},
# Ho: Alfa = 0, vs. H1: Alfa != 0
# Several test:
# simplest one is based on the standard F-test
# Another approximation is based on the asymptotic chi -distribution (with J degrees of freedom)

# 1) Breush_Pagan test 
# 2) The White test

# Test the log-linear model

summary(model2_log_linear)
model2_log_linear$residuals %>% head(6)

# Alternativ - the broom-packages
log_linear_auxiliary <- model2_log_linear %>%
  broom::augment() %>% 
  mutate( e2 = .resid^2) %>% 
  lm( I(log(e2)) ~ age + earnings75 + nodegree + education + married + black, data =.)

# Clear indication for hetroskedastecity. Can reject Alfa = 0. 
summary(log_linear_auxiliary)$r.squared*nrow(lalonde)


tibble( res = model2_log_linear$residuals, 
        fitted = model2_log_linear$fitted.values) %>% 
  ggplot( aes(y =  res, x = fitted,  )) + geom_point(alpha = 0.3)

# FLGS
# Transform the fitted value exp(:)^0.5
lalonde_expr$exp_fitted_e <- (exp(log_linear_auxiliary$fitted.values))^.5

model_egls <- lalonde_expr %>% 
  transmute( log_re78_e =    log(earnings78 + 0.001)/exp_fitted_e,
          age_e = age/exp_fitted_e,
          edu_e =     (education)/exp_fitted_e,
          re75_e =    (earnings75)/exp_fitted_e,
          nodgree_e = (nodegree)/exp_fitted_e,
          married_e = married/exp_fitted_e,
          black_e = black/exp_fitted_e,
          c_e = 1/exp_fitted_e) %>% 
  lm(log_re78_e  ~ 0 + c_e + age_e   + edu_e  + re75_e + nodgree_e + married_e + black_e  , data = .)

summary(model2_log_linear)
summary(model_egls)


# e) Hetrorobust standard error



model2_log_linear_robust <- lalonde_expr %>%
  mutate( log_re78 = log(earnings78 + 0.0001)) %>% 
  estimatr::lm_robust( formula =  log_re78 ~ age + earnings75 + nodegree + education + married + 
                                         black, data = .)


# Lalonde -robust standard error
lalonde_expr %>%
  mutate( log_re78 = log(earnings78 + 0.0001)) %>% 
  estimatr::lm_robust( log_re78 ~  age + earnings75 + nodegree + education + married + 
                         black, data = . )

summary( model2_log_linear)
summary(model2_log_linear_robust)
