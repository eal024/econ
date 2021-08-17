

# Data
library(tidyverse)
lalonde_expr <- haven::read_dta("data/lalonde.dta") %>% 
  # Keep the experiment data
  filter( sample == 3)

lalonde_expr %>% count()

lalonde_expr %>% group_by( sample) %>% 
  summarise_if( is.numeric, list( mean = ~mean(.x), 
                                  sd = ~sd(.x),
                                  antall = ~n())) %>% 
  pivot_longer( -sample, "key", "values") %>% 
  head(20)

# a) model 1 linear-linear

outcome <- "earnings78"
var_c <- paste("age" , "married","black" ,"education", "earnings75", "hispanic","nodegree" , sep = " +")

f <- as.formula( paste( outcome, var_c, sep = " ~"))

model1 <- lalonde_expr %>% 
  lm( formula = f, data = .)

summary(model1)

# Regression(estimation) -> Residuals or Error or Explained
# TSS: Total sum of squares Sigma(y-y_hat)² -> total variation
# ESS: Explained sum of squares (y_hat - y_mean)²   -> var. from the estimation.
# SSR: Sum of sq. residuals (y-y_hat)² (variation in residuals)

# R2 = 1- SSR/TSS

mean( lalonde_expr$earnings78)
lalonde_expr %>% lm( earnings78 ~ 1, data = .)

summary(lalonde_expr %>% lm( earnings78 ~ 1, data = .))
# Confidence intervals
lalonde_expr %>% summarise( m = mean(earnings78),
                            se = sd(earnings78)/sqrt(n()),
                            low  = m-se,
                            heigh = m+se
                            )

# b) The log-linear model:
log_outcome <- "I(log(earnings78))" 

mode2_log_linaer <- lalonde_expr %>% 
  filter( earnings78 != 0) %>% 
  mutate( log_re78 = log(earnings78)) %>%
  lm( formula =  paste( "log_re78", var_c, sep = "~")) 

summary(mode2_log_linaer)

# Interpretations: y changes y*Beta = d(yn/y0) = [exp(B)]-1

# c) Breusch-Pagan test

lalonde_expr$e_2 <- ((model1$residuals)^2)

e_2 <-  "e_2"

BP_model <- lalonde_expr %>% 
  lm( formula =  paste(e_2, var_c, sep = " ~"))

summary(BP_model)
# Test H0: e2 = h(ax), a == 0, a != 0
# Large R2 implies presents of hetroskedasticity
summary(BP_model)$r.squared*nrow(lalonde_expr)

## BP-model log-linear
lalonde_expr_1 <- lalonde_expr %>% 
  filter( earnings78 != 0) %>% 
  mutate( e_2 = (mode2_log_linaer$residuals)^2)


BP_log_linear <- lalonde_expr_1 %>% lm( formula = paste( "e_2" , var_c, sep = "~"), data = .)

# Model: e2 ~ v. 
summary(BP_log_linear)
# Can reject that a == 0
summary(BP_model)$r.squared*nrow(lalonde_expr_1)

# Testing for Multiplicative heteroskedasticity
# σ_i_2 = σ_2exp{z_i * alfa},

# F-distribution (with J and N − J − 1
# Chi-distribution (J = df), J = equal number of X (-intercept)

lalonde_expr_1 <- lalonde_expr %>% 
  filter( earnings78 != 0) %>% 
  mutate( log_e_2 = log( (mode2_log_linaer$residuals)^2 ) )


m_hetro <- lalonde_expr_1 %>% lm( formula =  paste( "log_e_2", var_c, sep = "~" ), 
                                  data = .)

summary(m_hetro)$r.squared*nrow(m_hetro$model)
pchisq( summary(m_hetro)$r.squared*nrow(m_hetro$model) , df = length(var_c), lower.tail = F)


# FGLS --------------------------------------------------------------------


# Estimate eeighted form model1 linaer-linear

BP_log_linear <- lalonde_expr%>% lm( formula = paste( "I(log(e_2))" , var_c, sep = "~"), data = .)

summary(BP_log_linear)

lalonde_expr$h <- (exp(BP_log_linear$fitted.values))^.5

model1_w <- lalonde_expr %>%
  mutate( c = 1) %>% 
  select(-contains("e2|e_2"),-sample, - treatment) %>% 
  mutate_all(~.x/h ) %>% 
  lm( earnings78 ~ 0 + c + age + education + nodegree + married + hispanic + black + earnings75, data = .)

summary(model1)
summary(model1_w)

lalonde_expr$h <- exp(model_log_e2$fitted.values)^(0.5)

lalonde_expr %>% 
  lm( formula = f, data = ., weights = exp(model_log_e2$fitted.values)^.5) %>% 
  summary()

lalonde_expr %>%
  transmute( 
    c_h = 1/h,
    re78_h = earnings78/h,
    age_h = age/h,
    edu_h = education/h,
    nodegree_h = nodegree/h,
    black_h = black/h,
    hispanic_h = hispanic/h,
    re75 = earnings75/h,
    married_h = married/h
    ) %>% 
  lm( formula = re78_h ~0 +., data = .) %>%  
  summary()


# EGLS (FGLS)
egls_model <- lalonde_expr_1 %>%
  transmute( c_h = 1/log_h_0.5,
             age_h = age/log_h_0.5,
             edu_h = education/log_h_0.5,
             re75_h = earnings75/log_h_0.5,
             re78_h = (earnings78)/log_h_0.5,
             nod_h = nodegree/log_h_0.5,
             black_h = black/log_h_0.5,
             hispanic_h = hispanic/log_h_0.5,
             married_h = married/log_h_0.5
             ) %>% 
  lm(  re78_h ~ 0 +. , data = . )

summary(egls_model)
#


# Heteroskedasticity-consistant standard error ----------------------------

lalonde_expr %>% estimatr::lm_robust( formula = f, data = ., se_type = "HC2")
# Compare to homo:
summary(model1)







