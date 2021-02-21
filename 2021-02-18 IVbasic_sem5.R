

#
library(tidyverse)

dir("data") 

angev98 <- haven::read_dta("data/angev98.dta")

tibble( n = names(angev98), label = map( angev98, ~attr(.x, which = "label"))) %>% 
  unnest( label) %>% 
  head(20)

# Labor: 
  # Earnings
  # Employment 
  # Hours worked

# Fertility
  # Number of children
  # Two kids
  # Children gender and age
  # And race


# a) workdedm (employement) ~ Morekids 
model1 <- angev98 %>%
  select( workedm, morekids) %>% 
  lm( workedm ~ morekids, data = .)

# Eqvivivalent to:
angev98 %>%
  select( workedm, morekids) %>% 
  group_by( morekids) %>% 
  summarise( se = sum(workedm)/n())

stargazer::stargazer( model1, type = "text")


# b) Check the correlation between morekids and ageqk
cor(angev98$morekids, angev98$ageqk)

cov(angev98$morekids, angev98$ageqk)

# c) Child with same sex:
model_morekids <- angev98 %>%
  mutate( m_2 = ifelse( kidcount > 2, 1, 0)) %>% 
  select( morekids, samesex, kidcount,  m_2  ) %>% 
  lm( morekids ~ samesex, data = .)

stargazer::stargazer( model_morekids, type = "text")

# e) validity
# More attention to this! Other alternatives?
angev98$first_stage_e <- residuals(model_morekids)
angev98 %>% lm( workedm ~ morekids + first_stage_e , data = .) %>% summary()

# f) Wald estimate

# i) b_wald <- sum(mean(angev98$workedm[angev98$samesex ==1]) - mean(angev98$workedm[angev98$samesex ==0]) )/( mean(angev98$morekids[angev98$samesex ==1]) - mean(angev98$morekids[angev98$samesex ==0]))

b_wald
# Check the result:
model_iv <- angev98 %>% estimatr::iv_robust( workedm ~ morekids | samesex, se_type = "stata",  data = ., )
summary(model_iv) 


# ii) Divide RF/1.stage

# 1.stage
first_stage <- angev98 %>% lm( morekids ~  samesex , data = .)
reduce_form <- angev98 %>% lm( workedm ~ samesex , data = .)

# Wald
coefficients(reduce_form)[2]/coefficients(first_stage)[2]

# iii) By hand
#a)
# 1)
angev98$more_kids_hat <- first_stage$fitted.values

# 2)
sls <- angev98 %>% lm( workedm ~ more_kids_hat, data = .)

summary(sls)

# 3) By R command:
model_iv <- angev98 %>% estimatr::iv_robust( workedm ~ morekids | samesex, se_type = "stata",  data = ., )
summary(model_iv)

# 4) Include controls
names( angev98)

angev98 %>% estimatr::iv_robust( workedm ~  agem + aged + blackm + hispm + blackd + hispd +boy1st + boy2nd +ageq2nd + ageqk  + morekids | samesex +agem + aged +blackm + hispm +blackd + hispd + boy1st + boy2nd +ageq2nd + ageqk , data  =., se_type = "stata")


# g) Hausman-test
angev98$first_stage_e <-  first_stage$residuals

husman <- angev98 %>% estimatr::lm_robust( workedm ~ morekids + first_stage_e, se_type = "stata" , data = .)

# Hard to concludes: t-value indicate no endogenous, but magnitude of B - is consistent
summary(husman)
# Weak instrument? No
summary(first_stage)

# 2) Hausman-test
#angev98 %>% select(samesex , aged , blackm , hispm , blackd , hispd , boy1st, boy2nd,ageq2nd,ageqk)
alt2_fist_stage <- angev98 %>% lm( morekids ~ samesex  +blackm + hispm +blackd + hispd + boy1st + boy2nd +ageq2nd + ageqk, data = .)
angev98$e_alt2_fist_stage <-  alt2_fist_stage$residuals

husman_2 <- angev98 %>% estimatr::lm_robust( workedm ~ morekids  +blackm + hispm +blackd + hispd + boy1st + boy2nd +ageq2nd + ageqk + e_alt2_fist_stage, se_type = "stata" , data = .)

# Hard to concludes: t-value indicate no endogenous, but magnitude of B - is consistent
summary(husman_2) # t-value abouve 2 = can reject that H0: 0 is True 



## h) 
# fist stage
alt_first_stage <- angev98 %>% lm( morekids ~  twins2 ,data = .) 
angev98 %>% estimatr::iv_robust( workedm ~ morekids | twins2, data = .)


angev98$e_alternativ <- alt_first_stage$residuals

angev98 %>% estimatr::lm_robust( workedm ~ morekids + e_alternativ , data = ., se_type = "stata")


# i) Sargan-test; 
# 1) 

angev98$first_alt_x1 <- alt2_fist_stage$fitted.values
sls <- angev98 %>% lm( workedm ~ first_alt_x1 +blackm + hispm +blackd + hispd + boy1st + boy2nd +ageq2nd + ageqk , data = .) 
angev98$e_hat <- sls$residuals
sargan_lm <- angev98 %>% lm( e_hat ~ samesex + twins2 + morekids +blackm + hispm +blackd + hispd + boy1st + boy2nd +ageq2nd + ageqk, data = .) 
summary(sargan_lm)

pchisq(summary(sargan_lm)$r.squared*nrow(angev98), lower.tail = F, df = 1) 



# j) Married woman or not married - the effect from more kids:

f <- as.formula( workedm ~ morekids +blackm + hispm +blackd + hispd + boy1st + boy2nd +ageq2nd + ageqk | samesex +blackm + hispm +blackd + hispd + boy1st + boy2nd +ageq2nd + ageqk)

msample_models <-  angev98 %>% 
  group_by(msample) %>% 
  nest( ) %>% 
  mutate( model = map( data, function(x ) { x %>% estimatr::iv_robust( formula = f, data = ., se_type = "stata")} ))

msample_models$model[[1]]
msample_models$model[[2]]


## Hausman. Test Z2: Twins2

second_stage_hh <- angev98 %>% 
  mutate( yhat_alt = fitted( alt2_fist_stage) ) %>% 
  lm( workedm ~ yhat_alt  + blackm + hispm +blackd + hispd + boy1st + boy2nd +ageq2nd + ageqk, data = .) 

angev98$u_hausman <- second_stage_hh$residuals

names(angev98)
hausman <- angev98 %>% lm( u_hausman ~ blackm + hispm +blackd + hispd + boy1st + boy2nd +ageq2nd + ageqk + twins2  + samesex + morekids,  data = .) 

summary(hausman)$r.squared * nrow(hausman$model)




