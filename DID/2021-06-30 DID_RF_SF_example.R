

# Example DID, RF and SF

library(tidyverse)

# Data from Feb.- Mars to Nov. Dec. 1992.
# Employment at fast fod restauratns in the US (New Jersey and Pennsylvania)
# Data from Card and Krueger (1994, American Economic Review Vol, 84(4)) 

minwage <- haven::read_dta("data/econ4137_seminar11_did.dta")

# Goal: Want to estimate the effect from increasing min.wage level. 

# A) Estimate OLS employment(min.wage)

model_ols_pre <- minwage %>% filter( post == 0) %>% lm(empft ~ nregs + d2+d3+d4 + hrsopen + minwage, data = .)

#
stargazer::stargazer(model_ols, type =  "text")

# B) Controlling for selectionm, modeling with the DID-estimat

model1_did <- minwage %>% lm( empft ~ post + state + I(post * state), data = . ) 


stargazer::stargazer( model1_did, type = "text")


# c) How much does min wage affects full time employment in fast food restaurants?

# To interpret the estimate as the impact of minimum wage, the reduce form (RF) should be divided by the "first stage" (FS)

# First stage
mean(minwage$minwage[minwage$post ==1 & minwage$state == 1]) - mean(minwage$minwage[minwage$post ==0 & minwage$state == 1])

#minwage |> lm( formula =minwage ~ post + state ) 
rf_minwage <- minwage |> filter( state == 1) |> { \(y) lm( minwage ~ post , data = y) }()  

stargazer::stargazer(rf_minwage, type ="text")

rf_minwage_predict <- predict( rf_minwage, newdata = tibble( post = 0:1, state = 1) )

# Impact of increased minwage on employment
model1_did$coefficients[[4]]/(rf_minwage_predict[[2]]-rf_minwage_predict[[1]])

# Increased employment with 4.2

## D) t-test may understate the uncertainty in the effect of min wage on full time employment

# Correcting for the standard errors use the IV-robust estimation.
model2_did_iv <- minwage |> { \(y) estimatr::iv_robust( formula =  empft ~ state +  post + minwage  |  I(post*state) +state +  post , data = y, se_type = "HC1" ) }()

model2_did_iv

model2_did_robust <- minwage |> { \(y) estimatr::lm_robust(
  formula  =  empft ~ hrsopen + nregs + state +  post + I(post*state) ,
  data     = y , se_type = "stata") 
  }()

model2_did_robust






