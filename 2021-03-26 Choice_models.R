

# 
library(tidyverse)


fs::dir_ls("data/data_verbeek")

priv_insurance <- haven::read_dta("data/mus14data.dta")

skimr::skim(priv_insurance)

priv_insurance_1 <- priv_insurance %>% select(ins, retire, age, hstatusg, hhincome, educyear,married, hisp)

# Stats
priv_insurance_1 %>% summarise_all( function(x){ list( m = mean(x), s = sd(x), min = min(x), max = max(x))  }) %>% unnest( cols =  everything()) %>% 
  mutate( statistikk = c("mean", "sd", "min", "max")) %>%
  select(statistikk, everything())


# Models
lpm <- priv_insurance_1 %>% lm( ins ~ retire + age +hstatusg + hhincome + educyear + married + hisp , data = .)

# Probit
probit <- priv_insurance_1 %>% glm( ins ~ retire + age +hstatusg + hhincome + educyear + married + hisp , data = ., family = binomial(link = "probit"))

# Logit
logit <- priv_insurance_1 %>% glm( ins ~ retire + age +hstatusg + hhincome + educyear + married + hisp , data = ., family = binomial(link = "logit")) 

stargazer::stargazer(lpm,probit,logit, type = "text")


# Interpretation of the result --------------------------------------------


tibble( model = c("lpm","probit", "logit"), mean = map_dbl(list( lpm, probit, logit), ~.x %>% predict( type = "response" ) %>% mean() ),
        sd = map_dbl(list(lpm, probit, logit), ~.x %>% predict( type = "response" ) %>% sd() ),
        min = map_dbl(list(lpm, probit, logit), ~.x %>% predict( type = "response" ) %>% min() )
        ) %>% 
  add_row( model = "inv",
           mean = priv_insurance_1$ins %>% mean(),
           sd =  priv_insurance_1$ins %>% sd(),
           min  = priv_insurance_1$ins %>%  min(na.rm = T)
           )



# Partial effects ---------------------------------------------------------

library(mfx)
library(margins)
library(broom)
logitmfx( logit, data = priv_insurance_1)
logitmfx( probit, data = priv_insurance_1)
logitmfx( lpm, data = priv_insurance_1)


lpm %>% summary()
margins::margins( priv_insurance_1, model = lpm, variables = "hhincome")



summary(probit)
priv_insurance_1 %>% mutate( pred = predict( probit, type = "response") )

margins::margins( priv_insurance_1, model= probit , variables = "hhincome")

# Probit
summary(probit)
augment( probit , type.predict = "response", newdata = priv_insurance_1 %>% dplyr::select(-ins) %>% summarise_all(mean))
augment( probit , type.predict = "response", newdata = priv_insurance_1) %>% summarise( m = mean(.fitted, na.rm =T) ) %>% pull(m)*0.374

# PEA (Partial effect at the means)
margins::margins( priv_insurance_1, model= probit , variables = "hhincome")

# APE (average partial effects)
margins::dydx(probit,data = priv_insurance_1 , variable = "hhincome") %>% as_tibble() %>% summarise( mean = mean(dydx_hhincome))

# Logit
tidy(summary(logit)$coefficients)
fitted_logit_atmeans <- augment( logit , type.predict = "response", newdata = priv_insurance_1 %>% dplyr::select(-ins) %>% summarise_all(mean)) %>% pull(.fitted)
fitted_logit_atmeans*(1-fitted_logit_atmeans)*0.00230
margins::margins( priv_insurance_1, model= logit , variables = "hhincome")
margins::dydx(logit,data = priv_insurance_1 , variable = "hhincome") %>% as_tibble() %>% summarise( mean = mean(dydx_hhincome))

fitted_logit_atmeans <- augment( logit , type.predict = "response", newdata = priv_insurance_1 %>% dplyr::select(-ins) %>% summarise_all(mean)) %>% pull(.fitted)
fitted_logit_atmeans*(1-fitted_logit_atmeans)*0.0023
margins::margins( priv_insurance_1, model= logit , variables = "hhincome")



# Inference ---------------------------------------------------------------
library(sandwich)
library(AER)
coeftest(probit, vocv = vcovHC, type = "HC1") 
coeftest(logit, vocv = vcovHC, type = "HC1") 
coeftest(lpm, vocv = vcovHC, type = "HC1")

aod::wald.test( b = coef(probit), Sigma = vcov(probit), Terms = c(2,5))

# Prob

# The difference between two parameter:
# Swithing places by 1 and -1
l <- c(0,0,0,-1,1,0,0,0)

# The logLike test ratio
summary(probit)
logLik(probit)



# Goodness of fit
probit

# Check this again. Look at Verbeek (2012) for the example
#rms::lrm(formula = probit$formula, data = priv_insurance_1)
#rms::lrm(formula = ui  ~  rr + I(rr ^ 2) + age + I(age ^ 2 / 10) + tenure + joblost + married + dkids + dykids + smsa + nwhite + yrdispl + school12  + sex + statemb + stateur + head, data = benefit)


# 1) McFadden’s Pseudo-R2:
logLik(probit)
pR2 <- 1 - probit$deviance/probit$null.deviance
1-logLik(probit)[1]/logLik( glm(ins ~ 1, data = priv_insurance_1, family =  binomial(link = "probit") ) )[1]
  
# 2) How many per cent correctly predicted?

prob_probit <- probit %>% predict( type = "response")


head(prob_probit, 20)
p_p <- ifelse(prob_probit > 0.5, 1, 0  )

# Sum
goodness_fitt_table <- tibble( true = priv_insurance_1$ins, prediced =  p_p) %>%
  mutate( compare = ifelse(true == prediced, "rett", "feil")) %>% 
  group_by( true,compare) %>% 
  count() %>%
  ungroup() 

goodness_fitt_table %>%
  mutate( andel = n/sum(n) ) %>%
  group_by(compare) %>%
  summarise( sum(andel))

## Goodness of fit: table
goodness_fitt_table %>% 
  pivot_wider( names_from = true, values_from = n) %>% janitor::clean_names()

glance( logit)
glance( probit)
glance(lpm)

pred <- ifelse( predict(probit, types = "response") > 0.5, "yes", "no")
trues <- ifelse( priv_insurance_1$ins == 0, "no", "yes" )

tibble( pred = pred, t = trues, correct = ifelse(pred == t, 1, 0)) %>% group_by(t, correct) %>% count()  
# The caret packages
caret::confusionMatrix( factor(pred), factor(trues))











