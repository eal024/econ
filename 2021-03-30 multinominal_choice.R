

# 
library(tidyverse)

# mlogit models:
library(nnet) 
library(mlogit)

# data
heating <- haven::read_dta("data/heating.dta")


attr(heating$idcase, "label")
tibble( navn = names(heating), labels = map_chr( heating, ~.x %>% attr(., "label") ) ) 

# convert number to text
heating_1 <- heating %>% 
  mutate_at( vars(depvar, region) ,as.numeric ) %>% 
  mutate( 
    depvar_text = case_when(
      depvar == 1 ~ "gc",
      depvar == 2 ~ "gr",
      depvar == 3 ~ "ec",
      depvar == 4 ~ "er",
      T ~ "hp") %>% factor( levels = c("gc", "gr", "ec", "er", "hp") ),
    region_text = case_when(
      region == 1 ~ "valley",
      region == 2 ~ "scostl",
      region == 3 ~ "mounth",
      T ~ "ncostl" ) %>% factor( levels = c("valley", "scostl", "mounth", "ncostl") )
    ) %>%  
  relocate( depvar_text, .after = depvar)

tibble( navn = names(heating), labels = map_chr( heating, ~.x %>% attr(., "label") ) ) 
heating_1 %>% group_by(depvar_text) %>%  summarise_all( mean) %>% arrange( depvar) 

heating_1 %>%
  summarise_all( mean) %>%
  arrange( depvar)  

# How interpret the data? -------------------------------------------------

# Five heating possibilities - and they are unordered

# alternative 1: reformulate it to a binary
heating_1 %>% 
  mutate( y = ifelse( str_detect(depvar_text, "^g"),1,0) ) %>% 
  glm( y ~ income + agehed +rooms + I(factor(region)) , data =., family = binomial( link = "logit")) %>% 
  summary()

# alternative 2: reformulate it to a binary - heating by room 

heating_1 %>% 
  mutate( y = ifelse( str_detect(depvar_text, "r$"),1,0) ) %>% 
  glm( y ~ income + agehed +rooms + I(factor(region)) , data =., family = binomial( link = "logit")) %>% 
  summary()


# Multinomial logit ------------------------------------------------------

# First example

library(mlogit)

data("Heating", package = "mlogit")
dfidx(Heating, choice = "depvar", varying = c(3:12))

model_mlogit_heat_1 <- mlogit(depvar ~ ic + oc | 0, dfidx(Heating, choice = "depvar", varying = c(3:12)) )

# Look at the intercepts sign:
summary(model_mlogit_heat_1)

# Choice Prob. goes down with the installation cost and opr. cost.

# How closely to average prob-match the shares of customers:
est_prob <- fitted(model_mlogit_heat_1, outcome = F) %>% broom::tidy() %>% summarise_all( mean) %>% pivot_longer( cols = everything(), names_to = "depvar", values_to = "estimert_andel")

as_tibble(Heating) %>% group_by(depvar) %>% count() %>% ungroup() %>% mutate( faktisk_andel = n/sum(n)) %>% 
  left_join( est_prob, by = "depvar")

# Interpretation:
# What is the WTP from highest installation cost, for reduced operating cost?
# dU = d(ic) + d(oc) -> dic/doc = B0c/Bic

coef(model_mlogit_heat_1)["oc"]/coef(model_mlogit_heat_1)["ic"]

# Implies: WTP is 0.73$ for each dollar reduced oc.


# Model with constant
model_mlogit_heat_1e <- mlogit(depvar ~ ic + oc|0 , dfidx(Heating, choice = "depvar", varying = c(3:12)),
                               reflevel = "ec")


summary(model_mlogit_heat_1e)

fitted(model_mlogit_heat_1e, outcome =  F, ) %>% broom::tidy() %>% 
  summarise_all(  mean )


# Estimate the model with constrain ---------------------------------------

# Set a lifecycle cost constraint

h <- dfidx(Heating, choice = "depvar", varying = c(3:12))

h$lcc <- h$ic + h$oc/0.12

model_mlogit_heat_2 <- mlogit( depvar ~lcc|0, h)

lmtest::lrtest(model_mlogit_heat_2, model_mlogit_heat_1)

# LM-test
2*(1248-1095) 
# H0: r = 12: diskonteringsraten
qchisq(0.05, df = 1, lower.tail = F)




# The model - prediction again: -------------------------------------------

# alternative-specific constants
model3 <- mlogit( depvar ~ic + oc|0 , data =dfidx(Heating, choice = "depvar", varying = c(3:12)), reflevel = "hp" )

summary(model3)

# alternative-specific constants
as_tibble(Heating) %>% group_by(depvar) %>% count() %>% ungroup() %>% mutate( faktisk_andel = n/sum(n)) %>% 
  left_join( fitted(model3, outcome = F) %>%
               broom::tidy() %>%
               summarise_all( mean) %>%
               pivot_longer( cols = everything(), names_to = "depvar", values_to = "estimert_andel") , by = "depvar"
             )


wtp <- coef(model3)["oc"] / coef(model3)["ic"]

r <- 1/wtp # Implies r at 22 per cent. Which seems more reasonble

model3 <- update(model3, reflevel = "gr")

# The extended model ------------------------------------------------------

model4 <- mlogit(depvar ~ oc + I(ic/income),
                 data = dfidx(Heating, choice = "depvar", varying = c(3:12)),
                 method = "nr")

# The model seem to get worse. ln L is lower, the coefficient in installation becomes insignificant
summary(model4)


# Altearntiv: divide install cost by income.
model5 <- mlogit::mlogit( depvar ~ oc + ic | income, data = dfidx(Heating, choice = "depvar", varying = c(3:12)),
                reflevel = "hp"
                )


#
summary(model5)

# The model implies that as income rises, 
# the probability of heat pump rises relative to all the others
# (since income in the heat pump alt is normalized to zero,
# and the others enter with negative signs such that they are lower than that for heat pumps.
# Also, as income rises, the probability of gas room drops relative to the other non-heat-pump
# systems (since it is most negative).


# Do these income terms enter significantly? No. It seems that income doesn't
# really have an effect. Maybe this is because income is for the family 
# that lives in the house, whereas the builder made decision of which system to install.

lmtest::lrtest(model3, model5)




# Lecture example ---------------------------------------------------------

mlogit_heating <-  nnet::multinom( depvar ~income + agehed + rooms + factor(region), data = heating_1)

summary(mlogit_heating)$coefficients 

summary(mlogit_heating)$standard.errors

# Predicted probabilities
fitted(mlogit_heating) %>% broom::tidy() %>% summarise_all(list(mean = mean, sum = sum)) 

# Margins:
margins::marginal_effects( mlogit_heating,
                           data = heating_1,
                           variable = "income") %>%
  as_tibble( )
  summarise_all(mean)

margins::dydx(mlogit_heating, variable = "income")



# Multinomial Chioce: conditional logit -----------------------------------

heating_1_long <- heating_1 %>% 
  select(-depvar) %>% 
  select(1:12) %>%
  pivot_longer( - c(idcase,depvar_text)) %>% 
  separate( col = "name", into = c("cost", "alt")) %>% 
  select(idcase,depvar = depvar_text, alt, cost, value) %>% 
  pivot_wider( names_from = cost, values_from = value) %>% 
  mutate_at( vars(ic,oc ), ~.x/1000) %>% 
  group_by(idcase) %>% 
  mutate( choice = ifelse( depvar == alt, 1,0)) %>% 
  relocate( choice, .after = alt)

heating_1_long %>%
  ungroup() %>% 
  multinom( choice ~ ic + oc , data = . ) %>% summary()

# Conditional on idcases:
model3 <- mlogit( depvar ~ic + oc|0 , data =dfidx(Heating, choice = "depvar", varying = c(3:12)), reflevel = "hp" )

#
coef(summary(model3))

fitted(model3) %>% broom::tidy() %>% summarise_all(mean)

predicted <- fitted( model3, outcome =  F ) %>% broom::tidy() %>%  summarise_all(  mean )

# Condition:
heating_1_long %>% group_by(depvar) %>% count(choice) %>% filter( choice == 1) %>% ungroup() %>% mutate( n/sum(n)) %>% 
  left_join( predicted %>% pivot_longer(everything()) %>% rename( depvar = name ), by =  "depvar")

## Mixed logit model



# Lecture-------------------------------------------------------------

library(nnet) # mlogit is an alternativ

mlogit_heating <-  nnet::multinom( depvar ~income + agehed + rooms + factor(region), data = heating_1)

summary(mlogit_heating)$coefficients %>% data.frame() %>% rownames_to_column( "alt") %>% as_tibble() 

coef <- summary(mlogit_heating)$coefficients %>%
  data.frame() %>%
  rownames_to_column( "alt") %>% as_tibble() %>%  
  select(depvar, everything()) %>% 
  pivot_longer( -depvar, names_to = "var", values_to = "coeff") 

coef

se <- summary(mlogit_heating)$standard.errors %>% as_tibble( ) %>% mutate( depvar = row_number()) %>% 
  select(depvar, everything()) %>% 
  pivot_longer( -depvar, names_to = "var", values_to = "se") 


coef %>% left_join(se, by = c("depvar", "var") ) %>% head(20)

# Predicted values:The predicted outcome is the one with the highest predicted probability
mlogit_heating$fitted.values %>% as_tibble() %>% janitor::clean_names() %>% summarise_all( list(sum = sum, mean = mean ) )

mlogit_heating



























# Appendix ----------------------------------------------------------------




