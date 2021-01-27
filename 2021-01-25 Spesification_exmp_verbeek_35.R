
#
library(tidyverse)

house_price <- read_delim("data/data_verbeek/housing.dat", delim = " ") %>%
  mutate( price = as.numeric(price),
          log_price = log(price),
          lotsize = as.numeric(lotsize),
          log_lotsize = log(lotsize)
  )

# Saleprice and the house characteristics: hedonic price function
house_price %>% skimr::skim()


model1 <- house_price %>% lm( log_price  ~ log_lotsize + bedrooms + bathrms + airco, data = .)

# model1 Simple
model1 %>% summary()

# Test functional form
house_price_1 <- tibble( select(house_price, log_price, log_lotsize, bedrooms, bathrms, airco ), yhat = model1$fitted.values)

model1_reset <- house_price_1 %>%
  mutate( yhat_2 = yhat^2, yhat_3 = yhat^3) %>%
  lm( log_price ~ log_lotsize + bedrooms + bathrms + airco + yhat_2 + yhat_3, data = .)

# RESET Model:
  # Model not for meaningful results, only for test f-form
summary(model1_reset)
  # Result do not indicat misspesification

## Model2: Extended
model2 <- house_price %>%
  lm( log_price ~ log_lotsize + bedrooms + bathrms + airco + driveway + recroom + fullbase + gashw + garagepl + prefarea + stories, data =. )

# log lotsize has lower prediction. THis is because ceteris paribus has changed.
  # Bigger house tend to have more often driveway etc. which (is now included),
summary(model2)

## Model3: Explained price
model3 <- house_price %>%
  lm( price ~ log_lotsize + bedrooms + bathrms + airco + driveway + recroom + fullbase + gashw + garagepl + prefarea + stories, data =. )

summary(model3)

## PE-test - linear or log-linear
house_PE_data <- tibble( house_price ,
        log_yhat_m3 = log(model3$fitted.values), log_yhat_m2 = model2$fitted.values)

PE_model_test_linear <- house_PE_data %>%
  mutate( log_test = (log_yhat_m3-log_yhat_m2) ) %>%
  lm( price ~ log_lotsize + bedrooms + bathrms + airco + driveway + recroom + fullbase + gashw + garagepl + prefarea + stories +
        log_test
        , data =. )

# Result t = -5.5
summary(PE_model_test_linear)



