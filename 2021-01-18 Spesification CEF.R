


library(tidyverse)

data <- haven::read_dta("data/femalelabour.dta")

small_data <- data %>%
  filter( year == 1986) %>%
  filter( wage > 0) %>%
  select(nr,  age,  school, wage)


# RESET
model <- small_data %>% mutate( lnwage = log(wage) ) %>% lm( lnwage ~ age + school, data = .)

pred_w <- predict(model)

# RESET model:
RESET_model <- small_data %>%  mutate( lnwage = log(wage),
                        pred_lnwage_2 = pred_w^2,
                        pred_lnwage_3 = pred_w^3) %>%
  lm( lnwage ~age + school + pred_lnwage_2 + pred_lnwage_3, data = .)


# Test H0; pred = 0
RESET_model %>% summary()

## Ok - can recject pred_y
library(car)

linearHypothesis( RESET_model, c("pred_lnwage_2 = 0", "pred_lnwage_3 = 0"))

# F = 0.12
# P-value = 0.886
# can not reject the H0



# The Nonnested alternatives ----------------------------------------------
# Non-nested F-test











