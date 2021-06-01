

library(tidyverse)


data_iv <- haven::read_dta("data/angev98.dta")

skimr::skim(data_iv %>% select(morekids))
# correlation between x1, x2
data_iv  %>%  lm( workedm ~ morekids , data = .) %>%  summary()

data_iv  %>%  lm( workedm ~ morekids + ageqk, data = .) %>%  summary() 

#
cor(data_iv$morekids, data_iv$ageqk)

cov_x_y <- ((-0.12-(-0.17) )*0.49^2)/0.0066

cov_x_y/(var(data_iv$morekids)*var(data_iv$ageqk))^0.5
