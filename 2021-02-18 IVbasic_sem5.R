

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

# f) Wald
b_wald <- sum(mean(angev98$workedm[angev98$samesex ==1]) - mean(angev98$workedm[angev98$samesex ==0]) )/( mean(angev98$morekids[angev98$samesex ==1]) - mean(angev98$morekids[angev98$samesex ==0]))

b_wald

# Check the result:
model_iv <- angev98 %>% estimatr::iv_robust( workedm ~ morekids | samesex, se_type = "stata",  data = ., )

summary(model_iv) 










