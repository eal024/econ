
library(tidyverse)

# Seminar 4137 DID exercise 2 ------------------------------------------------------

# Estimate the impact of having death penalty on muder rates - diff in diff
dir("data")

death <- haven::read_dta("data/death.dta")

# See the variables
tibble( names = names(map(death , ~ attr(.x, "label"))) ,
        values = map_chr(map(death , ~ attr(.x, "label")), function(x){ ifelse( !is.null(x) , x, "XX")} )
)

# State ID (IA), NYm VT, WV abandoned the death penalty.

# 1 == states that abandoned, 0 otherwise

# a) Calculate the DID with OLS
library(data.table)

death_1 <- as.data.table(death)[ , ':=' ( D = ifelse( st %in% c("IA", "NY", "VT", "WV"), 1 , 0),
                                          post = ifelse(year >= 1965, 1, 0),
                                          t = year - 1960) , ] %>% as_tibble()


# a
#i)
death_1 %>% filter( between(year, 1964,1965)) %>% lm( pc_mur ~ D + post + I(D*post), data = .) %>% summary()

# ii)
death_1 %>%  lm( pc_mur ~ D + post + I(D*post) + I( factor(t) ) , data = .) %>% summary()

# Visualization
death_1 %>%
  group_by( D, year) %>%
  summarise( pc_mur = mean(pc_mur, na.rm = T )) %>%
  ggplot( aes( x = year, y  = pc_mur, fill = factor(D), color = factor(D) )) +
  geom_line() +
  geom_vline( xintercept =  1965) +
  geom_smooth( data = death_1 %>% filter( year < 1965), method = "lm", se = F ) +
  geom_smooth( data = death_1 %>% filter( year > 1965), method = "lm", se = F )

# Test different trend between groups:
library(sandwich);
library(clubSandwich)

model_preTtrend <- death_1 %>%  filter( year < 1965) %>% lm( pc_mur ~ year + D + I(year*D) , data = .)

summary(model_preTtrend)

model_preTtrend$model %>% head()
# Cluster-robust standard errors:
coef_test( model_preTtrend, vcov = "CR1", cluster = death_1$stid[death_1$year < 1965] )

# Compositon effect:
# if criminal moves their criminal acts (murders) to states that have abandoned the death penalty, then the bias would be a higher impact from the treatment
death_1 %>%
  filter( between(year, 1964,1965)) %>%
  select(year, D , st, rpc_inc, ur, ipolice, nonwhite , contains("age")) %>%
  group_by(D, year) %>%
  summarise_all( ~mean(.x))

death_1 %>%
  filter( between(year, 1964,1965)) %>%
  select(year, D , st, rpc_inc, ur, ipolice, nonwhite , contains("age")) %>%
  mutate( rpc_inc = rpc_inc/max(rpc_inc, na.rm = T),  ipolice = ipolice/max(ipolice, na.rm = T), ur = ur/max(ur, na.rm = T)) %>%
  pivot_longer(
    -c(D, st, year),
    names_to = "key",
    values_to = "value"
  )  %>%
  #filter( key != "ipolice") %>%
  ggplot( aes(x = key, y = value, fill = factor(D) )) + geom_boxplot() + coord_flip()  + facet_wrap(~year)



# Test time -trend ->
death_1 %>%
  select(year, D , st, rpc_inc, ur, ipolice, nonwhite , contains("age")) %>%
  pivot_longer(
    -c(D, st, year),
    names_to = "key",
    values_to = "value"
  ) %>%
  group_by(year, D, key) %>%
  summarise_all( ~mean(.x, na.rm = T)) %>%
  ggplot( aes( x = year, y = value, fill = factor(D), color = factor(D) ) ) + geom_line() + facet_wrap(~key, scales = "free") + geom_vline( xintercept = 1965, linetype = 2)


# d)  controlling for pop- non-white
model_did_non_white <- death_1 %>% lm( pc_mur ~   + factor(year)  + I(D*post)  + nonwhite ,data = .)

coef_test(model_did_non_white,  vcov = "CR1", cluster = death_1$stid)


## d2)

# Home
death_1 %>% lm( pc_mur ~ D + post  + I(D*post)  ,data = .) %>% summary()
#
model_did_robust <- death_1 %>% lm( pc_mur ~ D + post  + I(D*post)  ,data = .)
# By state
coef_test(model_did_robust,  vcov = "CR1", cluster = death_1$stid)
# By state and after
coef_test(model_did_robust,  vcov = "CR1", cluster = c(death_1$stid & death_1$post) )


