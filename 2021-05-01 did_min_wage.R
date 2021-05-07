

# 
library(tidyverse)

minwage <- haven::read_dta("data/econ4137_seminar11_did.dta")

tibble( na = names(minwage), label = map_chr( minwage, ~.x %>% attr("label") ))

skimr::skim(minwage)

minwage %>% group_by(state, post) %>% summarise( antall = n()   ) %>% pivot_wider( names_from = post, values_from = antall)
# Wage was unchanged
minwage %>% group_by(state, post) %>% summarise( mean_min_w = mean(minwage, na.rm = T)   ) %>% pivot_wider( names_from = post, values_from = mean_min_w)

minwage %>% group_by(state, post) %>% summarise( mean_empl  = mean(empft, na.rm = T)   ) %>% pivot_wider( names_from = post, values_from = mean_empl)

# Model 1
model1  <- minwage %>%
  filter( post == 0) %>% 
  lm( empft ~ minwage + nregs + hrsopen  + d2 + d3 + d4, data = .)

expand_grid(  p_value = seq(from = 0.7, to = 0.95, by  =0.05), N = seq(from = 10, to =100, by =10) ) %>% arrange( N) %>% mutate( t = qt(p_value, N) ) %>% pivot_wider( names_from = p_value, values_from = t)

stargazer::stargazer( model1, type = "text")

# Confindence interval:
-5.175-2.288*qt( (1-(0.1/2)), nrow(minwage %>% filter( post == 0)) )
-5.175+2.288*qt( (1-(0.1/2)), nrow(minwage %>% filter( post == 0)) )


# Test n2 != n3
as.data.frame( vcov(model1)) %>% rownames_to_column() %>% filter( str_detect(rowname, "d")) %>% select(1, d2:d4)

(1.125+1.178)/(2.79+1.41-2*0.587)^0.5


model1_test_n <- model1  <- minwage %>%
  filter( post == 0) %>% 
  lm( empft ~ minwage + nregs + hrsopen  + I(d2-d3) + d4, data = .)

summary(model1_test_n)


# Two-way Fixed effects ---------------------------------------------------

model_twow_fe <-minwage %>% lm( empft ~minwage + state + post + d2 + d3 + d4 + nregs + hrsopen, data = .)


# DiD-estimation ----------------------------------------------------------

model_did <- minwage %>% 
  lm( empft ~  post + state + I(post*state) + d2 + d3 + d4 + nregs + hrsopen , data = .) 


stargazer::stargazer( model1,model_twow_fe , model_did, type = "text")








