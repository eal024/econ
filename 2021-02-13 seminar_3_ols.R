

dir("data")


mus <- haven::read_dta("data/mus03.dta")

# income ~ education + log_medical_expenditure. Control = gender, private insurance

str(mus) 
attr(mus$year03, "label")

tibble( var = names(mus), label =  map( mus, ~ attr(.x, "label"))) %>% unnest(label) %>% head(20)


# 1) 
model1 <- mus %>% 
  filter( totexp != 0) %>%
  lm( I(log(totexp)) ~ income  + female + private , data = .)




#
stargazer::stargazer(model1, type = "text") 