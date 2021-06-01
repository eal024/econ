


library(tidyverse)

did <- haven::read_dta("data/econ4137_seminar11_did.dta")

did %>% skimr::skim()

did %>%   lm( empft ~ state + post + I(state*post) , data = .) %>% summary()


did_model <- did %>% group_by(post, state) %>% summarise( m_w = mean(minwage)) |> pivot_wider(names_from = state, values_from = m_w)


did %>% lm(minwage ~ I(post*state)  + state, data = .) %>%
  broom::tidy()

did(5.05+0.8+0.45) - (3.8+0.45)