males_1 %>%
  mutate( pred = predict(model_ols) ) %>% 
  group_by(nr) %>%
  mutate( exper_2 = exper^2, union = ifelse( union == "yes", 1, 0), maried = ifelse( maried == "yes", 1, 0)
  ) %>% 
  mutate( m.wage = mean(wage),
          m.school = mean(school),
          m.exper = mean(exper),
          m.exper_2 = mean(exper^2),
          m.union = mean(union),
          m.maried = mean(maried),
          m.black = mean(black),
          m.hispanic = mean(hispanic),
          # index =  seq( from = 1, to = 8) %>% rep(times = 545)
  ) %>%
  ungroup() %>% 
  mutate( wage   = wage-m.wage,
          school = school - m.school,
          exper  = exper - m.exper,
          exper_2 = exper_2 - m.exper_2,
          union = union - m.union,
          maried = maried - m.maried,
          black = black - m.black,
          hispanic = hispanic - m.hispanic
  ) %>% 
  select( year, wage, school, exper, exper_2, union, maried, black, hispanic, public) %>% 
  ungroup() %>%
  filter( year == 1987) %>% 
  lm( wage ~ school +  exper + exper_2 + union + maried + black + hispanic + public, data = .) %>% 
  summary()



males_1 %>% group_by(nr) %>%  transmute( school = mean(school), 
                                         exper = mean(exper), 
                                         exper_2 = mean(exper^2),
                                         union = mean(as.integer(union)), maried = mean(as.integer(maried)),
                                         public = mean(public),
                                         black = mean(black),
                                         hispanic = mean(hispanic),
                                         wage = mean(wage)
) %>% 
  ungroup() %>% 
  mutate( y = males_1$year) %>% 
  select(-y) %>% 
  lm( wage ~ school +  exper + I(exper^2) + union + maried + black + hispanic + public, data = .) %>% 
  summary( )