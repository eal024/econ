
# Overtall R-sq:
model_overall <- df_beer_accid %>% lm( fatalityrate ~ beertax, data = .)

model_overall %>% summary()

# between:
model_between01 <- df_beer_accid %>% 
  select( state, year, fatalityrate, beertax) %>% 
  group_by( state) %>% 
  mutate( m.fatalityrate = mean( fatalityrate),
          m.beertax = mean(beertax) 
  ) %>% 
  filter( year == 1988) %>% 
  lm( m.fatalityrate ~ m.beertax, data = .)

summary(model_between01)

# Within variation!
model_within_plm <- plm::plm(data = df_beer_accid,
         formula = fatalityrate ~ beertax,
         model = "within") 


# Or from the FE-model
fe_model <- df_beer_accid %>% 
  select( state, year, fatalityrate, beertax) %>% 
  group_by(state) %>% 
  mutate( m.fatalityrate = mean(fatalityrate),
          m.beertax = mean(beertax)
  ) %>% 
  ungroup( ) %>% 
  mutate( yhat =  fatalityrate - m.fatalityrate,
          xhat =  beertax -      m.beertax 
  ) %>% 
  lm( yhat  ~ 0 + xhat, data = .)

summary(fe_model)
broom::glance(fe_model)
broom::augment(fe_model) %>% mutate( sse = (yhat - .fitted)^2,
                                     sst = (yhat - mean(yhat))^2
                                     ) %>% 
  summarise( R2 = 1 - sum(sse)/sum(sst) )



summary(plm::plm( data = df_beer_accid, formula = fatalityrate ~ beertax, model = "random"))



# variance decompostion -----------------------------------------------------------

# Sigma_between
df_beer_accid %>% 
  select(state, year, beertax, fatalityrate) %>% 
  mutate( m.x = mean(beertax),
          m.y = mean(fatalityrate)) %>% 
  group_by(state) %>% 
  mutate( m.x.i = mean(beertax),
          m.y.i = mean(fatalityrate)
          ) %>% 
  ungroup() %>% 
  mutate( x = (m.x.i - m.x)^2,
          y = (m.y.i - m.y)^2
          ) %>%
  select(state, m.x:y) %>% 
  distinct() %>% 
  summarise( antall = n_distinct(state),
              min = min(m.x.i),
              max = max(m.x.i),
              sigma_between_x = ((1/(n()-1))*sum(x))^0.5,
              test_sigma_x = sd(m.x.i),
              sigma_between_y = ((1/(n()-1))*sum(y))^0.5,
              tet_sigma_y = sd(m.y.i)
              ) %>% 
  pivot_longer( everything()
                )

# Overall
as_tibble(df_beer_accid) %>%
  mutate( tx = (beertax-mean(beertax) )^2 ) %>% 
  summarise( sigma = sd(beertax),
             mean =  mean(beertax),
             sigma_test = ( (1/(7*48-1) )*sum(tx) )^0.5
             )


# within
df_beer_accid %>% 
  group_by(state) %>% 
  mutate( m.x.i = mean(beertax),
          xt_xi = (beertax - m.x.i)^2  
          ) %>% 
  ungroup() %>% 
  select( state, year, xt = beertax, m.x.i, xt_xi ) %>% 
  group_by(state) %>% 
  summarise( xt_xi = sum(xt_xi) ) %>% 
  summarise( 
    sigma_within = ( (1/ ( 48*(7-1) ) )*sum(xt_xi) )^.5 ) %>% 
  identity( )
  








