

n <- 10000

df <- tibble( 
  adm = sample(x =  c(0,1), size = n, replace = T ) 
  ) %>% 
  rowwise() %>% 
  mutate( med = ifelse(adm == 1,
                       sample( size = 1, c(0,1), prob = c(0.1,0.9) ),
                       sample( size = 1,  c(0,1), prob = c(0.9,0.1) )
                       ))  %>% 
  ungroup() %>% 
  mutate( y = rnorm(n = n, mean = 10, sd = 1)*med*1 + rnorm(n = n, mean = 3, sd = 1)
          )

df %>% group_by( adm, med ) %>% count()


df %>% lm( med  ~ adm, data = .) %>% summary()

# v1 <- 
  df %>%
  mutate( v1 = ifelse( med == 0, 0, y)) %>%
  lm( v1 ~adm, data = .) %>% summary()        

v2 <- df %>% mutate( v2 = ifelse( med == 1, 0, y)) %>%
  lm( v2 ~adm, data = .) %>% summary()        


coef(v1)[2] + coef(v2)[2]
y

y <- df %>% lm( y ~ adm, data = .) %>% summary()


df %>% estimatr::iv_robust(y ~med|adm, data = .)

mean(df$y[df$adm == 1])-mean(df$y[df$adm == 0])



