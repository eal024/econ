

# Replica of simulation in The mixtape
library(tidyverse)

dat <- tibble( 
  x = rnorm(n = 1000, mean = 50, sd = 25)
  ) %>% 
  mutate( x = ifelse(x < 0, 0, x),
          D = ifelse( x > 50, 1, 0),
          y = 25 + 0*D + 1.5*x + rnorm( n(), 0, 20)
          ) %>% 
  filter( x < 100)


# Graph the X`s. No jump in the potenial outcome at any point

dat %>% ggplot( aes( x = x, y = y , fill = factor(D), color = factor(D), shape = factor(D) ) ) +
  geom_point(alpha = 0.5) +
  geom_smooth( method = "lm", se= F) +
  geom_smooth( data = dat, aes(x = x, y = y), method = "lm", se = F, color = "black", group = 1, inherit.aes = F)

## Chaning the effect from the cut-off to 40:

dat %>%
  # Change the effct to 40 by the cutoff value
  mutate( y = 25 + 40*D + 1.5*x + rnorm( n(), 0, 20) ) %>% 
  ggplot( aes( x = x, y = y , fill = factor(D), color = factor(D), shape = factor(D) ) ) +
  geom_point(alpha = 0.5) +
  geom_smooth( method = "lm", se= F, color ="black") 


# Non linearity

dat2 <- tibble( x = rnorm(1000, 100, 50)
                ) %>% 
  mutate( x = ifelse(x < 0, 0, x ), D = ifelse(x > 140, 1,0),
          x2 = x^2,
          x3 = x^3,
          y3 = 10000 + 0 * D - 100 * x + x2 + rnorm(1000, 0, 1000)
          ) %>% 
  filter( x < 280)

dat2 %>% ggplot( aes( x = x, y = y3)) + geom_point()+
  geom_smooth( data = dat2 %>% filter( x < 140), aes(x = x, y = y3), method = "lm", se = F, color = "red") +
  geom_smooth( data = dat2 %>% filter( x > 140), aes(x = x, y = y3), method = "lm", se = F, color ="yellow")


# With regression: Appear to be a positiv effect. Even when we kown that D has 0 effect.
dat2 %>% lm( y3 ~ I(x - 140) +  D, data =. ) %>% summary()

# Polynomial equation 
m1 <- dat2 %>% lm( y3 ~ I(x - 140) +  D*., data =. )
m2 <- dat2 %>% lm( y3 ~ I(x - 140) + D +  I(D*x) + I(D*x2) + I(D*x3) + x2 + x3, data =. ) 
stargazer::stargazer(m1, m2, type = "text")



  
  
  
  
  
  
  
  
  
  
