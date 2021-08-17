
library(broom)
library(tidyverse)

dir("data/data_verbeek")
m <- read.delim("data/data_verbeek/males.asc", sep = " ", header = F) %>% as_tibble()
males <- Ecdat::Males %>% as_tibble()

mean(m[[8]])
males %>% summarise( mean_exp = mean(exper))

males$year %>% unique() %>% length()

# 545 working males. N = 545, T = 8
males %>% nrow()/males$year %>% unique() %>% length()

males_1 <- males %>%
  mutate( public   = ifelse( industry == "Public_Administration", 1, 0),
          black    = ifelse( ethn == "black", 1, 0),
          hispanic = ifelse( ethn == "hisp", 1, 0)
  ) %>% mutate(index = 1:nrow(.))

View( m %>% mutate(indeks = 1:nrow(.)))
males_1 %>% select(union, index) %>% View("Male_1")

males_1$v29 <- m
males_1 %>% select(union, index) %>% slice(20:30) 

males %>% skimr::skim()

males$industry %>% unique()

 
model_ols <- males_1 %>% 
  mutate(exper_2 = exper^2, union = as.integer(union), maried = as.integer(maried)
         ) %>%
  lm( wage ~ school +  exper + exper_2 + union + maried + black + hispanic + public , data = .  )

# Overall
summary(model_ols)$r.squared
sst <- sum( (males_1$wage - mean(males_1$wage))^2  )
sse <- sum(resid(model_ols)^2)
1-sse/sst 

# 
tibble( nr = males_1$nr, year = males_1$year, y = males_1$wage, m.y = mean(males_1$wage) ) %>% 
  group_by( nr) %>% 
  mutate( y_a = (y - m.y)^2,
          y_w = (y - mean(y))^2,
          y_b = (mean(y) - m.y)^2  
          ) %>% 
  ungroup() %>% 
  summarise( N = n_distinct(nr),
             time = n_distinct(year),
             r2_overall = sum(y_a)*(1/( N*time ))
             )

# Overall
cor( predict(model_ols), males_1$wage )^2

# Between
cor( predict(model_between),
     males_1 %>% group_by(nr) %>%
       summarise( y = mean(wage) ) %>%
       pull(y)  
     )^2

# Within
data <- tibble( y_fe = predict(model_within),
        nr = males_1$nr,
        y = males_1$wage
        ) %>% 
  group_by(nr) %>% 
  mutate( y_fe_ = y_fe - mean(y_fe),
          y_ = y - mean(y)
          ) 


cor( data$y_fe_, data$y_)^2



model_between <- males_1 %>% plm::plm( formula = wage ~ 0 +school +  exper + I(exper^2) + union + maried + black + hispanic + public,
                      data = ., model = "between") 

model_within <- males_1 %>% plm::plm( formula = wage ~ school +  exper + I(exper^2) + union + maried + black + hispanic + public,
                                      data = ., model = "within") 

model_random <- males_1 %>% plm::plm( formula = wage ~ school +  exper + I(exper^2) + union + maried + black + hispanic + public,
                                      data = .,model = "random")

# 
summary(model_ols)
summary(model_between)
summary(model_within)
summary(model_random)

# Variance
N <- unique(males_1$nr) %>% length()
time <- unique(males_1$year) %>% length()
k <- 7
sigma_e <- model_within %>% augment() %>% summarise( simga_e = sum(.resid^2)/(N*(time-1) ) ) %>% pull(simga_e)
model_between %>% augment( ) %>% summarise(sigma_b = sum(.resid^2)/N ) 

df_pred <- males_1 %>% 
  mutate( `I(exper^2)` = exper^2,) %>% 
  select(nr ,school ,  exper , `I(exper^2)` , union , maried , black , hispanic , public ) %>% 
  mutate_if( is.factor, as.integer) %>% 
  group_by(nr) %>% 
  summarise_all( mean)

predict( model_between, data = df_pred)

males_1 %>% 
  select( wage , nr) %>% 
  group_by(nr ) %>% 
  summarise( wage = mean(wage)   ) %>% 
  mutate( pred = predict( model_between, data = df_pred) ) %>% 
  summarise( se =  sum((wage - pred)^2 )/(N) )

sigma_a <- 0.119 - (0.1234/8)
tau <- sigma_e/(sigma_e + time*sigma_a )


