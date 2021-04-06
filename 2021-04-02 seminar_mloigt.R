
library(tidyverse)
library(mlogit)
library(nnet)

# data
mus15 <- haven::read_dta("data/mus15data.dta") %>% mutate( mode = as_factor(mode) )

tibble( navn = names(mus15), l = map_chr( mus15, ~.x %>% attr("label")))


data <- mus15 %>% 
  # Characteristics choice, and individual that make the choice
  # 1 Char choice
  mutate( idcase = row_number() %>% as.numeric()) %>% 
  select(idcase, mode,  c(pbeach:pcharter, qbeach:qcharter), c(income))

names(data) <- c(  names(data)[1:2], 
                   # p and q
                   map_chr(  names(data)[3:10], function(x) { str_c( str_sub(x, star = 1, end = 1), ".", str_sub(x, start = 2, end = nchar(x) )) } ) ,
                   names(data)[length(names(data))] 
)


data_1 <- data

# Lang as:
#expand_grid( mode = unique( data$mode) , id = 1:nrow(data)  ) %>% arrange( id) %>% left_join(data, by = c("mode", "id" = "idcase")) %>% group_by(id) %>% arrange(id, pbeach) %>% 
data_1_mlogit <- dfidx(data_1, choice = "mode", varying = c(3:10))


# mlogit ------------------------------------------------------------------

# a) Some descriptive data: 

# How many choose those different alternatives:
andel <- data %>% group_by(mode) %>% count() %>% ungroup( ) %>% mutate( andel = n/sum(n))

andel
# Price
data %>% group_by(mode) %>% summarise_at( vars( p.beach:p.charter), mean)

# Quantum

# Individual specific
data %>% group_by(mode) %>% summarise_at( vars( income), mean)

# Price and catch rate at different mode
g <- data %>% 
  mutate( pincome = income*10) %>% 
  select(-idcase, -income) %>% 
  pivot_longer( -mode) %>% 
  mutate( v_class = ifelse( str_detect(name, "^p"), "heigh", "low"  )  )  %>% 
  group_by( v_class) %>% 
  nest() %>% 
  mutate( graph = map( data, function(x) { x %>%
      ggplot( aes( x = fct_reorder(name, value), y = value )) +
      geom_boxplot() +
      facet_wrap(  ~mode , scales = "free_y")  + theme( axis.text.x = element_text( angle = 45) )}
      )
      )
# Price and quantum
g$graph[[2]]


# b) The multinomial logit model---------------------------------------------------------------

# multinomial model: characteristics by the individual
# The conditional model: characteristics of the choice 

# b) Multinomial conditional Logit model: characteristics of the choice 
model1 <- nnet::multinom( formula = mode  ~ income, data = data %>% mutate( mode = fct_relevel( mode, "charter")))

summary(model1)

coef <- as_tibble(summary(model1)$coefficients) %>% mutate( mode = summary(model1)$coefficients %>% row.names()) %>% select(mode, everything())
coef

# Predicted
fitted( model1 ) %>% as_tibble() %>% summarise_all( list(mean = mean, sum = sum) )


# Margins:  Gå igjennom denne igjen:
tibble(  mode = unique(data$mode), data = list(data)) %>% 
  mutate( data = map2(mode, data, function(x,y) { y %>% filter( mode == x)  } ),
          marginal = map(data, function(x) margins::marginal_effects( model1 , data = x) %>% as_tibble() %>% summarise_all(mean))
          )  %>% 
  unnest(marginal)
  
# Consider 10% income

# c) Group beach and pier togheter and reestimate and compare the fit:
data_2 <- data %>% 
  mutate( mode = as.character(mode),
          mode = ifelse( mode %in% c("pier", "beach") , "pierbeach", mode) %>% as.factor() )


model2 <- nnet::multinom( mode ~income, data = data_2 %>% mutate( mode = fct_relevel( mode, "charter")))

#Comparing the fit:
m1 <- summary(model1)
m1$coefficients

m2 <- summary(model2)
m2$coefficients

# Predict model1
fitted(model1, outcome = F) %>% broom::tidy() %>% summarise_all( mean) %>% pivot_longer( cols = everything(), names_to = "mode", values_to = "estimert_andel") %>% 
  left_join(andel, by = "mode")

# model2
fitted(model2, outcome = F) %>% broom::tidy() %>% summarise_all( mean) %>% pivot_longer( cols = everything(), names_to = "mode", values_to = "estimert_andel")

andel
0.151+0.113

# Conclusion: The fit is equal -> predict the same prob.

# d) IIA property:



# # e) conditional logit model ----------------------------------------

# Partial effects of price: 10% decrease
data_2 <- data %>% mutate_at( vars(p.beach:p.charter),  ~.x*0.9) 

# adjusted data
data_2_mlogit <- dfidx(data_2, choice = "mode", varying = c(3:10))

# All prices decrease with 10%
predict( model1, newdata = data_2_mlogit) %>% as_tibble() %>% summarise_all(mean) %>% pivot_longer( cols = everything(), names_to = "mode", values_to = "estimert_andel") %>% 
  left_join(andel, by = "mode") %>% 
  mutate( delta = andel -estimert_andel)

# 2: Only price for beach decreases:
data_2 <- data %>% mutate( p.beach = p.beach*0.9) %>% dfidx( choice = "mode", varying = c(3:10))

# Only price for beach decreases:
predict( model1, newdata = data_2_mlogit) %>% as_tibble() %>% summarise_all(mean) %>% pivot_longer( cols = everything(), names_to = "mode", values_to = "estimert_andel") %>% 
  left_join(andel, by = "mode")



data_3 <- data %>% 
  mutate( mode = as.character(mode),
          mode = ifelse( mode %in% c("pier", "beach") , "pierbeach", mode) %>% as.factor()
          )  %>% 
  mutate( p.pierbeach = (p.pier + p.beach)/2, q.pierbeach = (q.pier + q.beach)/2 ) %>% 
  select( -c(p.beach, p.pier, q.beach, q.pier) ) %>% 
  relocate( income, .after = q.pierbeach) %>% 
  dfidx( choice = "mode", varying = c(3:8))

model2 <- mlogit::mlogit( formula = mode ~ p + q ,data = data_3, reflevel = "private")

# Does the coeff. changes? No, they do not.
model1 %>% summary()
model2 %>% summary()

fitted(model1, outcome = F) %>% as_tibble() %>% summarise_all(mean)
fitted(model2, outcome = F) %>% as_tibble() %>% summarise_all(mean) #0.113+0.151: fitted is also equal


## d) The IIA property



# Appendix ----------------------------------------------------------------
model_nnet <- nnet::multinom( formula =  mode ~ income , data  = data) 

# model 
coef <- summary(model_nnet)$coefficients %>% as_tibble() %>% mutate( mode = summary(model_nnet)$coefficients %>% row.names()) %>% select(mode, everything() )

coef

# Predicted
andel %>% left_join( 
  fitted( model_nnet ) %>% as_tibble() %>% summarise_all( list(mean) ) %>% 
    pivot_longer( everything(), names_to = "mode", values_to = "predicted")
  , by = "mode")


# Margins
margins::marginal_effects( model_nnet, data = Heating) %>% as_tibble()























