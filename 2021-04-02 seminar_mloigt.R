
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


# Lang as:
#expand_grid( mode = unique( data$mode) , id = 1:nrow(data)  ) %>% arrange( id) %>% left_join(data, by = c("mode", "id" = "idcase")) %>% group_by(id) %>% arrange(id, pbeach) %>% 



# mlogit ------------------------------------------------------------------

# a) Some descriptive data: 
# How many choose those different alternatives:
andel <- data %>% group_by(mode) %>% count() %>% ungroup( ) %>% mutate( andel = n/sum(n))

andel
# Price
data %>% group_by(mode) %>% summarise_at( vars( p.beach:p.charter), mean)

# Quantum
data %>% group_by(mode) %>% summarise_at( vars( q.beach:q.charter), mean)

# Individual specific
data %>% group_by(mode) %>% summarise_at( vars( income), mean)

# Price and catch rate at different mode
g <- data %>% 
  mutate( pincome = income*10) %>% 
  select(-idcase, -income) %>% 
  pivot_longer( -mode ) %>% 
  mutate( v_class = ifelse( str_detect(name, "^p"), "heigh", "low"  )  )  %>% 
  separate( col= name, into= c("komp", "mode2"), remove = F) %>%
  mutate( e = ifelse( mode == mode2, 1, 0 ), ) %>% replace_na( list(e = 0)) %>%  
  group_by( v_class) %>% 
  nest() %>% 
  mutate( graph = map( data, function(x) { x %>%
      ggplot( aes( x = fct_reorder(name, value), y = value , fill = factor(e) )) +
      geom_boxplot() +
      facet_wrap(  ~mode , scales = "free_y")  + theme( axis.text.x = element_text( angle = 45) )}
      )
      )
# Price and quantum
g$graph[[1]]

g$data[[1]] %>% separate( col= name, into= c("komp", "mode2")) %>% mutate( e = ifelse( mode == mode2, 1, 0 )) %>% replace_na( list(e = 0))

# b) The multinomial logit model---------------------------------------------------------------

# multinomial model: characteristics by the individual
# The conditional model: characteristics of the choice 

# b) Multinomial conditional Logit model: characteristics of the choice 
# model1 <- nnet::multinom( formula = mode  ~ income, data = data %>% mutate( mode = fct_relevel( mode, "charter")))
# 
# summary(model1)
# 
# mlogit
model1a <- mlogit(mode ~0|income, data = dfidx( data = data, choice = "mode", varying = c(3:10) )   )

summary( update(model1a, reflevel = "beach")  )

coef <- as_tibble(summary(model1)$coefficients) %>% mutate( mode = summary(model1)$coefficients %>% row.names()) %>% select(mode, everything())
coef




# Filter for each mode
d <- margins::marginal_effects(model1) %>% mutate( index = 1:nrow(margins::marginal_effects(model1))) %>% as_tibble() %>%
  left_join( data %>% select( mode) %>% mutate( index = 1:nrow(data)) , by = "index" ) %>%
  group_by( mode) %>% 
  nest() 

# Reflevel == beach, filter by pier (3), mean marginal effect
d$data[[3]] %>% summarise( mean(dydx_income), antall= n()) 

# Predicted
fitted( model1 ) %>% as_tibble() %>% summarise_all( list(mean = mean) )


  
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


# Test those two models:
# Look at example:https://cran.r-project.org/web/packages/mlogit/vignettes/e1mlogit.html

# d) IIA property:



# # e) conditional logit model ----------------------------------------

data_1_mlogit <- dfidx(data, choice = "mode", varying = c(3:10))

model3_cml <- mlogit( data = data_1_mlogit, formula = mode ~ p + q|0, reflevel = "beach")

summary(model3_cml)
p_e <- summary(model3_cml)$CoefTable %>% broom::tidy() %>% filter( .rownames == "p") %>% pull(Estimate)
q_e <- summary(model3_cml)$CoefTable %>% broom::tidy() %>% filter( .rownames == "q") %>% pull(Estimate)
# Partial effects for those who choose different choies: Self partial effect, for delta p

tibble(fitted = fitted(model3_cml)) %>% mutate(idcase = 1:nrow(.))  %>% left_join(data_1_mlogit, #%>%
                                                                                  #filter( idx$id2 == "beach" ),
                                                                                  by = "idcase") %>%
  filter(mode == T)  %>%
  mutate(pr =  fitted * (1 - fitted) * (-0.02047652)) %>%
  group_by(mode, idx$id2) %>%
  summarise(mean_p = mean(pr))

#
tibble(fitted = fitted(model3_cml)) %>% mutate(idcase = 1:nrow(.))  %>% left_join(data_1_mlogit, #%>%
                                                                                    #filter( idx$id2 == "beach" ),
                                                                                    by = "idcase") %>% 
  mutate( f = exp(p_e *p + q_e*q) ) %>% 
  group_by( idcase) %>% 
  mutate( pr = f/sum(f),
          pr_t = pr[mode == T]
          ) %>% 
  arrange( idcase, mode) %>% 
  ungroup() %>% 
  mutate( dpr = ifelse( mode == T, pr_t*(1-pr_t)*p_e , -pr_t*pr*p_e) ) %>% 
  filter( idx$id2 == c("beach", "pier"  ) ) %>% 
  group_by( idx$id2, mode) %>% 
  summarise( dpr = mean(dpr) )
  
  








# Different modes:
condtinoal_models <- tibble( mode = data$mode %>% unique(),
        d = list(data)
        ) %>% 
  mutate( d = pmap( list( d = d, m = mode), function(d, m) { d %>% filter( mode == m)})) %>% 
  # mutate( d_p = map(d, ~.x %>% mutate_at( vars(p.beach:p.charter), function(x){x*0.9} ) )) %>% 
  # mutate( d_q = map(d, ~.x %>% mutate_at( vars(q.beach:q.charter), function(x){x*0.9} ) )) %>% 
  # Models
  # condition models
  mutate_at( vars(d), function(x) {map(x, function(x){ mlogit( mode ~ p + q|0, reflevel = "beach", data = dfidx(x, choice = "mode", varying = c(3:10)) )  } )} ) 


andel
model3_cml %>% predict() %>% broom::tidy( ) %>% left_join(andel, by = c("names" = "mode") )

#
condtinoal_models$mode[[1]] 
# Condtional model at mode == beach [[1]]
condtinoal_models$d[[1]] %>% summary()


# Marginal effects

new_data <- dfidx( data = data %>% mutate_at( vars(p.beach), function(x){x*0.9}),
                   choice = "mode",
                   varying = 3:10
                   )


model3_cml %>% predict( newdata = new_data) %>% broom::tidy() %>%  summarise_all( mean) %>% 
  pivot_longer( everything(), names_to = "names", values_to = "value_marginal") %>% 
  left_join(model3_cml %>% predict( ) %>% broom::tidy() %>% rename( value = x)  , by = "names") %>% 
  # Delta
  mutate( delta = value_marginal -value )



# The mixed model:
model4_mixed <-  mlogit( data = dfidx(data, choice = "mode", varying = c(3:10)) ,
        formula = mode ~ q + p| income
        )


summary(model4_mixed)

# Sample partial effects:
model4_mixed %>% predict()
model4_mixed %>% predict( newdata = new_data) %>% broom::tidy() %>% summarise_all(.fun = mean)

new_data2 <- dfidx( data = data %>%
                      filter( mode == "beach") %>% 
                      mutate_at( vars(p.beach:p.charter), function(x){x*0.9}),
                   choice = "mode",
                   varying = 3:10
)

p <- model4_mixed %>% predict( newdata = new_data2) %>% broom::tidy() %>% summarise_all(.fun = mean)

p %>% pivot_longer( everything()) %>% rename( mode = name )

# All modes:
tibble( mode = data$mode %>% unique(),
        data = list(data)
        ) %>% 
  mutate( new_data = map2(data,mode, function(x,y) { 
    dfidx( data = x %>%
             filter( mode == y) %>% 
             mutate_at( vars(p.beach:p.charter), function(x){x*0.9}),
           choice = "mode",
           varying = 3:10
    )
    })) %>% 
  # Margnial effect, different modes
  mutate( predict = map(new_data, function(x) { predict(model4_mixed, newdata = x) %>% broom::tidy() %>% summarise_all(mean)} )) %>% 
  unnest( cols = predict) %>% 
  mutate( type = "marginal") %>% 
  select(-data,-new_data) %>% 
  relocate( type , .after = mode) %>% 
  pivot_longer( -c(mode, type)) %>% 
  left_join( p %>% pivot_longer( everything()) %>% rename( mode = name ), by = "mode" ) %>% 
  rename( marginal_effect_at_mode = mode)








