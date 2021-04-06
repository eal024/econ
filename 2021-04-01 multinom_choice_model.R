
# Pacakges
library(mlogit)
library(tidyverse)
library(nnet)

# Data
data("Heating", package = "mlogit")

head(Heating)

Heating %>% group_by( depvar) %>% summarise( n =  n()) %>% 
  ungroup( ) %>% 
  mutate( per_cent = n/sum(n), cum = cumsum(per_cent))

# Descriptive
as_tibble(Heating) %>% select(ic.gc:oc.hp) %>% 
  pivot_longer( everything()) %>% 
  group_by( name) %>% 
  summarise_all(list(
    mean = mean,
    sd = sd,
    min = min,
    max = max
  ))


# Models
# Multinomial Choice: Characterize of individual that choose (room, income agehed)
model0 <- nnet::multinom( depvar ~income + agehed + rooms + factor(region), data = Heating)

coef <- as_tibble(summary(model0)$coefficients) %>% mutate( depvar = summary(model0)$coefficients %>% row.names()) %>% select(depvar, everything())

coef

# Predicted
fitted( model0 ) %>% as_tibble() %>% summarise_all( list(mean = mean, sum = sum) )


# Margins
margins::marginal_effects( model0, data = Heating) %>% as_tibble()

margins::marginal_effects( model0,
                           data = Heating) %>% as_tibble()  


# Conditional models -------------------------------------------------------


# Characteristics to the choices
Heating_1 <- as_tibble(Heating) %>% mutate_at( vars( contains("ic") |  contains("oc"))  , function(x){ x/1000})

model1 <- mlogit(depvar ~ ic + oc|0 , dfidx(Heating_1, choice = "depvar", varying = c(3:12)))


model2 <- mlogit(depvar ~ ic + oc|0 , dfidx(Heating_1, choice = "depvar", varying = c(3:12)),
                               reflevel = "ec")


summary(model1)

summary(model2)

fitted(model1, outcome = F) %>% broom::tidy() %>% summarise_all( mean) %>% pivot_longer( cols = everything(), names_to = "depvar", values_to = "estimert_andel")
fitted(model2, outcome = F) %>% broom::tidy() %>% summarise_all( mean) %>% pivot_longer( cols = everything(), names_to = "depvar", values_to = "estimert_andel")

est_prob <- fitted(model2, outcome = F) %>% broom::tidy() %>% summarise_all( mean) %>% pivot_longer( cols = everything(), names_to = "depvar", values_to = "estimert_andel")
est_sd <- fitted(model2, outcome = F) %>% broom::tidy() %>% summarise_all(sd) %>% pivot_longer( cols = everything(), names_to = "depvar", values_to = "estimert_sd")

as_tibble(Heating) %>% group_by(depvar) %>% count() %>% ungroup() %>% mutate( faktisk_andel = n/sum(n)) %>% 
  left_join( est_prob, by = "depvar") %>% 
  left_join( est_sd , by = "depvar")


# With constant -----------------------------------------------------------


model3_ec <- mlogit(depvar ~ ic + oc , dfidx(Heating, choice = "depvar", varying = c(3:12)),
                 reflevel = "hp")


est_prob3 <- fitted(model3_ec, outcome = F) %>% broom::tidy() %>% summarise_all( mean) %>% pivot_longer( cols = everything(), names_to = "depvar", values_to = "estimert_andel")

# With constant
summary(model3_ec)
est_prob3

summary(model3_ec)
update(model3_ec, reflevel = "gr") %>% summary()


## With reflevel:
as_tibble(Heating) %>% group_by(depvar) %>% count() %>% ungroup() %>% mutate( faktisk_andel = n/sum(n)) %>% 
  left_join( est_prob, by = "depvar") %>% 
  left_join( est_sd , by = "depvar") %>% 
  left_join( est_prob2, by = "depvar")



# Multinominal chicoe: Mixed logit ----------------------------------------

head(Heating)

dfidx(Heating, choice = "depvar", varying = c(3:12))

model4 <- mlogit( depvar ~ ic + oc | income, dfidx(Heating, choice = "depvar", varying = c(3:12)) )

summary(model4)

model5 <- mlogit(depvar ~ ic + oc | income + agehed    ,
                 dfidx(Heating, choice = "depvar", varying = c(3:12)))

summary(model4)$coef %>% broom::tidy() %>% 
  separate( col = names, into = c("var", "type"), sep = ":") %>% 
  pivot_wider( names_from = type, values_from = x)




# Prediction --------------------------------------------------------------


fitted( model3_ec, outcome = F) %>% broom::tidy() %>% summarise_all(mean)

# HP: 5.5%

predict( model3_ec,
         newdata = dfidx(Heating %>%
                           mutate( ic.hp = 0.9*ic.hp),
                         choice = "depvar", varying = c(3:12))) %>% 
  as_tibble() %>% 
  summarise_all(mean)

# HP increase to 6.45% if price HP decrease with 10 per cent.


# IIA: "New technoligy - a new alterantiv" --------------------------------

# The new alternativ cost $200 more than ec, but has 75% of OC to EC

model3_ec <- update(model3_ec, reflevel = "gr")
mc <- model3_ec

chid <- idx(mc, 1)

X <- model.matrix(mc)
Xn <- X[idx(mc, 2) == "ec",]
Xn[, "ic"] <- Xn[, "ic"] + 200
Xn[, "oc"] <- Xn[, "oc"] * 0.75
unchid <- unique(idx(mc, 1))
rownames(Xn) <- paste(unchid, 'new', sep = ".")
chidb <- c(chid, unchid)
X <- rbind(X, Xn)
X <- X[order(chidb), ]
eXb <- as.numeric(exp(X %*% coef(mc)))
SeXb <- as.numeric(tapply(eXb, sort(chidb), sum))
P <- eXb / SeXb[sort(chidb)]
P <- matrix(P, ncol = 6, byrow = TRUE)
apply(P, 2, mean)


# The new tec captures 10% of the marked. 
# It draws the same percent from each system. It drasw the most in absolute terms 
# from the most popular system: gas ventral.

# The same percent drop for all systems is a consequence of the IIA property of logit.













