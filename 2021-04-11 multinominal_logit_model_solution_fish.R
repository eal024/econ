

library(mlogit)

data("Fishing",package="mlogit")


Fish <- mlogit.data(Fishing %>% mutate(income = income/1000 ) %>% as_tibble() ,varying=c(2:9),shape="wide",choice="mode")

# conditional logit model with alternative-specific variables only --------

mnl <- summary(mlogit(mode~price+catch-1,data=Fish))

mnl

as_tibble(Fishing)

mnl_o <- summary(mlogit(mode~ 0|income,data= Fish , reflevel = "charter"))

z <- with(Fishing, data.frame(income = index(mnl_o)$alt, mean) ) )

effects(mnl_o, covariate= "income" , data = Fish, type = "rr")

effects(mnl, covariate= "price" , data = Fish, type = "rr")

# beach
u1 <- 

#Fishing <- as_tibble(Fishing) %>% filter(  mode == "beach")
         # Price beach * p + q_beach*q
V1 = exp(Fishing[,2]*mnl$coef[1] + Fishing[,6]*mnl$coef[2])

V2 = exp(Fishing[,3]*mnl$coef[1] + Fishing[,7]*mnl$coef[2])

V3 = exp(Fishing[,4]*mnl$coef[1] + Fishing[,8]*mnl$coef[2])

V4 = exp(Fishing[,5]*mnl$coef[1] + Fishing[,9]*mnl$coef[2])

V = V1 + V2 +V3 + V4

P = cbind(V1/V, V2/V, V3/V, V4/V)

tibble_p <- P %>% as_tibble() %>% rename( beach = 1, pier = 2, boat = 3, charter = 4)
tibble_p

tibble_p %>% summarise_all(mean)
fitted(mlogit(mode~price+catch|0,data=Fish), outcome = F) %>% broom::tidy() %>% summarise_all( mean)

# Prob. beach from increase price beach
marg_bpb <- mean(P[,1]*(1-P[,1])*mnl$coef[1])*100 
#Prob of charter, from Price increase on Beach
marg_cpb <- mean(P[,4]*(0-P[,1])*mnl$coef[1])*100 
#Prob of boeat, from Price increase on Beach
# Private
marg_cboat <- mean(P[,3]*(0-P[,1])*mnl$coef[1])*100 
#Prob of pier, from Price increase on Beach
marg_cpierbeach <- mean(P[,2]*(0-P[,1])*mnl$coef[1])*100 
marg_cpierbeach

# Prob. chater delta, from increase price charter
mean(P[,4]*(1-P[,4])*mnl$coef[1])

#Prob of Charter, from CR increase on Charter
marg_ccc <- mean(P[,4]*(1-P[,4])*mnl$coef[2]) 
marg_ccc
#Prob of Private Boat, from CR increase on Charter
marg_ccp <- mean(P[,3]*(0-P[,4])*mnl$coef[2]) 

marg_bpb
marg_cpb
marg_ccc
marg_ccp

tibble_p %>% summarise_all( mean)
predict(mlogit(mode~price+catch-1,data=Fish) , type = "prop")



#Partial effect for different mode




