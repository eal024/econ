## Cameron and Trivedi's Microeconometrics p.493

## There are two alternative specific variables : pr (price) and ca (catch)

## and four fishing modes : beach, pier, boat, charter

library(mlogit)

data("Fishing",package="mlogit")

Fish <- mlogit.data(Fishing,varying=c(2:9),shape="wide",choice="mode")

#---------------------------------------------------------------------------------

## conditional logit model with alternative-specific variables only

mnl <- summary(mlogit(mode~price+catch-1,data=Fish))



V1 = exp(Fishing[,2]*mnl$coef[1] + Fishing[,6]*mnl$coef[2])

V2 = exp(Fishing[,3]*mnl$coef[1] + Fishing[,7]*mnl$coef[2])

V3 = exp(Fishing[,4]*mnl$coef[1] + Fishing[,8]*mnl$coef[2])

V4 = exp(Fishing[,5]*mnl$coef[1] + Fishing[,9]*mnl$coef[2])

V = V1 + V2 +V3 + V4

P = cbind(V1/V, V2/V, V3/V, V4/V)



#Prob of beach, from Price increase on Beach

marg_bpb <- mean(P[,1]*(1-P[,1])*mnl$coef[1])*100 

marg_bpb

#Prob of charter, from Price increase on Beach

marg_cpb <- mean(P[,4]*(0-P[,1])*mnl$coef[1])*100 

marg_cpb

#Prob of Charter, from CR increase on Charter

marg_ccc <- mean(P[,4]*(1-P[,4])*mnl$coef[2]) 

marg_ccc

#Prob of Private Boat, from CR increase on Charter

marg_ccp <- mean(P[,3]*(0-P[,4])*mnl$coef[2]) 

marg_ccp

#---------------------------------------------------------------------------------

## a "multinomial model" with individual-specific variables only

summary(mlogit(mode~1|income,data=Fish))

## which can also be estimated using multinom (package nnet)

#library(nnet)

#summary(multinom(mode~income,data=Fishing))

#---------------------------------------------------------------------------------

## a "mixed" model with individual and alternative specific variables

m <- mlogit(mode~price+catch|income,data=Fish)

summary(m)




library(mlogit)

data("Fishing",package="mlogit")

Fish <- mlogit.data(Fishing,varying=c(2:9),shape="wide",choice="mode")


mnl <- summary(mlogit(mode~1|income,data=Fish))

mnl

as_tibble(Fishing)

#Fishing <- as_tibble(Fishing) %>% filter(  mode == "beach")
# Price beach * p + q_beach*q
V1 = exp(Fishing[,2]*mnl$coef[1] + Fishing[,6]*mnl$coef[2])

V2 = exp(Fishing[,3]*mnl$coef[1] + Fishing[,7]*mnl$coef[2])

V3 = exp(Fishing[,4]*mnl$coef[1] + Fishing[,8]*mnl$coef[2])

V4 = exp(Fishing[,5]*mnl$coef[1] + Fishing[,9]*mnl$coef[2])

V = V1 + V2 +V3 + V4

P = cbind(V1/V, V2/V, V3/V, V4/V)

# Prob. beach from increase price beach
marg_bpb <- mean(P[,1]*(1-P[,1])*mnl$coef[1])*100 


