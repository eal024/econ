

multinomial_fishing1 <- Fishing
#View(multinomial_fishing1)

mldata <- mlogit.data(multinomial_fishing1, varying=c(2:9), choice = "mode", shape = "wide")

mlogit.model1 <- mlogit(mode ~ 1 | income, data=mldata, reflevel="charter")

summary(mlogit.model1)

# Multinomial logit model odds ratios
# Taking the exponents of the Multinomial logit model to create the odds ratio.
exp(coef(mlogit.model1))

# An odds ratio of over one make it more likely than the other outcomes and likewise less than one makes the outcome less likely.
# 
# As income increases,
# 
# Beach becomes more likely.
# Charter becomes more likely
# Pier becomes less likely

model3_cml <- mlogit( data = data_1_mlogit, formula = mode ~ p + q|0, reflevel = "beach")
clogit.model1 <- mlogit(mode ~ price+catch |income, data = mldata, reflevel="beach")
summary(clogit.model1)

m <- mlogit(mode ~ price+catch |income, data = mldata, reflevel="beach")
z <- with(mldata, data.frame(price = tapply(price, index(m)$alt, mean), 
                             catch = tapply(catch, index(m)$alt, mean),
                             income = mean(income)))

effects(m, covariate = "catch", data = z)
  

effects(
  mlogit.model1,
  covariate = "income",
  data = tibble(mode = row.names(z), income =  z$income)
)



