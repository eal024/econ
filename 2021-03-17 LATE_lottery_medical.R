

# Seminar 6 LATE: 
library(tidyverse)

# Data
lottery <- haven::read_dta("data/lottery.dta")

# OLS: Want to estimate the return of attending medical school. Applicants to medical studies are assigned to lottery categories, based on their high school grades.
lottery %>% lm( lnw ~ d , data = .) %>% summary()

# c) Assess instrument relevance
first_stage <- lottery %>% filter( year == 1988) %>% lm( d ~ z, data = . )

summary(first_stage)
# Interpretation.
# d = 1: attending medical school is highly correlated with the instrument.
# 55% from wining the lottery.
# F-statistic is  > 10

# Calculate return attending m.school.

# Wald
(mean(lottery$lnw[lottery$z == 1]) - mean(lottery$lnw[lottery$z == 0]))/(mean(lottery$d[lottery$z == 1]) - mean(lottery$d[lottery$z == 0]))

lottery %>% estimatr::iv_robust( lnw ~ d|z, data = . , se_type = "stata")

lottery %>% estimatr::iv_robust( lnw ~ factor(year) + female + factor(lotcateg) +  d|z +factor(year) + female + factor(lotcateg), data = . , se_type = "stata")

# attending m_s increase wage by 19%. 
# If LATE: This is the effect for the Compliers group.

# d) # Count the number of compliers

lottery %>% 
    group_by(d,z) %>% 
    summarise( y = sum(lnw), n = n(), female = sum(female)/n,  snitt = mean(lnw))

# Female in total
sum(lottery$female )/nrow(lottery)

mean(lottery$d[lottery$z == 1])-mean(lottery$d[lottery$z == 0])

# y1,y0
lottery %>% lm( formula =  lnw ~ z, data = .) %>% summary()
lottery %>% lm( d ~ z, data = .)
lottery %>% estimatr::iv_robust( lnw ~ d | z, data = .)

lottery %>% estimatr::iv_robust( lnw ~ d|z, data = .) %>% summary()

lottery %>% estimatr::iv_robust( lnw ~ female + I(factor(year)) + lotcateg +  d|z + female + I(factor(year)) + lotcateg, data = .) %>% summary()

# Estimating the y1 and y0

lottery %>% mutate( y1 = d*lnw) %>% estimatr::iv_robust( formula =  y1 ~ female + I(factor(year)) + lotcateg + d | z +female + I(factor(year)) + lotcateg  , data = .) %>% summary()
lottery %>% mutate( y0 = (1-d)*lnw) %>% estimatr::iv_robust( formula =  y0 ~ female + I(factor(year)) + lotcateg + I(1-d) | z +female + I(factor(year)) + lotcateg  , data = .) %>% summary()

# Observing the E[y1] always


lottery %>% 
    ggplot( aes( x = lnw) ) +
    geom_density(data = lottery %>% filter( d == 1, z == 0), aes( x = lnw), inherit.aes = F, color = "black") +
    geom_density(data = lottery %>% filter( d == 0, z == 1), aes( x = lnw), inherit.aes = F, color = "red") +
    geom_vline( xintercept =  lottery %>% filter( d == 1, z == 0) %>% summarise( d = mean(lnw)) %>% pull(d) ) +
    geom_vline( xintercept =  lottery %>% filter( d == 0, z == 1) %>% summarise( d = mean(lnw)) %>% pull(d), linetype = 2 )


# The distribution for the compliers

lottery %>% 
    filter( d == 1, z == 1) %>%  
    mutate( y1c = lnw*(0.52+0.41)/0.52 - ifelse(z == 0 & d == 1, (lnw*0.41/0.52) , 0) ) %>% 
    # mutate( f1c = g1c*(0.52 + 0.41)/0.52 - f1*(0.41/0.52) ) %>% 
    # filter( f1c > 0) %>% 
    ggplot( aes( x = lnw)) + geom_density( color = "blue") +
    geom_density( data = lottery %>% 
                      filter( d == 0, z == 0) %>%  
                      mutate( y0c = lnw*(0.52+0.07)/0.59 - ifelse(z == 0 & d == 1, (lnw*0.07/0.59) , 0) ),
                  aes( x = y0c), inherit.aes = F, color = "red")










