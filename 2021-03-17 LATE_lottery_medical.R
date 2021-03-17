

# 
library(tidyverse)

lottery <- haven::read_dta("data/lottery.dta")

# OLS
lottery %>% lm( lnw ~ d , data = .) %>% summary()

# Assess instrument relevance

first_stage <- lottery %>% lm( d ~ z, data = . )

summary(first_stage)

# Calculate return attending m.school.

(mean(lottery$lnw[lottery$z == 1]) - mean(lottery$lnw[lottery$z == 0]))/(mean(lottery$d[lottery$z == 1]) - mean(lottery$d[lottery$z == 0]))

lottery %>% estimatr::iv_robust( lnw ~ d|z, data = . , se_type = "stata")

lottery %>% estimatr::iv_robust( lnw ~ factor(year) + female + factor(lotcateg) +  d|z +factor(year) + female + factor(lotcateg), data = . , se_type = "stata")


# Count the number of compliers

lottery %>% 
    group_by(d,z) %>% 
    summarise( y = sum(lnw), n = n(), snitt = y/n)

mean(lottery$d[lottery$z == 1])-mean(lottery$d[lottery$z == 0])

0.07+0.41+0.52

nrow(lottery)*0.52








