

# Replica of Dehejia and Wahba - reevaluate Lalonde (1986)
library(tidyverse)

# Data from Dehjia and Wahba (2002)
nsw_dw <- haven::read_dta("https://raw.github.com/scunning1975/mixtape/master/nsw_mixtape.dta")

# 1. ATE for the actual experiment
mean1<- nsw_dw %>% filter( treat == 1) %>% pull(re78) %>% mean()
mean0<- nsw_dw %>% filter( treat == 0) %>% pull(re78) %>% mean()

# Mean diff
mean1-mean0


# Calculate T-effect - e.g. ATT(reated) or ATUT
# we will use the non-experimental control group from the Current Population Survey.
# the control group suffers from extreme selection bias
# since most Americans would not function as counterfactuals

# Data CPS
nsw_dw_cpscontrol <- haven::read_dta("https://raw.github.com/scunning1975/mixtape/master/cps_mixtape.dta")

nsw_dw_cpscontrol <- bind_rows(nsw_dw_cpscontrol, nsw_dw) %>%
  mutate(
    age_2 = age^2,
    age_3 = age^3,
    educ_2 = educ^2,
    u74 = ifelse(re74 == 0, 1 , 0),
    u75 = ifelse(re75 == 0 , 1, 0),
    interaction = educ*re74,
    re74_2 = re74^2,
    re75_2 = re75^2,
    interaction_u_hisp = u74*hisp
    )

logit_nsw <- nsw_dw_control %>%
  glm( treat  ~ age + age_2 + age_3 + educ + educ_2 + u74 + u75 + re75 + re74 + interaction + marr + nodegree + black + hisp  ,family = binomial(link = "logit"), data = . )

nsw_dw_cpscontrol$pscore = logit_nsw$fitted.values

# we used the estimated coefficients from that logit regression to estimate the conditional
# probability of treatment, assuming that probabilities are based on the cumulative logistic distribution:

# Testing the CIA- Histrogram

nsw_expr_control %>%
  ggplot( aes(  x = pscore, fill = factor(treat), color = factor(treat) ) ) +
  geom_histogram() +
  facet_wrap( ~treat)

# The probability of treatment is spread out across
# the units in the treatment group, but there is a very
# large mass of nearly zero propensity scores in the CPS.
# How do we interpret this? What this means is that the characteristics
# of individuals in the treatment group are rare in the CPS sample.

## Weighting on the PS
# Several ways to estimate the ATT using an PS
# Busso, DiNardo, and McCrary (2014) examined the properties of various approaches and found
# that inverse probability weighting was competitive in several simulations.

# Assuming that CIA holds in our data,
# then one way we can estimate treatment effects is
# to use a weighting procedure in which each individual’s
# propensity score is a weight of that individual’s outcome (Imbens 2000).


##
N <- nrow( nsw_dw_cpscontrol)

# Manual with non-normalized weights using all data
nsw_dw_cpscontrol_1 <-  nsw_dw_cpscontrol %>%
  #
  mutate( d1 = treat/pscore,
          d0 = (1-treat)/(1-pscore)
          )

s1 <- sum(nsw_dw_cpscontrol_1$d1)
s0 <- sum(nsw_dw_cpscontrol_1$d0)

nsw_dw_cpscontrol_2 <- nsw_dw_cpscontrol_1 %>%
  mutate(
          # ATT
          y1 = treat*re78/pscore,
          y0 = (1-treat)*re78/(1-pscore),
          ht = y1 - y0,
          # Normalization of the weights
          y1_norm_w = ( (treat*re78)/pscore)/(s1/N),
          y0_norm_w = ( (1-treat)*re78/(1-pscore) )/(s0/N),
          norm_ = (y1_norm_w - y0_norm_w)
          ) %>%
  select(treat, pscore, y1:norm_)

# Calcuate the mean
nsw_dw_cpscontrol_2 %>% summarise( ht = mean(ht), norm = mean(norm_) )

nsw_dw_cpscontrol_2 %>% pull(ht) %>% mean()
# Why is this so much different than what we get using the experimental data?
 # -> We will need to trim the data. Here we will do a very small trim to eliminate the mass of values at the far-left tail.
 # A good rule of thumb, they note, is to keep only observations on the interval [0.1,0.9], which was performed at the end of the program.

# Trimmed mean
nsw_dw_cpscontrol_trim <- nsw_dw_cpscontrol_1 %>%
  filter( between( pscore, 0.1, 0.9) ) %>%
  mutate( N = n(),
          s1 = sum(d1),
          s0 = sum(d0)
          ) %>%
  mutate(
    # ATT
    y1 = treat*re78/pscore,
    y0 = (1-treat)*re78/(1-pscore),
    ht = y1 - y0,
    # Normalization of the weights
    y1_norm_w = ( (treat*re78)/pscore)/(s1/N),
    y0_norm_w = ( (1-treat)*re78/(1-pscore) )/(s0/N),
    norm_ = (y1_norm_w - y0_norm_w)
  ) %>%
  select(treat, pscore, y1:norm_)

nsw_dw_cpscontrol_trim %>% summarise( ht = mean(ht), norm = mean(norm_) )

nsw_dw_cpscontrol_trim %>% filter( between(pscore, 0.1,0.9)) %>% summarise( ht = mean(ht), norm = mean(norm_) )

# MatchIt -----------------------------------------------------------------

library(MatchIt)
library(Zelig)
#
m_out <- matchit( treat  ~ age + age_2 + age_3 + educ + educ_2 + u74 + u75 + re75 + re74 + interaction + marr + nodegree + black + hisp,
                  data = nsw_dw_control,
                  method = "nearest",
                  distance = "logit", ratio =5)

m_data <- match.data(m_out)

m_data %>%
  filter( between(pscore, 0.1, 0.9)) %>%
  summarise( y  = mean(re78[ treat == 1]) -mean(re78[ treat == 0]) )

z_out <- zelig(re78 ~ treat + age + age_2 + age_3 + educ +
                 educ_2 + marr + nodegree +
                 black + hisp + re74 + re75 + u74 + u75 + interaction,
               model = "ls", data = m_data)

x_out <- setx(z_out, treat = 0)
x1_out <- setx(z_out, treat = 1)

s_out <- sim(z_out, x = x_out, x1 = x1_out)

summary(s_out)

m_data %>%  filter( between(pscore, 0.1, 0.9)) %>% lm( re78 ~ treat, data = .)












