

# Propensity score replica Causal inference

# Dehejia and Wahba (1999)

# The control group data sets from CPS and PSID as Lalonde (1986)
library(tidyverse)

nsw_dw <- haven::read_dta("https://raw.github.com/scunning1975/mixtape/master/nsw_mixtape.dta")


mean1<- nsw_dw %>% filter( treat == 1) %>% pull(re78) %>% mean()
mean0<- nsw_dw %>% filter( treat == 0) %>% pull(re78) %>% mean()

# the average treatment effect from the actual experiment.
mean1-mean0


# variants of average treatment effect on the treatment group or the average treatment effect on the untreated group
# use the non-experimental control group
# the control group now consists of a random sample of Americans from that time period. Thus, the control group suffers
# from extreme selection bias since
# most Americans would not function as counterfactuals for the distressed group of workers who selected into the NSW program.


nsw_dw_cpscontrol <- haven::read_dta("https://raw.github.com/scunning1975/mixtape/master/cps_mixtape.dta")


nsw_expr_control <- bind_rows(nsw_dw_cpscontrol, nsw_dw) %>%
  mutate(
    age_2 = age ^ 2,
    age_3 = age ^ 3,
    educ_2 = educ ^ 2,
    u74 = ifelse(re74 == 0, 1 , 0),
    u75 = ifelse(re75 == 0 , 1, 0),
    interaction = educ*re74,
    re74_2 = re74^2,
    re75_2 = re75^2,
    interaction_u_hisp = u74*hisp )

var <- c("treat", "educ" , "educ_2", "marr", "nodegree", "hisp" )

logit_nsw <- nsw_expr_control %>%
  select( var,  contains("age"), contains("re"), contains("u7"), treat, contains("interaction")) %>%
  glm( treat  ~ . ,family = binomial(link = "logit"), data = . )


nsw_expr_control$pscore = logit_nsw$fitted.values

# we used the estimated coefficients from that logit regression to estimate the conditional
# probability of treatment, assuming that probabilities are based on the cumulative logistic distribution:


# Histrogram
nsw_expr_control %>%
  ggplot( aes(  x = pscore, fill = factor(treat), color = factor(treat) ) ) +
  geom_histogram() +
  facet_wrap( ~treat)
