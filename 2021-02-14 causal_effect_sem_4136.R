
library(tidyverse)
# Create data that reflects observed data
# You want to estimate T on Y, condition on covariats x.
# Assume linear in T and x.

# a) No treatment effect
df <- tibble( x = seq(1:100),
              y = x*0.5 + rnorm(n = 100, mean = 10, sd = 5),
              treat = ifelse( x %% 2 == 1, "T", "No") %>% as_factor()
              )
  
# No treatment effect
df %>% ggplot( aes(x = x, y = y, fill = treat, color = treat, shape = treat ) )+ geom_point()
