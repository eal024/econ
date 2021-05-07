

# packages and data
library(tidyverse)
female_labour <-haven::read_dta("data/femalelabour.dta")

# Observation over T
female_labour %>% 
  group_by( year ) %>% 
  count() %>%
  ungroup( ) %>% 
  mutate( andel = n/sum(n)) %>% 
  ggplot( aes( x = year, y = andel)) + geom_col( )


# How many individuals?
female_labour %>% summarise( unike = n_distinct(nr))
female_labour %>%  group_by(nr) %>% add_count() %>% group_by(year,n ) %>% count() %>% arrange( year ) %>% pivot_wider( names_from = n, values_from = nn)

# Unbalaned panel
female_labour %>%  group_by(nr) %>% add_count() %>% group_by(n) %>% count() %>% ggplot(aes(y = nn, x =n)) + geom_col() + labs( title = "numer of individ that has N# observation", y = "freq", x = "nr#") 


# Models
tibble( names = names(female_labour), label = map_chr( female_labour, ~attr(.x, "label") )) %>% head(20) 
pooled_ols <- female_labour %>% lm( wage ~ mar + black + hisp + school + exper + ex2  +  , data = . )

