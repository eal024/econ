

# Sampling

# uniform
df <- tibble(
    uniform = runif( n = 10, min = -3, max = 3 ),
    uniform2 = sample( seq(from = -3, to = 3, by = 1), size = 10,  replace = T ),
    normal = rnorm( n = 10, mean = 2, sd = 1) 
)

ggplot( df, aes( x = normal)) + 
    geom_histogram( binwidth = 0.5)


# Divide the data into simulare sizes:

df2 <- tibble( a = 1:100, b = seq(1,2) %>% rep( times = 50))

19*5 # = 95
95-4
99%%5
(99-4)%%5
98%%5
(98-3)%%5

# First (downward) number that can be divided by 5
99 %/% 5
19*5

row_index  <- seq_len( nrow(df2))*(nrow(df2)%/%5)

df2 %>% slice(1:4)
df2 %>% slice( row_index)
df2 %>% 
    # Random sample the complete data
    slice_sample(prop = 1) %>% rowid_to_column()



