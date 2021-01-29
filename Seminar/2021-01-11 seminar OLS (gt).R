
library(tidyverse)

# 1) The law of large number

# i) Normal distributed
set.seed(1234)

##
data <- tibble( x = rnorm(10000, mean =  4, sd = 2)) %>%
  mutate( x_r = round(x) %>% as.integer())


data %>%
  group_by( x_r) %>%
  count() %>%
  ungroup() %>%
  mutate( a_x = n/sum(n)) %>%
  ggplot( aes(x = x_r, y = a_x)) +
  geom_col() +
  geom_density( data = data, aes( x = x),  color = "red", linetype  = 3, inherit.aes = F) +
  geom_vline( xintercept = 4, color = "red")



## 1 LLN & CLT
# x ~Bernouili(p) p = 0.6 Illustrate law of large numbers
# https://mathworld.wolfram.com/BernoulliDistribution.html

# Bernoulli , p(x) = p(x)^x*(1-p)^(1-x)
# E(x) = P
# Var()
mean(rbinom(n = 100000, size = 1, prob = 0.6  ))

tibble( index = 1:3, se = cumsum(index)/index)
mean(c(1:3))

N <- 1000
tibble( index = 1:N ,x = rbinom(n = N, size = 1, prob = 0.6  )) %>%
  mutate( x_mean_cumsum = cumsum(x)/index) %>%
  ggplot( aes( x = index, y = x_mean_cumsum) ) +
  geom_line( ) +
  geom_hline( yintercept =  0.6 , color = "red", linetype = 2)


# Rlab
tibble( i = rep(10000, times = 5), letter = letters[1:5]  ) %>%
  mutate( data = map(i,  ~tibble( x = Rlab::rbern(n = .x, prob = 0.6)))) %>%
  mutate( data = map(data, function(x) {
    x %>% mutate( index = c(1:10000),
                  x_mean = cumsum(x)/index)
  })) %>%
  unnest(data) %>%
  ggplot( aes( x = index, y = x_mean, fill = letter, color = letter)) +
  geom_line( alpha = 0.5) +
  geom_hline( yintercept = 0.6, linetype = 2)

## ii Chi2(k = 1) Illustrate the Central limit theorem
tibble( x = rchisq( 100, df = 1, ncp = 0 ) ) %>%
  ggplot( aes( x )) + geom_histogram(  )

list_data <- list()
vec_length <- c(1,3,5,10,20,50,100,200,1000)
i <- 0
for(i in 1:length( vec_length )) {
  list_data[[i]] <- list();

  len <- 1000;

    for( j in 1:len){
      list_data[[i]][j] <- list(rchisq(n = vec_length[i], df = 1))
    }
}

tibble( i = 1:length(list_data),
        utvalg = vec_length %>% as.character(),
        data = list_data[i]
        ) %>%
  # sorter etter antall i utvalget
  mutate( utvalg = fct_reorder( utvalg, as.integer(utvalg) )) %>%
  unnest( data) %>%
  arrange( utvalg) %>%
  mutate( snitt = map( data, mean)) %>%
  unnest( snitt) %>%
  ggplot( aes( snitt )) +
  geom_histogram( ) +
  facet_wrap(~utvalg, scales ="free")+
  geom_vline( xintercept =   1, color = "red", linetype = 2)


# OLS matrix --------------------------------------------------------------

# Data
ols_data <- tibble( x = sample(size = 400, c(0,1), prob = c(0.5, 0.5), replace = T),
        e = rnorm(400, mean = 0, sd = 2),
        y = 1 + 1*x + e
        )

model <- ols_data %>% lm( y ~ x, data = .)

model %>% summary()

# a) Estimate B, var(e)
mX <- as.matrix( bind_cols(1, ols_data$x) )
mY <- as.matrix(ols_data$y)

# i) # Beta = ((X'X)⁻1)X'Y
beta <- solve( t(mX)%*%mX)%*%t(mX)%*%mY

res <- as.matrix(mY - beta[1] -beta[2]*mX[,2] )

t(res)%*%res
N <- nrow(ols_data)

# Variance
var_e <- (1/(N-1-1))*as.numeric(t(res)%*%res)
varb <- var_e*solve(t(mX)%*%mX)
var_cov <- 1/(N-1)*as.numeric(t(res)%*%res)*solve(t(mX)%*%mX)
var_cov <- var_e*solve(t(mX)%*%mX)
se <-  sqrt( diag(var_cov))

se_m <- se*solve(t(mX)%*%mX)

# b) t-statistic:
# t = Bhat-BH0/ se(B)
# H0: B0 = 1
(beta[2,]-1)/se[2] > qt( (1-0.05), df = 1)


# ii) Test H0: B0 + B1 = 1
# var(a+b) = var(a) + var(b) - 2cov(a,b)
se_b0b1 <- sqrt(se[1]^2 + se[2]^2 - 2*var_cov[1,2])

#H-test:
((beta[2,] + beta[1,])-1)/se_b0b1 > qt( (1-0.05),  df = 400-1)


# Wald test
R <- as.matrix(bind_cols(1,1))
q <- as.matrix(bind_cols(1,1))




wald <- t(R%*%beta-1)%*% solve( R%*% varb %*%t(R) )*(R%*%beta-1)
car::linearHypothesis(mod.ols_data, c("(Intercept) = x-1"))


test_model <- ols_data %>%  lm( I(y - 1+  1*x) ~ 0 + I(1 + x), data= .) %>% summary()

library(car)
data("Davis")
# Example
mod.davis <- lm(weight ~ repwt, data=Davis)
car::linearHypothesis(mod.davis, c("(Intercept)", "repwt"), c(0,1))
linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"))

mod.ols_data <- ols_data %>% lm( y ~x, data = .)
car::linearHypothesis(mod.ols_data, c("(Intercept)", "x"), c(1,1))
car::linearHypothesis(mod.ols_data, c("(Intercept) = 1", "x = 1"))
car::linearHypothesis(mod.ols_data, c("(Intercept) = x-1"))


# Power -------------------------------------------------------------------

# d) H0: B1 = 1, H1: B1 not 1

# i) prob for rejecting H0, when it is true (Type 1 error)
# ii) prob for reject H0, when true B1 =2, (type 2 error)

# i)

vec_len <- c(1)
list_data <- list()

for(i in 1:length( vec_length )) {
  list_data[[i]] <- list();

  len <- 500;

  for( j in 1:len){
    list_data[[i]][j] <- list(tibble( e = rnorm(len, mean = 0,sd = 2),
                                      x = sample(size = len, c(0,1), replace = T, prob = c(0.5, 0.5)),
                                      y = (1+1*x + e)))
  }
}


# Power
tibble( n = 1:500, data = list_data[[1]]) %>%
  mutate( model = map( data, ~lm( y ~ x, data = .x) %>% summary() %>% coefficients()),
          b1 =map(model, ~.x[2,1]),
          se = map(model, ~.x[2,2]),
          test = map(model, ~(.x[2,1]-1)/.x[2,2]) ) %>%
  unnest( b1, se, test ) %>%
  mutate(  p_value = pnorm(-abs(b1-1) / se)  * 2) %>%
  mutate( ss = ifelse(p_value < 0.05, 1, 0)) %>%
  summarise( N = n(), under_05 = sum(ss),  power = under_05/N )

## ii)  B2 = 2, H0 = 1
vec_len <- c(1)
list_data <- list()
for(i in 1:length( vec_length )) {
  list_data[[i]] <- list();

  len <- 400;

  for( j in 1:len){
    list_data[[i]][j] <- list(tibble( e = rnorm(len, mean = 0,sd = 3),
                                      x = sample(size = len, c(0,1), replace = T, prob = c(0.5, 0.5)),
                                      y = (1+2*x + e)))
  }
}


# Power
tibble( n = 1:400, data = list_data[[1]]) %>%
  mutate( model = map( data, ~lm( y ~ x, data = .x) %>% summary() %>% coefficients()),
          b1 =map(model, ~.x[2,1]),
          se = map(model, ~.x[2,2]),
          test = map(model, ~(.x[2,1]-1)/.x[2,2]) ) %>%
  unnest( b1, se, test ) %>%
  mutate( t = (b1-1)/se,
          p_value = pnorm(-abs(b1-1) / se)  * 2) %>%
  mutate( ss = ifelse(p_value < 0.025, 1, 0)) %>%
  summarise( N = n(), under_05 = sum(ss),  power = under_05/N , type2 = 1-power)

##
vec_len <- c(1)
list_data <- list()
for(i in 1:length( vec_length )) {
  list_data[[i]] <- list();

  len <- 400;

  for( j in 1:len){
    list_data[[i]][j] <- list(tibble( e = rnorm(len, mean = 0,sd = 20),
                                      x = sample(size = len, c(0,1), replace = T, prob = c(0.5, 0.5)),
                                      y = (1+2*x + e)))
  }
}


# Power
tibble( n = 1:400, data = list_data[[1]]) %>%
  mutate( model = map( data, ~lm( y ~ x, data = .x) %>% summary() %>% coefficients()),
          b1 =map(model, ~.x[2,1]),
          se = map(model, ~.x[2,2]),
          test = map(model, ~(.x[2,1]-1)/.x[2,2]) ) %>%
  unnest( b1, se, test ) %>%
  mutate( t = (b1-1)/se,
          p_value = pnorm(-abs(b1-1) / se)  * 2) %>%
  mutate( ss = ifelse(p_value < 0.025, 1, 0)) %>%
  summarise( N = n(), under_05 = sum(ss),  power = under_05/N , type2 = 1-power)


















