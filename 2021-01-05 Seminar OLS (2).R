

library(tidyverse)


set.seed(42)
N <- 400
x <- sample( c(0,1), size = N, replace = T, prob = c(0.5,0.5))
e <- rnorm(n = N, mean = 0, sd = 2)

df <- tibble( x = x, y = 1 + 1*x + e)


# a) Estimate B, var and var(e) -------------------------------------------

# https://economictheoryblog.com/2016/02/20/rebuild-ols-estimator-manually-in-r/

mX <-   as.matrix(bind_cols(1 ,df$x))
mY <- as.matrix((df$y))

# Beta = ((X'X)⁻1)X'Y
beta <- solve( t(mX)%*%mX)%*%t(mX)%*%mY

res <- as.matrix(mY - beta[1]-beta[2]*mX[,2])

Var_cov <- 1/(N-1)*as.numeric(t(res)%*%res)*solve(t(mX)%*%mX)

# Calculate the Standard errors:
se <- sqrt(diag(Var_cov))

# p-values
p_values <- rbind(2*pt(abs(beta[1]/se[1]), df = N-1, lower.tail = F),
                  2*pt(abs(beta[2]/se[2]), df = N-1, lower.tail = F)
                  )

output <- as.data.frame( cbind(c("Intercept", "height"),
                               beta, se, p_values)
                         )

names(output) <- c("Coeff:", " Estimate: ", "Std. Error", "Pr(>|t|")


# Verify the matrix-calculation.
df %>% lm( y ~ x, data = .) %>% summary()


# b) hypothesis testing ---------------------------------------------------

# i) Test B1 differ from 1
# False: cant reject that B1 different from 1.
# Alfa/2 : Two-side test:
a <- 0.05/2
((beta[2]-1)/se[2]) > qt(1-a, N-1)

# ii) Test that B0 + B1 is different from 1:
# H0 = B0 + 1 = 1
var_B0_B1 <- (se[1])^2 + se[2]^2 - 2*Var_cov[1,2]
# True: Differ from 1
(beta[1] + beta[2] -1)/sqrt(var_B0_B1) > qt(1-a, N-1)

# Wald test
aod::wald.test((beta[1] + beta[2]) , Sigma =  sqrt(var_B0_B1), Terms =1 )



# d) Monte Carlo - test for Type 1 and Type 2 errors ----------------------


create_data <- function(n){ tibble( x = sample(c(0,1), size = n ,replace = T, prob = c(0.5,0.5)),
                                    y = 1 + 1*x+ rnorm(n = n, 0 , 1)
                                    ) }
df <- create_data(100)

koeff <- df %>% lm( y ~ x, data =.) %>% summary() %>% coefficients()

koeff[2,1]-1/koeff[2,2]

create_many <- function( number ) {

  data_list <- list();
  for(i in 1:number){
    data_list[[i]] <- create_data(400);
  }

  return(data_list)
}

N_s <- 1000

#koff <- compose(coefficients ,summary,lm)

df_many_data <- tibble( index = 1:N_s, data = create_many(N_s) )


df_power <- df_many_data %>%
  mutate( kof = map( data, ~.x %>% lm(y ~x, data =.) %>% summary() %>% coefficients() ),
          recj = map(kof, ~  ((.x[2,1]-1)/.x[2,2]) > qt(0.95,  399)   )
          ) %>%
  unnest( recj)

# 5 % Prop. of rejecting B differ form 1, when its true
sum( df_power$recj)/N_s


































