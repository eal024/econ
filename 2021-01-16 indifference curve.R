
library(tidyverse)
library(Ryacas0)

# https://cran.r-project.org/web/packages/Ryacas0/index.html
# https://www.andrewheiss.com/blog/2019/02/16/algebra-calculus-r-yacas/

# Yacas variables
x <- Sym("x")
y <- Sym("y")
U <- Sym("U")

total <- 45
price_x  <- 3
price_y <- 1.5

n_pizza <- total/price_x
n_yougurt <- total/price_y

slope <- -( n_yougurt/n_pizza)

budget <- function(x) {(slope * x) + n_yougurt}


ggplot( data = tibble( x = 0:5, y = budget(x) ), aes(x, y)) +
  geom_line()

g <- ggplot(data = tibble(x = 0:5, y = budget(x)), aes(x, y)) +
  geom_line()

utility <- function(x, y){ x^2*(0.25*y) }
utility_u <- function(x, y){ x^2*(0.25*y) }

utility(x = 5, y = 5)

utility_solved <- Solve( utility(x, y) == U, y)

Eval(utility_solved, list(x = 5, U = 10))
Eval(utility_solved, list(x = 1:5, U = 10))

## wraping this into a function
utility <- function(my_x, my_U) {

  solved <- Solve(utility_u(x, y) == U, y)
  solution <- Eval(solved, list(x = my_x, U = my_U))
  # Regex extract number from yacas expression
  as.numeric(str_extract(solution, "-?[0-9]\\d*(\\.\\d+)?"))
}

utility(5, 10)
utility(1:5, 10)

x_max <- 25
tibble(x = 0:x_max, y = budget(x)) %>%
  ggplot( aes(x = x, y = y)) +
  geom_line( ) +
  geom_line(data = tibble(x = seq(from = 0, to = x_max, by = 0.1), y = utility(x, 40)),
              aes(y = y, x = x),inherit.aes = F) +
  geom_line(data = tibble(x = seq(from = 0, to = x_max, by = 0.1), y = utility(x, 150)),
            aes(y = y, x = x),inherit.aes = F) +
  ggplot2::lims( x = c(0,x_max), y = c(0,40))


# The optimum
# MRS = MRT

my_x <- deriv(utility_u( x,y), x)
my_y <- deriv(utility_u(x,y),y)

# MRS
Simplify( my_x / my_y)

# Solution
Solve( paste(Simplify(my_x/my_y), "==", price_x, "/", price_y), y)

# Into function
marginal_u <- function(my_x) {

  mux_muy <- Simplify( deriv(utility_u(x,y),x) / deriv(utility_u(x,y), y))
  mux_muy_price <- Solve(paste(mux_muy, "==", price_x,"/", price_y), y)
  solution <- Eval( mux_muy_price, list(x = my_x))
  as.numeric( str_extract( solution, "-?[0-9]\\d*(\\.\\d+)?"))

}

# uniroot to see were the function are the same (0-100) -> the rage to look at
optimal_x <- uniroot(function(x) {budget(x) - marginal_u(x)}, c(0,100) )$root

optimal_y <- budget(optimal_x)

max_utility <- utility_u(optimal_x, optimal_y)

graph <- tibble(x = 0:x_max, y = budget(x)) %>%
  ggplot( aes(x = x, y = y)) +
  geom_line( ) +
  geom_line(data = tibble(x = seq(from = 0,
                                  to = x_max, by = 0.1), y = utility(x, max_utility)),
            aes(y = y, x = x),inherit.aes = F) +
  ggplot2::lims( x = c(0,x_max), y = c(0,40))


graph + theme_minimal() + theme(axis.line = element_line(color = "black")) +
  labs(y = "goods y", x = "goods x")

















