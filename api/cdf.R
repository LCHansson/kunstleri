cdf_line <- function(cdf_func, typical_range, max_range, typical_range_weight) {
  cdf_func()
}




gompertz <- function(x, A, B, C) {
  A * exp(-B * exp(-C * x))
}

generalized_logistic <- function(x, A, k, x0, nu) {
  A / (1 + exp(-k * (x - x0)))^nu
}

weibull <- function(x, lambda, k, divisor = max(x)) {
  (1 - exp(-((x / divisor) / lambda)^k) ) * divisor
}


## Tests ----
x_max <- 500
x_median <- 350

# Example parameters
A <- 1    # Upper asymptote
B <- 2    # Horizontal shift
C <- 0.5  # Growth rate

# Evaluate the Gompertz function
x_vals <- seq(0, 10, by = 0.1)
y_vals <- gompertz(x_vals, A, B, C)
plot(y_vals, main = "Gompertz")

# Example parameters
A <- 1750     # Upper asymptote
# k <- 0.1     # Growth rate
k <- log(99) / (x_max - x_median) # Growth rate
x0 <- x_median    # Inflection point
nu <- 0.5  # Asymmetry (nu < 1 for a fatter right tail)

# Evaluate the generalized logistic function
x_vals <- seq(0, x_max, by = 1)
y_vals <- generalized_logistic(x_vals, A, k, x0, nu)
plot(y_vals, main = "Generalized logistic")


# Example parameters
lambda <- 350  # Scale parameter
k <- 10     # Shape parameter for fatter tail

# Evaluate the Weibull CDF
x_vals <- seq(0, x_max, by = 1)
y_vals <- weibull(x_vals, lambda, k, divisor = 1)
plot(y_vals, main = "Weibull")


# Parameters
A_gomp <- 1; B_gomp <- 3; C_gomp <- 1.2
A_gen <- 1; k_gen <- 4; x0_gen <- 5; nu_gen <- 0.9
lambda_weibull <- 6; k_weibull <- 2.5

# Generate x values
x_vals <- seq(0, 10, by = 0.1)

# Evaluate the functions
y_gompertz <- gompertz(x_vals, A_gomp, B_gomp, C_gomp)
y_logistic <- generalized_logistic(x_vals, A_gen, k_gen, x0_gen, nu_gen)
y_weibull <- weibull(x_vals, lambda_weibull, k_weibull)

# Plot
plot(x_vals, y_gompertz, type="l", col="blue", ylim=c(0, 1), lwd=2,
     ylab="S-curve", xlab="x", main="Comparison of S-curve Functions")
lines(x_vals, y_logistic, col="red", lwd=2)
lines(x_vals, y_weibull, col="green", lwd=2)

legend("bottomright", legend=c("Gompertz", "Generalized Logistic", "Weibull CDF"),
       col=c("blue", "red", "green"), lwd=2)


