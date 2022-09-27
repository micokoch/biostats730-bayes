## Pruebas

norm.dens <- function(x, s, m){
  p.x = (1/(s*sqrt(2*pi))) * exp(-.5*(((x-m)/s)^2))
  return(p.x)
}

low <- norm.dens(-3, 1, 0)
high <- norm.dens(3, 1, 0)
mitad <- norm.dens(0, 1, 0)
area <- mitad * 3

set.seed(1234)
(theta <- sample.int(100, 10))
(r <- sample.int(20, 10))
(z <- (theta + r))

(theta.sum <- sum(theta))
(r.sum <- sum(r))
(y.sum <- (theta.sum + r.sum))
(z.sum <- sum(z))

(theta.bar <- mean(theta))
(r.bar <- mean(r))
(y.bar <- (theta.bar + r.bar))
(z.bar <- mean(z))

### HW 3

post.iqs %>% 
  ggplot(aes(x = b_Intercept)) + 
  geom_histogram(bins=100) + 
  theme_bw()

## Lucas code
ratio_hat <- (posterior_samples(fit,   pars= 'b_Intercept')/posterior_samples(fit,   pars= 'sigma'))[[1]]
mean(ratio_hat)
quantile(ratio_hat,c(.1,.9)) 


fit_df <- as_draws_df(fit)
x <- mutate_variables(fit_df, ratio = b_Intercept/sigma)
x <- subset_draws(x, 'ratio')
posterior_summary(x, probs = c(.1,.9), variable= 'ratio')

summarise_draws(x)

## INLA - https://www.r-inla.org/
# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
inla.upgrade() # for the stable version

# Test example
library(INLA)
n = 100; a = 1; b = 1; tau = 100
z = rnorm(n)
eta = a + b*z

scale = exp(rnorm(n))
prec = scale*tau
y = rnorm(n, mean = eta, sd = 1/sqrt(prec))


data = list(y=y, z=z)
formula = y ~ 1+z
result = inla(formula, family = "gaussian", data = data)

summary(result)
