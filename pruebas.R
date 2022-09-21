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


