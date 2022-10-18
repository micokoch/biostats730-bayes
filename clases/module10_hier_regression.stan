data {
  int<lower=1> N;
  int<lower=1> J; // number of counties
  int<lower=1,upper=J> county_id[N];
  vector[N] x;
  vector[N] y;
}
parameters {
  vector[J] eta; // using a stan-preferred non-centered parametrization 
  real beta;
  real mu_alpha;
  real<lower=0> sigma_y;
  real<lower=0> sigma_alpha;
}
model {
  vector[N] mu;
  
  //comment: vectorized statements are more efficient in stan
  // but using a loop here to make code easy to follow
  for (i in 1:N) 
    mu[i] = mu_alpha + eta[county_id[i]] + x[i] * beta;
  
  eta ~ normal(0, sigma_alpha); 
  
  // prior choice to be discussed 
  mu_alpha ~ normal(0, 1);
  beta ~ normal(0, 1);
  sigma_y ~ normal(0, 1);
  sigma_alpha ~ normal(0, 1);

  y ~ normal(mu, sigma_y);
}