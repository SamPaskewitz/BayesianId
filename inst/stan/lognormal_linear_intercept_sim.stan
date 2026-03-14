data {
  int<lower=1> N_tilde;  // total number of observations
}
parameters {
  real b0;  // intercept
  real<lower=0> sigma;  // dispersion parameter
}
generated quantities {
  vector[N_tilde] mu; // linear predictor
  array[N_tilde] real<lower=0> Y_tilde;  // simulated response variable
  mu = rep_vector(b0, N_tilde);
  Y_tilde = exp(normal_rng(mu, sigma));
}
