data {
  int<lower=1> N_tilde;  // total number of observations
  int<lower=1> K;  // number of population-level effects
  matrix[N_tilde, K] X_tilde;  // population-level design matrix (no intercept)
}
parameters {
  vector[K] b;  // actual regression coefficients
  real b0;  // actual intercept
  real<lower=0> sigma;  // dispersion parameter
}
//transformed parameters {
//  vector[N_tilde] mu;
//  mu = b0 + X_tilde*b; // linear predictor
//}
generated quantities {
  vector[N_tilde] mu; // linear predictor
  array[N_tilde] real Y_tilde;  // simulated response variable
  mu = b0 + X_tilde*b;
  Y_tilde = normal_rng(mu, sigma);
}
