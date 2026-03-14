data {
  int<lower=1> N_tilde;  // total number of observations
  int<lower=1> K;  // number of population-level effects
  matrix[N_tilde, K] X_tilde;  // population-level design matrix (no intercept)
}
parameters {
  real b0;  // actual intercept
  vector[K] b;  // actual regression coefficients
}
generated quantities {
  vector[N_tilde] mu; // linear predictor
  array[N_tilde] int<lower=0> Y_tilde;  // simulated response variable
  mu = b0 + X_tilde*b;
  Y_tilde = poisson_log_rng(mu);
}
