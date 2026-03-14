data {
  int<lower=1> N_tilde;  // total number of observations
  int<lower=1> K;  // number of population-level effects
}
parameters {
  real b0;  // actual intercept
}
generated quantities {
  vector[N_tilde] mu; // linear predictor
  array[N_tilde] int<lower=0> Y_tilde;  // simulated response variable
  mu = rep_vector(b0, N_tilde);
  Y_tilde = poisson_log_rng(mu);
}
