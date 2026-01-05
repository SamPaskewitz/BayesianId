data {
  int<lower=1> N_tilde;  // total number of observations
  int<lower=1> K;  // number of population-level effects
}
parameters {
  real b0;  // actual intercept
}
transformed parameters {
  vector[N_tilde] mu;
  mu = rep_vector(b0, N_tilde);
}
generated quantities {
  array[N_tilde] int Y_tilde;  // simulated response variable
  Y_tilde = bernoulli_logit_rng(mu);
}
