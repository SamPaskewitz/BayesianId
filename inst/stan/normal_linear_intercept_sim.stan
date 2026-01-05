data {
  int<lower=1> N_tilde;  // total number of observations
}
parameters {
  real b0;  // intercept
  real<lower=0> sigma;  // dispersion parameter
}
transformed parameters {
  vector[N_tilde] mu;
  mu = rep_vector(b0, N_tilde);
}
generated quantities {
  array[N_tilde] real Y_tilde;  // simulated response variable
  Y_tilde = normal_rng(mu, sigma);
}
