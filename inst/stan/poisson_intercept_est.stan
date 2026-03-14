data {
  int<lower=1> N;  // total number of observations
  array[N] int<lower=0> Y;  // response variable
  int prior_only;  // should the likelihood be ignored?
  real<lower=0> prior_scale; // scale of prior distribution on standarized coefficients
}
parameters {
  real b0;  // actual intercept
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += poisson_log_lpmf(Y | b0);
  }
}
