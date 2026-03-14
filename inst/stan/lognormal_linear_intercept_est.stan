data {
  int<lower=1> N;  // total number of observations
  vector<lower=0>[N] Y;  // response variable
  int prior_only;  // should the likelihood be ignored?
  real<lower=0> prior_scale; // scale of prior distribution on standarized coefficients
}
transformed data {
  vector[N] log_Y;
  log_Y = log(Y);
}
parameters {
  real b0;  // actual intercept
  real<lower=0> sigma;  // dispersion parameter
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += normal_lpdf(log_Y | b0, sigma);
  }
  // priors including constants
  target += -2*log(sigma);
}
