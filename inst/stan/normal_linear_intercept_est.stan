data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  real Ymean; // sample mean of response variable
  int prior_only;  // should the likelihood be ignored?
  real<lower=0> prior_scale; // scale of prior distribution on standarized coefficients
}
parameters {
  real delta0;  // standardized intercept
  real<lower=0> sigma;  // dispersion parameter
}
transformed parameters {
  real b0;  // actual intercept
  b0 = Ymean + sigma*delta0;
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += normal_lpdf(Y | b0, sigma);
  }
  // priors including constants
  target += cauchy_lpdf(delta0 | 0, prior_scale);
  target += -2*log(sigma);
}
