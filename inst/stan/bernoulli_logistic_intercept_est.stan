data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  real Ymean; // sample mean of response variable
  int prior_only;  // should the likelihood be ignored?
  real<lower=0> prior_scale; // scale of prior distribution on standarized coefficients
}
parameters {
  real delta0;  // standardized intercept
}
transformed parameters {
  real b0;  // actual intercept
  b0 = logit(Ymean) + 1.81*delta0;
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += bernoulli_logit_lpmf(Y | b0);
  }
  // priors including constants
  target += cauchy_lpdf(delta0 | 0, prior_scale);
}
