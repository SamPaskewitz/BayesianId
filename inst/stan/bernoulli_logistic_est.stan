data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  real Ymean; // sample mean of response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix (no intercept, numeric vars centered)
  vector[K] Xcol_scale; // factors for converting scaled effects
  int prior_only;  // should the likelihood be ignored?
  real<lower=0> prior_scale; // scale of prior distribution on standarized coefficients (delta's)
}
parameters {
  vector[K] delta;  // standardized coefficients
  real delta0;  // standardized intercept
}
transformed parameters {
  vector[K] b;  // actual regression coefficients
  real b0;  // actual intercept
  b = delta./Xcol_scale;
  b0 = logit(Ymean) + delta0;
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += bernoulli_logit_glm_lpmf(Y | X, b0, b);
  }
  // priors including constants
  target += cauchy_lpdf(delta0 | 0, prior_scale);
  target += cauchy_lpdf(delta | 0, prior_scale);
}
