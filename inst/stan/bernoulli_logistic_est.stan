data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix (no intercept, numeric vars centered)
  vector[K] Xcol_scale; // factors for converting scaled effects
  int prior_only;  // should the likelihood be ignored?
  real<lower=0> prior_scale; // scale of prior distribution on standarized coefficients (delta's)
}
parameters {
  real b0;  // actual intercept
  vector[K] b;  // actual regression coefficients
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += bernoulli_logit_glm_lpmf(Y | X, b0, b);
  }
  // priors including constants
  target += cauchy_lpdf(b | 0, 1.81*prior_scale./Xcol_scale);
}
