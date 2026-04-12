data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix (no intercept, numeric vars centered)
  vector[K] Xcol_scale; // factors for converting scaled effects
  int prior_only;  // should the likelihood be ignored?
  real<lower=0> prior_scale; // scale of prior distribution on standarized coefficients (delta's)
  int<lower=1> N_trials; // number of trials for binomial regression
}
parameters {
  real b0;  // actual intercept
  vector[K] b;  // actual regression coefficients
}
model {
  // likelihood including constants
  vector[N] mu; // linear predictor
  mu = b0 + X*b;
  if (!prior_only) {
    target += binomial_logit_lpmf(Y | N_trials, mu);
    //target += binomial_logit_glm_lpmf(Y | N_trials, X, b0, b); // binomial_logit_glm_lpmf is not yet available for the version of Stan compatible with rstan (so far as I can figure out)
  }
  // priors including constants
  target += cauchy_lpdf(b | 0, 1.81*prior_scale./Xcol_scale);
}
