data {
  int<lower=1> N;  // total number of observations
  vector<lower=0>[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix (no intercept, numeric vars centered)
  vector[K] Xcol_scale; // factors for converting scaled effects
  int prior_only;  // should the likelihood be ignored?
  real<lower=0> prior_scale; // scale of prior distribution on standarized coefficients (delta's)
}
transformed data {
  vector[N] log_Y;
  log_Y = log(Y);
}
parameters {
  real b0;  // actual intercept
  vector[K] b;  // actual regression coefficients
  real<lower=0> sigma;  // dispersion parameter
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += normal_id_glm_lpdf(log_Y | X, b0, b, sigma);
  }
  // priors including constants
  target += cauchy_lpdf(b | 0, sigma*prior_scale./Xcol_scale);
  target += -2*log(sigma);
}
