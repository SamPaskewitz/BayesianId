data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  real Ymean; // sample mean of response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix (no intercept, numeric vars centered)
  vector[K] Xcol_scale; // factors for converting scaled effects
  int prior_only;  // should the likelihood be ignored?
  real<lower=0> prior_scale; // scale of prior distribution on standarized coefficients (delta's)
  int<lower=1> Nncens; // number of not censored data points
  int<lower=1> Ncens; // number of censored data points
  real Ymax; // sample max of response variable (censoring point)
  array[Nncens] int which_ncens; // indicates non-censored data
  array[Ncens] int which_cens; // indicates censored data
}
transformed data {
  vector[Nncens] Yncens;  // response variable (non-censored data)
  matrix[Nncens, K] Xncens; // design matrix for non-censored data
  matrix[Ncens, K] Xcens; // design matrix for censored data
  Yncens = Y[which_ncens];
  Xncens = X[which_ncens,];
  Xcens = X[which_cens,];
}
parameters {
  vector[K] delta;  // standardized coefficients
  real delta0;  // standardized intercept
  real<lower=0> sigma;  // dispersion parameter
}
transformed parameters {
  vector[K] b;  // actual regression coefficients
  real b0;  // actual intercept
  b = sigma*delta./Xcol_scale;
  b0 = Ymean + sigma*delta0;
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += normal_id_glm_lpdf(Yncens | Xncens, b0, b, sigma); // non-censored data
    target += normal_lccdf(Ymax | b0 + Xncens*b, sigma); // censored data
  }
  // priors including constants
  target += cauchy_lpdf(delta0 | 0, prior_scale);
  target += cauchy_lpdf(delta | 0, prior_scale);
  target += -2*log(sigma);
}
