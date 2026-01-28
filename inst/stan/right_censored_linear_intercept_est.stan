data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  real Ymean; // sample mean of response variable
  int prior_only;  // should the likelihood be ignored?
  real<lower=0> prior_scale; // scale of prior distribution on standarized coefficients (delta's)
  int<lower=1> Nncens; // number of not censored data points
  int<lower=1> Ncens; // number of censored data points
  real Ymax; // sample max of response variable (censoring point)
  array[Nncens] int which_ncens; // indicates non-censored data
}
transformed data {
  vector[Nncens] Yncens;  // response variable (non-censored data)
  Yncens = Y[which_ncens];
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
    target += normal_lpdf(Yncens | b0, sigma); // non-censored data
    target += Ncens*normal_lccdf(Ymax | b0, sigma); // censored data
  }
  // priors including constants
  target += cauchy_lpdf(delta0 | 0, prior_scale);
  target += -2*log(sigma);
}
