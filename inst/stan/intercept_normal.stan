data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int prior_only;  // should the likelihood be ignored?
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> sigma;  // dispersion parameter
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += normal_lpdf(Y | Intercept, sigma);
  }
  // priors including constants
  target += -2*log(sigma);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
}
