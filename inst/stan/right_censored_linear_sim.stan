data {
  int<lower=1> N_tilde;  // total number of observations
  int<lower=1> K;  // number of population-level effects
  matrix[N_tilde, K] X_tilde;  // population-level design matrix (no intercept)
  real Ymax; // upper limit of Y (censoring point)
}
parameters {
  real b0;  // actual intercept
  vector[K] b;  // actual regression coefficients
  real<lower=0> sigma;  // dispersion parameter
}
generated quantities {
  vector[N_tilde] mu; // linear predictor
  array[N_tilde] real Y_tilde;  // simulated response variable
  mu = b0 + X_tilde*b;
  Y_tilde = normal_rng(mu, sigma);
  for (i in 1:N_tilde) { // do the right-censoring
    if (Y_tilde[i] > Ymax) {
      Y_tilde[i] = Ymax;
    }
  }
}
