#' Check data to make sure that it is suitable for the specified model type (family).
check_data = function(stan_data, family, n_trials){
  if(family == "lognormal_linear"){
    if(any(stan_data$Y <= 0)){
      stop("The predicted variable (y) must be strictly positive for log-linear regression.")
    }
  } else if(family == "bernoulli_logistic"){
    if(any(!stan_data$Y %in% c(0, 1))){
      stop("The predicted variable (y) must be binary for logistic regression.")
    }
  } else if(family == "binomial_logistic"){
    if(any(stan_data$Y < 0)){
      stop("The predicted variable (y) must be non-negative for binomial regression.")
    }
    if(any(!is.wholenumber(stan_data$Y))){
      stop("The predicted variable (y) must be integer-valued for binomial regression.")
    }
    if(is.null(n_trials)){
      stop("In binomial regression, the number of trials must be specified using the argument 'n_trials'.")
    }
    if(any(stan_data$Y > n_trials)){
      stop("The predicted variable (y) cannot be greater than n_trials in binomial regression.")
    }
  } else if(family == "poisson"){
    if(any(stan_data$Y < 0)){
      stop("The predicted variable (y) must be non-negative for Poisson regression.")
    }
    if(any(!is.wholenumber(stan_data$Y))){
      stop("The predicted variable (y) must be integer-valued for Poisson regression.")
    }
  }
}
