#' Fit a Bayesian regression model using the Laplace (multivariate normal) approximation (with Stan's optimization functionality).
#' @param formula An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param data A data frame containing the variables in the model.
#' @param family A string describing the response distribution and link function to be used in the model (see 'Details').
#' @param center Should the numeric predictor variables be mean-centered?
#' @param prior_scale Scale for the prior distribution on model coefficients (see 'Details').
#' @returns A fitted Bayesian regression model (of class "breg_laplace").
#' @details Currently this uses the Newton algorithm for optimization, because in my experience it more reliably finds the maximum than LBFGS, despite being slower.
#' @export
breg_laplace = function(formula, data, family = "normal_linear", center = TRUE, prior_scale = 1.0){
  # ** get formula info **
  formula_info = parse_formula(formula)

  # ** prepare data frame (centering, contrasts etc.) **
  data = prepare_data(formula_info = formula_info,
                      data = data,
                      center = center)

  # ** set up Stan data **
  stan_data = make_stan_data(formula_info = formula_info, data = data, family = family, prior_scale = prior_scale)

  # ** pick the Stan model to use **
  # NOTE: later this will be more elaborate to deal with mixed effects models etc.
  model_name = family
  intercept_only = !("X" %in% names(stan_data))
  if(intercept_only){
    model_name = paste0(model_name, "_intercept")
  }
  stan_model_to_use = stanmodels[[paste0(model_name, "_est")]]

  # ** set up initialization **
  # this is particularly important with censored data
  if(family %in% c("normal_linear", "right_censored_linear")){
    init = list(sigma = sd(stan_data$Y), b0 = mean(stan_data$Y))
  } else{
    init = "random"
  }

  # ** fit the model **
  optim_fit = rstan::optimizing(stan_model_to_use,
                                data = stan_data,
                                init = init,
                                draws = 9, # needs to be an odd number for ppc_intervals to work properly with binary data (it plots the medians of yrep)
                                hessian = TRUE,
                                algorithm = "Newton")

  # ** rename parameters **
  names(optim_fit$par)[names(optim_fit$par) == "b0"] = "(Intercept)"
  if(!intercept_only){
    n_coef = ncol(stan_data$X)
    coef_names = colnames(stan_data$X)
    names(optim_fit$par)[names(optim_fit$par) %in% paste0("b[",1:n_coef,"]")] = coef_names
  } else{
    coef_names = NULL
  }
  rownames(optim_fit$hessian) = names(optim_fit$par)
  colnames(optim_fit$hessian) = names(optim_fit$par)

  # ** compute the posterior covariance matrix **
  Sigma = solve(-optim_fit$hessian[c("(Intercept)", coef_names), c("(Intercept)", coef_names), drop = FALSE])

  # ** compute log evidence (log marginal likelihood) using the Laplace approximation **
  n_par = nrow(optim_fit$hessian)
  log_evidence = optim_fit$value - 0.5*determinant(optim_fit$hessian, logarithm = TRUE)$modulus[1] + 0.5*n_par*log(2*pi)

  # ** assemble a "breg_laplace" object **
  output = list(optim_fit = optim_fit,
                mu = optim_fit$par[c("(Intercept)", coef_names)], # posterior mean
                sigma = diag(Sigma) |> sqrt(), # posterior SD
                Sigma = Sigma, # posterior covariance matrix
                log_evidence = log_evidence,
                model_name = model_name,
                intercept_only = intercept_only,
                formula = formula,
                formula_info = formula_info,
                stan_data = stan_data,
                data = data,
                center = center,
                coef_names = coef_names,
                call = match.call())
  class(output) = "breg_laplace"
  return(output)
}
