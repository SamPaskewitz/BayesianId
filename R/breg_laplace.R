#' Fit a Bayesian regression model using the Laplace (multivariate normal) approximation.
#' @param formula An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param data A data frame containing the variables in the model.
#' @param family A string describing the response distribution and link function to be used in the model (see 'Details').
#' @param center Should the numeric predictor variables be mean-centered?
#' @param prior_scale Scale for the prior distribution on model coefficients (see 'Details').
#' @param resample Should posterior draws from the multivariate normal approximation to the posterior be resampled using importance sampling?
#' @param n_trials Number of trials per observation for binomial regression (not needed for other model families).
#' @returns A fitted Bayesian regression model (of classes "breg" and "breg_laplace").
#' @details
#' \subsection{More Information}{
#' More information is contained in the following vignettes:
#'- model_families.Rmd: a list of the available types of model and a description of when to use each
#' - basic_linear_regression.Rmd: shows the methods available for "breg" objects
#' - intro-to-bma.Rmd: gives an explanation and example of how to use Bayesian model averaging (BMA) for variable estimation/selection
#' }
#' \subsection{Computational Details}{
#' This function uses Stan's optimization routines to obtain the Laplace (multivariate normal) approximation of the posterior distribution. It uses the same Stan files as breg_mcmc. Currently this function uses the Newton algorithm for optimization, because in my experience it more reliably finds the maximum than LBFGS, despite being slower. Once the model has been fit, parameters are sampled from the multivariate normal approximate posterior distribution (and transformed to the contstrained space when appropriate, e.g. for the residual standard deviation in linear models). These samples can be optionally resampled by importance sampling to improve the approximation. Parameter estimates are NOT based on these samples (this would introduce Monte Carlo error), but posterior predictions do use the samples.
#' }
#' \subsection{Prior Distributions}{
#' The residual variance (\eqn{\sigma^2}) has a non-informative Jeffereys prior.
#' The intercept (\eqn{\beta_0}) has a non-informative uniform prior.
#' The code does not directly put priors on the regression coefficients (\eqn{\beta_1, \beta_2, \ldots}). Instead it gives priors to standardized coefficients (\eqn{\delta_1, \delta_2, \ldots}):
#' \deqn{\delta_i \sim \text{Cauchy}(0, r) \text{ for } i = 1, 2, \ldots}
#' where \eqn{r} is given by the prior_scale function argument.
#'
#' For models based on a normal likelihood (or related models, e.g. with censored data), these standardized parameters are defined as follows:
#' \deqn{\delta_i = \frac{\beta_i \text{SD}(x_i)}{\sigma} \text{ if } x_i \text{ is a real or integer predictor}}
#' \deqn{\delta_i = \frac{\beta_i}{\sigma} \text{ if } x_i \text{ is a contrast code for a factor}}
#' Note that the denominator for these standardized coefficients is not \eqn{\text{SD}(y)} (as in the standardized coefficients ordinarily reported), but instead \eqn{\sigma} (the residual standard deviation). The \eqn{\delta}'s are thus more closely related to Cohen's d.
#'
#' For logistic, binomial, and ordered logistic regression models the standardized parameters are defined as:
#' \deqn{\delta_i = \frac{\beta_i \text{SD}(x_i)}{1.81} \text{ if } x_i \text{ is a real or integer predictor}}
#' \deqn{\delta_i = \frac{\beta_i}{1.81} \text{ if } x_i \text{ is a contrast code for a factor}}
#' Why divide by 1.81? The latent variable interpretation of logistic regression (and related models) states that:
#' \deqn{y = 1 \text{ if } u > 0}
#' where \eqn{u} is a latent variable with a standard logistic distribution. The standard logistic distribution has the following standard deviation: \eqn{\text{SD}(u) = \pi/\sqrt{3} \approx 1.81}. Thus the standardization above is consistent with the standardization for normal models: it is a generalization of Cohen's d to the latent scale.
#' }
#' @md
#' @export
breg_laplace = function(formula, data, family = "normal_linear", center = TRUE, prior_scale = 1.0, resample = FALSE, n_trials = NULL){
  # ** get formula info **
  formula_info = parse_formula(formula)

  # ** prepare data frame (centering, contrasts etc.) **
  data = prepare_data(formula_info = formula_info,
                      data = data,
                      center = center)

  # ** set up Stan data **
  stan_data = make_stan_data(formula_info = formula_info, data = data, family = family, prior_scale = prior_scale, n_trials = n_trials)

  # ** check data **
  check_data(stan_data, family, n_trials)

  # ** pick the Stan model to use **
  # NOTE: later this will be more elaborate to deal with mixed effects models etc.
  model_name = family
  intercept_only = !("X" %in% names(stan_data))
  if(intercept_only){
    model_name = paste0(model_name, "_intercept")
  }
  stan_model_to_use = stanmodels[[paste0(model_name, "_est")]]

  # ** set up initialization (particularly important with censored data) **
  if(family %in% c("normal_linear", "right_censored_linear", "lognormal_linear")){
    init = list(sigma = sd(stan_data$Y), b0 = mean(stan_data$Y))
  } else{
    init = list(b0 = 0.0)
  }
  if(!intercept_only){
    init$b = rep(0.0, stan_data$K) |> as.array() # need "as.array" to prevent Stan from throwing a hissy fit when "b" is of length 1
  }

  # ** fit the model **
  optim_fit = rstan::optimizing(stan_model_to_use,
                                data = stan_data,
                                init = init,
                                draws = 10001,
                                hessian = TRUE,
                                importance_resampling = TRUE,
                                algorithm = ifelse(model_name == "binomial_logistic", "LBFGS", "Newton"))
  # for some reason, the Newton algorithm doesn't work for binomial regression

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

  # ** do importance resampling (optionally) **
  if(resample){
    set.seed(1212) # set seed for consistency
    resample_index = sample(1:10001, size = 10001, replace = TRUE, prob = exp(optim_fit$log_p - optim_fit$log_g))
    draws_matrix = optim_fit$theta_tilde[resample_index,]
  } else{
    draws_matrix = optim_fit$theta_tilde
  }

  # ** compute log evidence (log marginal likelihood) with the Laplace approximation **
    n_par = nrow(optim_fit$hessian)
    log_evidence = optim_fit$value - 0.5*determinant(optim_fit$hessian, logarithm = TRUE)$modulus[1] + 0.5*n_par*log(2*pi)

  # ** assemble a "breg_laplace" object **
  output = list(draws_matrix = draws_matrix,
                post_mean = optim_fit$par[c("(Intercept)", coef_names)], # posterior mean
                post_sd = diag(Sigma) |> sqrt(), # posterior SD
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
  class(output) = c("breg_laplace", "breg")
  return(output)
}
