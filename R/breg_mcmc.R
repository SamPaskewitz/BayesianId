#' Fit a Bayesian regression model using Stan's MCMC algorithm.
#'
#' @param formula An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param data A data frame containing the variables in the model.
#' @param family A string describing the response distribution and link function to be used in the model (see 'Details').
#' @param center Should the numeric predictor variables be mean-centered?
#' @param prior_scale Scale for the prior distribution on model coefficients (see 'Details').
#' @param seed Random seed (set manually for reproducible results).
#' @param chains Number of MCMC chains (see rstan documentation).
#' @param iter Number of MCMC iterations (see rstan documentation).
#' @param warmup Number of warmup/burnin iterations (see rstan documentation).
#' @returns A fitted Bayesian regression model (of class "breg").
#' @details
#' \subsection{Available Models}{
#' ADD THIS
#' }
#' \subsection{The Fitted Model Object}{
#' ADD THIS
#' }
#' \subsection{Prior Distributions}{
#' The residual variance (\eqn{\sigma^2}) has a non-informative Jefferys prior.
#' The code does not directly put priors on the intercept (\eqn{\beta_0}) or regression coefficients (\eqn{\beta_1, \beta_2, \ldots}). Instead it gives priors to standardized versions of the intercept (\eqn{\delta_0}) and coefficients (\eqn{\delta_1, \delta_2, \ldots}):
#' \deqn{\delta_0 \sim \text{Cauchy}(0, r)}
#' \deqn{\delta_i \sim \text{Cauchy}(0, r) \text{ for } i = 1, 2, \ldots}
#' where \eqn{r} is given by the prior_scale function argument.
#'
#' For models based on a normal likelihood (or related models, e.g. with censored data), these standardized parameters are defined as follows:
#' \deqn{\delta_0 = \frac{\beta_0 - \bar{y}}{\sigma}}
#' \deqn{\delta_i = \frac{\beta_i \text{SD}(x_i)}{\sigma} \text{ if } x_i \text{ is a real or integer predictor}}
#' \deqn{\delta_i = \frac{\beta_i}{\sigma} \text{ if } x_i \text{ is a contrast code for a factor}}
#' Note that the denominator for these standardized coefficients is not \eqn{\text{SD}(y)} (as in the standardized coefficients ordinarily reported), but instead \eqn{\sigma} (the residual standard deviation). The \eqn{\delta}'s are thus more closely related to Cohen's d.
#'
#' For logistic, binomial, and ordered logistic regression models the standardized parameters are defined as:
#' \deqn{\delta_0 = \frac{\beta_0 - \text{logit}(\bar{y})}{1.81}}
#' \deqn{\delta_i = \frac{\beta_i \text{SD}(x_i)}{1.81} \text{ if } x_i \text{ is a real or integer predictor}}
#' \deqn{\delta_i = \frac{\beta_i}{1.81} \text{ if } x_i \text{ is a contrast code for a factor}}
#' Why divide by 1.81? The latent variable interpretation of logistic regression (and related models) states that:
#' \deqn{y = 1 \text{ if } u > 0}
#' where \eqn{u} is a latent variable with a standard logistic distribution. The standard logistic distribution has the following standard deviation: \eqn{\text{SD}(u) = \pi/\sqrt{3} \approx 1.81}. Thus the standardization above is consistent with the standardization for normal models: it is a generalization of Cohen's d to the latent scale. ** GIVE REFS
#' }
#' @export
breg_mcmc = function(formula, data, family = "normal_linear", center = TRUE, prior_scale = 1.0, seed = NA, chains = 4, iter = 10000, warmup = floor(iter/4)){
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
    sample_mean = mean(stan_data$Y)
    sample_sd = sd(stan_data$Y)
    init = function(){
      list(sigma = runif(1, min = 0.5, max = 1.5)*sample_sd,
           b0 = runif(1, min = 0.5, max = 1.5)*sample_mean)
    }
  } else{
    init = "random"
  }

  # ** fit the model **
  stanfit = rstan::sampling(stan_model_to_use,
                            data = stan_data,
                            seed = seed,
                            chains = chains,
                            iter = iter,
                            warmup = warmup,
                            refresh = 0, # don't print annoying updates
                            init = init
                            )

  # ** save parameter draws (with original names) for posterior predictive simulations **
  par_names = c("b0")
  if(!intercept_only){
    n_coef = ncol(stan_data$X)
    par_names = c(par_names, paste0("b[",1:n_coef,"]"))
  }
  if(family %in% c("normal_linear", "right_censored_linear")){
    par_names = c(par_names, "sigma")
  }
  b_draws_matrix = as.matrix(stanfit)[,par_names]

  # ** rename parameters **
  names(stanfit)[names(stanfit) == "b0"] = "Intercept"
  if(!intercept_only){
    coef_names = colnames(stan_data$X)
    names(stanfit)[names(stanfit) %in% paste0("b[",1:n_coef,"]")] = coef_names
  } else{
    coef_names = NULL
  }

  # ** assemble a "breg_mcmc" object **
  output = list(stanfit = stanfit,
                model_name = model_name,
                intercept_only = intercept_only,
                formula = formula,
                formula_info = formula_info,
                stan_data = stan_data,
                data = data,
                center = center,
                coef_names = coef_names,
                call = match.call(), # this records the function call, so the default "update" method works (I don't need to write my own version)
                b_draws_matrix = b_draws_matrix)
  class(output) = "breg_mcmc"
  return(output)
}
