#' Fit a Bayesian regression model using Stan.
#'
#' @param formula **
#' @param data **
#' @param center **
#' @param r **
#' @param seed **
#' @param chains **
#' @param iter **
#' @param warmup **
#' @returns A fitted Bayesian regression model (of class "breg").
#' @details
#' ADD THESE
#'
#' @export
#'
breg = function(formula, data, center = TRUE, prior_scale = 1.0, seed = NA, chains = 4, iter = 5000, warmup = floor(iter/2)){
  # **** FIGURE THIS OUT *****

  # ** set up Stan data **
  stan_data = make_stan_data(formula = formula, data = data, prior_scale = prior_scale, center = center)

  # ** pick the Stan model to use **
  # NOTE: later this will be more elaborate to deal with mixed effects models, GLM's etc.
  intercept_only = !("X" %in% names(stan_data))
  if(!intercept_only){
    stan_model_to_use = stanmodels$normal_linear_est
  } else{
    stan_model_to_use = stanmodels$normal_intercept_est
  }

  # ** fit the model **
  fit = rstan::sampling(stan_model_to_use,
                        data = stan_data,
                        seed = seed,
                        chains = chains,
                        iter = iter,
                        warmup = warmup,
                        refresh = 0 # don't print annoying updates
                        )

  # ** assemble a "breg" object **
  output = list(stanfit = fit,
                formula = formula,
                data = data)
  class(output) = "breg"
  return(output)
}
