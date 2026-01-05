#' Print basic information about a "breg" object.
#' @param obj A "breg" object (fitted model).
#' @export
#' @method print breg
print.breg = function(obj){
  cat("breg model object")
  cat("\nCall:\n")
  print(obj$call)
}

#' Show summary information about a "breg" object.
#' @param obj A "breg" object (fitted model).
#' @details Note that the "se_mean" column represents Monte Carlo standard error, i.e. error due to MCMC estimation. The column "sd" represents posterior uncertainty, and is analogous to standard error in frequentist estimation.
#' @export
#' @method summary breg
summary.breg = function(obj){
  cat("Call:\n")
  print(obj$call)
  cat("\nSampling details and estimates:\n")
  print(obj$stanfit, pars = c("Intercept", obj$coef_names), probs = c(0.025, 0.975), digits_summary = 3)
}

#' Print parameter estimates (posterior means) from a "breg" object.
#' @param obj A "breg" object (fitted model).
#' @param pars Parameters to select. By default these are just the model coefficients (fixed effects), plus the intercept.
#' @importMethodsFrom rstan summary
#' @export
#' @method coef breg
coef.breg = function(obj, pars = c("Intercept", obj$coef_names)){
  sumtab = summary(obj$stanfit, pars = pars)$summary[,"mean"]
  return(sumtab)
}

#' Create various types of plot for model coefficients.
#' @param obj A "breg" object (fitted model).
#' @param type The type of plot. See "details" for a list of available options.
#' @param pars Parameters to select. By default these are just the model coefficients (fixed effects).
#' @details
#' The following plot types are available:
#' \itemize{
#'  \item "post_pred": posterior predictive plot
#'  \item "intervals": posterior means and credible intervals
#'  \item "density": posterior distributions
#'  \item "trace": trace plot for visually assessing MCMC convergence (should look like a "fuzzy caterpillar")
#' }
#' Several of these are based on some of the built-in Rstan plotting functions (https://mc-stan.org/rstan/reference/stan_plot.html).
#' You could use those Rstan plotting functions directly on obj$stanfit for more options and flexibility.
#' Also, note that the results are ggplot2 objects, so could use ggplot2 to modify them (e.g. add a different title).
#' @importFrom rstan stan_plot stan_trace stan_dens
#' @importFrom bayesplot ppc_dens_overlay
#' @export
#' @method plot breg
plot.breg = function(obj, type = "post_pred", pars = obj$coef_names){
  if(type == "post_pred"){
    ppred = posterior_predict(obj, ndraws = 5)
    pt = ppc_dens_overlay(y = obj$data[,obj$formula_info$lhs], yrep = ppred)
  } else if(type == "intervals"){
    pt = stan_plot(obj$stanfit, pars = pars)
  } else if(type == "density"){
    pt = stan_dens(obj$stanfit, pars = pars)
  } else if(type == "trace"){
    pt = stan_trace(obj$stanfit, pars = pars)
  } else{
    stop("Plot type not recognized.")
  }
  return(pt)
}

#' Computes the posterior variance-covariance matrix of model parameters.
#' @param obj A "breg" object (fitted model).
#' @param pars Parameters to select. By default these are just the model coefficients (fixed effects).
#' @returns The posterior variance-covariance matrix.
#' @export
#' @method vcov breg
vcov.breg = function(obj, pars = obj$coef_names){
  samples = as.matrix(fit$stanfit)[,pars]
  if(length(pars) > 1){
    return(cov(samples))
  } else{
    return(var(samples))
  }
}

#' Compute posterior credible intervals for model parameters.
#' @param obj A "breg" object (fitted model).
#' @param prob Probability of the interval (e.g. 0.9 for a 90% interval).
#' @param pars Parameters to select. By default these are just the model coefficients (fixed effects).
#' @returns Posterior credible intervals for selected parameters.
#' @importFrom rstantools posterior_interval
#' @export
#' @method posterior_interval breg
posterior_interval.breg = function(obj, prob = 0.9, pars = obj$coef_names){
  samples = as.matrix(obj$stanfit)[,pars]
  intervals = posterior_interval(samples, prob = prob)
  return(intervals)
}

#' Draw from the posterior predictive distribution of the outcome.
#' @param obj A "breg" object (fitted model).
#' @param newdata Optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted linear predictors are used.
#' @param ndraws Number of posterior draws to use, i.e. number of replicated data sets to simulate. Defaults to NULL (use all).
#' @param seed The seed for random number generation. Set this manually if you want reproducible results.
#' @importFrom rstantools posterior_predict
#' @export
#' @method posterior_predict breg
posterior_predict.breg = function(obj, newdata = NULL, ndraws = NULL, seed = sample.int(.Machine$integer.max, size = 1L)){
  # figure out which Stan model code to us
  stan_model_to_use = stanmodels[[paste0(obj$model_name, "_sim")]]
  # figure out how which draws to use
  if(is.null(ndraws)){
    draws_to_use = 1:nrow(obj$b_draws_matrix)
  } else{
    if(!is.null(seed)){
      set.seed(seed)
    }
    draws_to_use = sample(1:nrow(obj$b_draws_matrix), size = ndraws, replace = FALSE)
  }
  # package data (modify later to allow for new data)
  if(obj$intercept_only){
    stan_data = list(N_tilde = obj$stan_data$N)
  } else{
    if(is.null(newdata)){
      stan_data = list(N_tilde = obj$stan_data$N,
                       K = obj$stan_data$K,
                       X_tilde = obj$stan_data$X)
    } else{
      X_tilde = make_stan_data(formula = obj$formula, data = newdata, center = obj$center)$X
      stan_data = list(N_tilde = nrow(newdata),
                       K = obj$stan_data$K,
                       X_tilde = X_tilde)
    }

  }
  # sample from the posterior predictive distribution
  post_samples = rstan::gqs(object = stan_model_to_use,
                            data = stan_data,
                            draws = obj$b_draws_matrix[draws_to_use,],
                            seed = seed) |> as.matrix() |> drop()
  return(post_samples)
}
