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
  cat("\nFactor means (estimated marginal means):\n")
}

#' Get parameter estimates (posterior means) from a "breg" object.
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
#' @importFrom bayesplot ppc_dens_overlay ppc_hist
#' @export
#' @method plot breg
plot.breg = function(obj, type = "post_pred", pars = obj$coef_names){
  if(type == "post_pred"){
    ppred = posterior_predict(obj, ndraws = 5)
    # select type of post pred plot
    if(obj$model_name %in% c("bernoulli_logistic")){
      pt = ppc_hist(y = obj$data[,obj$formula_info$lhs], yrep = ppred, bins = 2)
    } else{
      pt = ppc_dens_overlay(y = obj$data[,obj$formula_info$lhs], yrep = ppred)
    }
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
#' @param pars Parameters to select. By default these are just the model coefficients (fixed effects) plus the intercept.
#' @returns The posterior variance-covariance matrix.
#' @export
vcov.breg = function(obj, pars = c("Intercept", obj$coef_names)){
  samples = as.matrix(obj$stanfit)[,pars]
  if(length(pars) > 1){
    return(cov(samples))
  } else{
    return(var(samples))
  }
}

#' Compute posterior credible intervals for model parameters.
#' @param obj A "breg" object (fitted model).
#' @param prob Probability of the interval (e.g. 0.9 for a 90% interval).
#' @param pars Parameters to select. By default these are just the model coefficients (fixed effects) plus the intercept.
#' @returns Posterior credible intervals for selected parameters.
#' @importFrom rstantools posterior_interval
#' @export
#' @method posterior_interval breg
posterior_interval.breg = function(obj, prob = 0.9, pars = c("Intercept", obj$coef_names)){
  samples = as.matrix(obj$stanfit)[,pars]
  intervals = posterior_interval(samples, prob = prob)
  return(intervals)
}

#' Simulate the model to get generated quantities (posterior predictive samples and posterior samples of mu, i.e. "linpred").
#' @param obj A "breg" object (fitted model).
#' @param newdata Optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted data are used.
#' @param ndraws Number of posterior draws to use, i.e. number of replicated data sets to simulate. Defaults to NULL (use all).
#' @param seed The seed for random number generation. Set this manually if you want reproducible results.
#' @details This runs the appropriate "_sim" Stan code with the "gqs" rstan function, producing samples of "mu" (the linear predictor) and "Y_tilde" (the posterior predictive distribution). The user will not ordinarily use this function directly. Instead, this function is used for the "posterior_predict" and "posterior_linpred" methods.
#' @returns A list with the stanfit object (containing the samples) and "N_tilde" (the number of simulated data points).
#' @export
#' @method simulate breg
simulate.breg = function(obj, newdata = NULL, ndraws = NULL, seed = sample.int(.Machine$integer.max, size = 1L)){
  # figure out which Stan model code to use
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
  # package data
  if(obj$intercept_only){
    stan_data = list(N_tilde = obj$stan_data$N)
  } else{
    if(is.null(newdata)){
      stan_data = list(N_tilde = obj$stan_data$N,
                       K = obj$stan_data$K,
                       X_tilde = obj$stan_data$X)
    } else{
      X_tilde = model.matrix(formula(obj$formula_info$rhs), data = newdata)[,-1,drop = FALSE]
      stan_data = list(N_tilde = nrow(newdata),
                       K = obj$stan_data$K,
                       X_tilde = X_tilde)
    }
  }
  if("Ymax" %in% names(obj$stan_data)){
    stan_data$Ymax = obj$stan_data$Ymax
  }

  # sample from the posterior predictive distribution
  stanfit = rstan::gqs(object = stan_model_to_use,
                       data = stan_data,
                       draws = obj$b_draws_matrix[draws_to_use,],
                       seed = seed)
  return(list(stanfit = stanfit, N_tilde = stan_data$N_tilde))
}

#' Draw from the posterior predictive distribution.
#' @param obj A "breg" object (fitted model).
#' @param newdata Optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted data are used.
#' @param ndraws Number of posterior draws to use, i.e. number of replicated data sets to simulate. Defaults to NULL (use all).
#' @param seed The seed for random number generation. Set this manually if you want reproducible results.
#' @returns A D x N matrix of samples from the posterior predictive distribution, where D is the number of draws and N is the number of data points.
#' @importFrom rstantools posterior_predict
#' @export
#' @method posterior_predict breg
posterior_predict.breg = function(obj, newdata = NULL, ndraws = NULL, seed = sample.int(.Machine$integer.max, size = 1L)){
  sim = simulate(obj = obj, newdata = newdata, ndraws = ndraws, seed = seed)
  all_samples = as.matrix(sim$stanfit)
  cnames = paste0("Y_tilde[", 1:sim$N_tilde, "]")
  Y_tilde_samples = all_samples[, cnames] |> drop()
  return(Y_tilde_samples)
}

#' Draw from the posterior distribution of the linear predictor (mu = b0 + b1*x + ...).
#' @param obj A "breg" object (fitted model).
#' @param newdata Optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted data are used.
#' @param ndraws Number of posterior draws to use, i.e. number of replicated data sets to simulate. Defaults to NULL (use all).
#' @param seed The seed for random number generation. Set this manually if you want reproducible results.
#' @returns A D x N matrix of samples from the posterior distribution of the linear predictor (mu), where D is the number of draws and N is the number of data points.
#' @importFrom rstantools posterior_linpred
#' @export
#' @method posterior_linpred breg
posterior_linpred.breg = function(obj, newdata = NULL, ndraws = NULL, seed = sample.int(.Machine$integer.max, size = 1L)){
  sim = simulate(obj = obj, newdata = newdata, ndraws = ndraws, seed = seed)
  all_samples = as.matrix(sim$stanfit)
  cnames = paste0("mu[", 1:sim$N_tilde, "]")
  mu_samples = all_samples[, cnames] |> drop()
  return(mu_samples)
}

#' Predicted values (means and optionally SD's) based on a fitted model.
#' @param obj A "breg" object (fitted model).
#' @param newdata Optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted linear predictors are used.
#' @param compute_sd Indicates whether predictive standard deviations should be computed.
#' @returns If compute_sd is FALSE, a vector of posterior predictive means. If compute_sd is TRUE, a list with the posterior predictive means and standard deviations.
#' @details This method is designed to be analogous to the "predict" method of lm and glm model objects. The "posterior_predict" method samples from the posterior predictive distribution; "predict" summarizes these samples to produce point predictions (means) and optionally their standard deviations (roughly analogous to predictive standard errors, but Bayesian).
#' @export
#' @method predict breg
predict.breg = function(obj, newdata = NULL, compute_sd = FALSE){
  ppred_draws = posterior_predict(obj = obj, newdata = newdata)
  ppred_mean = apply(ppred_draws, MARGIN = 2, FUN = mean)
  if(compute_sd){
    ppred_sd = apply(ppred_draws, MARGIN = 2, FUN = sd)
    return(list(mean = ppred_mean, sd = ppred_sd))
  }
  else{
    return(ppred_mean)
  }
}

#' Compute means of the linear predictor (mu = b0 + b1*x + ...) at all combinations of factor levels (i.e. expected marginal means).
#' @param obj A "breg" object (fitted model).
#' @param f The factor for which to compute estimated marginal means, or NULL to compute estimated marginal means for all factor combinations.
#' @returns The estimated marginal means and their standard deviations.
#' @export
#'
factor_means = function(obj, f = NULL){
  # get info
  x_names = obj$formula_info$fixed_main
  is_factor = sapply(obj$data[,x_names,drop=FALSE], is.factor)
  factor_names = x_names[is_factor]
  n_factors = length(factor_names)
  # make list of factors
  factor_list = list()
  for(i in 1:n_factors){
    factor_list[[i]] = levels(obj$data[,factor_names[i]])
  }
  names(factor_list) = factor_names
  # create grid of factor level combinations
  grid = expand.grid(factor_list)
  # add numeric variables at their means
  num_names = x_names[!is_factor]
  n_num = length(num_names)
  for(i in 1:n_num){
    grid[,num_names[i]] = mean(obj$data[,num_names[i]])
  }
  # sample the linear predictor (mu) across the grid
  mu_samples = posterior_linpred(obj, newdata = grid)
  # combine columns together based on "factor" (if needed)
  if(n_factors > 1 & !is.null(f)){

  }
  # compute factor means, SD's, and quantiles
  output = data.frame(mean = apply(mu_samples, MARGIN = 2, FUN = mean),
                      sd = apply(mu_samples, MARGIN = 2, FUN = sd),
                      "2.5%" = apply(mu_samples, MARGIN = 2, FUN = quantile, probs = 0.025),
                      "97.5%" = apply(mu_samples, MARGIN = 2, FUN = quantile, probs = 0.975),
                      check.names = FALSE)
  # label everything nicely
  cell_names = grid[,1]
  if(n_factors > 1){
    for(i in 2:n_factors){
      cell_names = paste(cell_names, grid[,i], sep = "_")
    }
  }
  row.names(output) = cell_names
  return(output)
}
