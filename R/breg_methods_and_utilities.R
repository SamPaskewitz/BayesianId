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
#' @aliases summary
#' @export
#' @method summary breg
summary.breg = function(obj){
  cat("Call:\n")
  print(obj$call)
  cat("\nEstimates:\n")
  sumtab = data.frame(mean = obj$post_mean,
                      sd = obj$post_sd) |>
    cbind(posterior_interval(obj, prob = 0.95))
  print(sumtab |> signif(3))
}

#' Get parameter estimates (posterior means) from a "breg" object.
#' @param obj A "breg" object (fitted model).
#' @export
#' @method coef breg
coef.breg = function(obj){
  return(obj$post_mean)
}

#' Return the posterior variance-covariance matrix of model parameters (intercept plus fixed effects).
#' @param obj A "breg" object (fitted model).
#' @returns The posterior variance-covariance matrix.
#' @export
vcov.breg = function(obj){
  return(obj$Sigma)
}

#' Get model "terms".
#' @param obj A "breg" object (fitted model).
#' @returns Model "terms" object.
#' @export
#' @method terms breg
terms.breg = function(obj){
  return(model.frame(obj$formula, obj$data) |> terms())
}

#' Return the data frame used for fitting the model.
#' @param obj A "breg" object (fitted model).
#' @returns The model data frame.
#' @details Note that this will return any variables that have been mean-centered in the model fitting process as mean-centered.
#' @aliases model.frame
#' @export
#' @method model.frame breg
model.frame.breg = function(obj){
  return(obj$data)
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
    draws_to_use = 1:nrow(obj$draws_matrix)
  } else{
    if(!is.null(seed)){
      set.seed(seed)
    }
    draws_to_use = sample(1:nrow(obj$draws_matrix), size = ndraws, replace = FALSE)
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
  if("N_trials" %in% names(obj$stan_data)){
    stan_data$N_trials = obj$stan_data$N_trials
  }

  # sample from the posterior predictive distribution
  stanfit = rstan::gqs(object = stan_model_to_use,
                       data = stan_data,
                       draws = obj$draws_matrix[draws_to_use,],
                       seed = seed)
  return(list(stanfit = stanfit, N_tilde = stan_data$N_tilde))
}

#' Print the Stan code.
#' @param obj A "breg" object (fitted model).
#' @returns The Stan code (as text).
#' @export
show_stancode = function(obj){
  stanmodels[[paste0(obj$model_name, "_est")]]
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

#' Create a posterior predictive plot for diagnostic purposes.
#' @param obj A "breg" object (fitted model).
#' @param group Name of a grouping variable (optional).
#' @param xvar Name of an x-axis variable for a scatterplot (optional).
#' @details
#' The type of plot created depends on the predicted/dependent variable ("y") and whether "group" or "x" is specified.
#'
#' If "xvar" is specified, then the data is represented as a scatterplot with "xvar" on the x-axis and both "y" (the real data) and "yrep" (points from one simulated data set) on the y-axis. This plot uses "ppc_intervals" from bayesplot, but with the intervals removed (and only one data set) because I find them visually distracting.
#'
#' If "xvar" is not specified and "y" is binary, count, or categorical, then you get a bar plot showing the actual data counts along with the median count from the simulated data. This uses "ppc_bars" with the intervals removed (again, I find them visually distracting).
#'
#' If "xvar" is not specified and "y" is numeric, then you get kernel density estimates of the distribution of the real data ("y", darker line) and the simulated data ("yrep", lighter lines, one for each simulated data set).
#'
#' If "group" is specified then there will be a subplot for each level of the grouping variable (using the "_grouped" version of the appropriate bayesplot function).
#' @aliases plot
#' @export
plot.breg = function(obj, group = NA, xvar = NA){
  # simulate data from the posterior predictive distribution
  Y_tilde_samples = posterior_predict(obj, ndraws = 5)

  # fix up y if it's binary (so bayesplot won't complain)
  if(obj$data[,obj$formula_info$lhs] |> is.factor()){
    y = obj$data[,obj$formula_info$lhs] |> as.numeric() - 1
  } else{
    y = obj$data[,obj$formula_info$lhs]
  }

  # select type of post pred plot
  if(is.na(xvar)){
    if(obj$model_name %in% c("bernoulli_logistic", "binomial_logistic", "poisson")){
      if(is.na(group)){
        pt = bayesplot::ppc_bars(y = y, yrep = Y_tilde_samples, prob = 0)
      } else{
        pt = bayesplot::ppc_bars_grouped(y = y, yrep = Y_tilde_samples, group = obj$data[,group], prob = 0)
      }
    } else{
      if(is.na(group)){
        pt = bayesplot::ppc_dens_overlay(y = y, yrep = Y_tilde_samples)
      } else{
        pt = bayesplot::ppc_dens_overlay_grouped(y = y, yrep = Y_tilde_samples, group = obj$data[,group])
      }
    }
  } else{
    if(is.na(group)){
      pt = bayesplot::ppc_intervals(y = y, yrep = Y_tilde_samples[sample(1:nrow(Y_tilde_samples), size = 1),,drop=FALSE], x = obj$data[,xvar], prob = 0.001, prob_outer = 0.001) + ggplot2::xlab(xvar)
    } else{
      pt = bayesplot::ppc_intervals_grouped(y = y, yrep = Y_tilde_samples[sample(1:nrow(Y_tilde_samples), size = 1),,drop=FALSE], group = obj$data[,group], x = obj$data[,xvar], prob = 0.001, prob_outer = 0.001) + ggplot2::xlab(xvar)
    }
  }
  return(pt)
}

#' Predicted values (means and optionally SD's) based on a fitted model.
#' @param obj A "breg" object (fitted model).
#' @param newdata Optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted linear predictors are used.
#' @param compute_sd Indicates whether predictive standard deviations should be computed.
#' @returns If compute_sd is FALSE, a vector of posterior predictive means. If compute_sd is TRUE, a list with the posterior predictive means and standard deviations.
#' @details This method is designed to be analogous to the "predict" method of lm and glm model objects. The "posterior_predict" method samples from the posterior predictive distribution; "predict" summarizes these samples to produce point predictions (means) and optionally their standard deviations (roughly analogous to predictive standard errors, but Bayesian).
#' @aliases predict
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
