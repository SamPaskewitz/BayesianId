#' Print basic information about a "breg_laplace" object.
#' @param obj A "breg_laplace" object (fitted model).
#' @export
#' @method print breg_laplace
print.breg_laplace = function(obj){
  cat("breg_laplace model object")
  cat("\nCall:\n")
  print(obj$call)
}

#' Show summary information about a "breg_laplace" object.
#' @param obj A "breg_laplace" object (fitted model).
#' @export
#' @method summary breg_laplace
summary.breg_laplace = function(obj){
  cat("Call:\n")
  print(obj$call)
  cat("\nEstimates:\n")
  sumtab = data.frame(mean = obj$post_mean,
                      sd = obj$post_sd) |>
    cbind(posterior_interval(obj, prob = 0.95))
  print(sumtab |> signif(3))
}

#' Get parameter estimates (posterior means) from a "breg_laplace" object.
#' @param obj A "breg_laplace" object (fitted model).
#' @export
#' @method coef breg_laplace
coef.breg_laplace = function(obj){
  return(obj$post_mean)
}

#' Return the posterior variance-covariance matrix of model parameters (intercept plus fixed effects).
#' @param obj A "breg_laplace" object (fitted model).
#' @returns The posterior variance-covariance matrix.
#' @export
vcov.breg_laplace = function(obj){
  return(obj$Sigma)
}

#' Compute posterior credible intervals for model parameters (intercept plus fixed effects).
#' @param obj A "breg_laplace" object (fitted model).
#' @param prob Probability of the interval (e.g. 0.9 for a 90\% interval).
#' @returns Posterior credible intervals.
#' @export
#' @method posterior_interval breg_laplace
posterior_interval.breg_laplace = function(obj, prob = 0.9){
  alpha = 1 - prob
  intervals = data.frame(lower = qnorm(p = alpha/2, mean = obj$post_mean, sd = obj$post_sd),
                         upper = qnorm(p = 1 - alpha/2, mean = obj$post_mean, sd = obj$post_sd)
                         ) |> as.matrix()
  colnames(intervals) = paste(100*c(alpha/2, 1 - alpha/2), "%")
  return(intervals)
}

#' Get model "terms".
#' @param obj A "breg_laplace" object (fitted model).
#' @returns Model "terms" object.
#' @export
#' @method terms breg_laplace
terms.breg_laplace = function(obj){
  return(model.frame(obj$formula, obj$data) |> terms())
}

#' Return the data frame used for fitting the model.
#' @param obj A "breg_laplace" object (fitted model).
#' @returns The model data frame.
#' @details Note that this will return any variables that have been mean-centered in the model fitting process as mean-centered.
#' @export
#' @method model.frame breg_laplace
model.frame.breg_laplace = function(obj){
  return(obj$data)
}

#' Simulate from the posterior predictive distribution.
#' @param obj A "breg_laplace" object (fitted model).
#' @details Currently this is very limited, and is just a support for posterior predictive plots. In the future I should add the option to use new data.
#' @export
#' @method simulate breg_laplace
simulate.breg_laplace = function(obj){
  # figure out which Stan model code to use
  stan_model_to_use = stanmodels[[paste0(obj$model_name, "_sim")]]

  # package data
  if(obj$intercept_only){
    stan_data = list(N_tilde = obj$stan_data$N)
  } else{
    stan_data = list(N_tilde = obj$stan_data$N,
                     K = obj$stan_data$K,
                     X_tilde = obj$stan_data$X)
  }
  if("Ymax" %in% names(obj$stan_data)){
    stan_data$Ymax = obj$stan_data$Ymax
  }

  # sample from the posterior predictive distribution
  stanfit = rstan::gqs(object = stan_model_to_use,
                       data = stan_data,
                       draws = obj$optim_fit$theta_tilde)
  all_samples = as.matrix(stanfit)
  cnames = paste0("Y_tilde[", 1:obj$stan_data$N, "]")
  Y_tilde_samples = all_samples[, cnames] |> drop()

  return(Y_tilde_samples)
}

#' Create a posterior predictive plot for diagnostic purposes.
#' @param obj A "breg_laplace" object (fitted model).
#' @param group Name of a grouping variable (optional).
#' @param xvar Name of an x-axis variable for a scatterplot (optional).
#' @details
#' The type of plot created depends on the predicted/dependent variable ("y") and whether "group" or "x" is specified.
#'
#' If "xvar" is specified, then the data is represented as a scatterplot with "xvar" on the x-axis and both "y" (the real data) and "yrep" (points from one simulated data set) on the y-axis. This plot uses "ppc_intervals" from bayesplot, but with the intervals removed (and only one data set) because I find them visually distracting.
#'
#' If "xvar" is not specified and "y" is binary or categorical, then you get a bar plot showing the actual data counts along with the median count from the simulated data. This uses "ppc_bars" with the intervals removed (again, I find them visually distracting).
#'
#' If "xvar" is not specified and "y" is numeric, then you get kernel density estimates of the distribution of the real data ("y", darker line) and the simulated data ("yrep", lighter lines, one for each simulated data set).
#'
#' If "group" is specified then there will be a subplot for each level of the grouping variable (using the "_grouped" version of the appropriate bayesplot function).
#' @export
plot.breg_laplace = function(obj, group = NA, xvar = NA){
  # simulate data from the posterior predictive distribution
  Y_tilde_samples = simulate(obj)

  # fix up y if it's binary (so bayesplot won't complain)
  if(obj$data[,obj$formula_info$lhs] |> is.factor()){
    y = obj$data[,obj$formula_info$lhs] |> as.numeric() - 1
  } else{
    y = obj$data[,obj$formula_info$lhs]
  }

  # select type of post pred plot
  if(is.na(xvar)){
    if(obj$model_name %in% c("bernoulli_logistic")){
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
      pt = bayesplot::ppc_intervals(y = y, yrep = Y_tilde_samples[1,,drop=FALSE], x = obj$data[,xvar], prob = 0.001, prob_outer = 0.001) + ggplot2::xlab(xvar)
    } else{
      pt = bayesplot::ppc_intervals_grouped(y = y, yrep = Y_tilde_samples[1,,drop=FALSE], group = obj$data[,group], x = obj$data[,xvar], prob = 0.001, prob_outer = 0.001) + ggplot2::xlab(xvar)
    }
  }
  return(pt)
}
