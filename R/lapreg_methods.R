#' Print basic information about a "lapreg" object.
#' @param obj A "lapreg" object (fitted model).
#' @export
#' @method print lapreg
print.lapreg = function(obj){
  cat("lapreg model object")
  cat("\nCall:\n")
  print(obj$call)
}

#' Show summary information about a "lapreg" object.
#' @param obj A "lapreg" object (fitted model).
#' @export
#' @method summary lapreg
summary.lapreg = function(obj){
  cat("Call:\n")
  print(obj$call)
  cat("\nEstimates:\n")
  sumtab = data.frame(mean = obj$mu,
                      sd = obj$sigma) |>
    cbind(posterior_interval(obj, prob = 0.95))
  print(sumtab |> signif(3))
}

#' Get parameter estimates (posterior means) from a "lapreg" object.
#' @param obj A "lapreg" object (fitted model).
#' @export
#' @method coef lapreg
coef.lapreg = function(obj){
  return(obj$mu)
}

#' Return the posterior variance-covariance matrix of model parameters (intercept plus fixed effects).
#' @param obj A "lapreg" object (fitted model).
#' @returns The posterior variance-covariance matrix.
#' @export
vcov.lapreg = function(obj){
  return(obj$Sigma)
}

#' Compute posterior credible intervals for model parameters (intercept plus fixed effects).
#' @param obj A "lapreg" object (fitted model).
#' @param prob Probability of the interval (e.g. 0.9 for a 90\% interval).
#' @returns Posterior credible intervals.
#' @export
#' @method posterior_interval lapreg
posterior_interval.lapreg = function(obj, prob = 0.9){
  alpha = 1 - prob
  intervals = data.frame(lower = qnorm(p = alpha/2, mean = obj$mu, sd = obj$sigma),
                         upper = qnorm(p = 1 - alpha/2, mean = obj$mu, sd = obj$sigma)
                         ) |> as.matrix()
  colnames(intervals) = paste(100*c(alpha/2, 1 - alpha/2), "%")
  return(intervals)
}

#' Get model "terms".
#' @param obj A "lapreg" object (fitted model).
#' @returns Model "terms" object.
#' @export
#' @method terms lapreg
terms.lapreg = function(obj){
  return(model.frame(obj$formula, obj$data) |> terms())
}

#' Return the data frame used for fitting the model.
#' @param obj A "lapreg" object (fitted model).
#' @returns The model data frame.
#' @details Note that this will return any variables that have been mean-centered in the model fitting process as mean-centered.
#' @export
#' @method model.frame lapreg
model.frame.lapreg = function(obj){
  return(obj$data)
}

#' Create a posterior predictive plot for diagnostic purposes.
#' @param obj A "lapreg" object (fitted model).
#' @export
#' @method plot lapreg
plot.lapreg = function(obj){
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

  # select type of post pred plot
  if(obj$model_name %in% c("bernoulli_logistic")){
    pt = bayesplot::ppc_bars(y = obj$data[,obj$formula_info$lhs] |> as.numeric() - 1, yrep = Y_tilde_samples)
  } else{
    pt = bayesplot::ppc_dens_overlay(y = obj$data[,obj$formula_info$lhs], yrep = Y_tilde_samples)
  }

  return(pt)
}
