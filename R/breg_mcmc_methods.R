#' Compute posterior credible intervals for model parameters.
#' @param obj A "breg_mcmc" object (fitted model).
#' @param prob Probability of the interval (e.g. 0.9 for a 90\% interval).
#' @param pars Parameters to select. By default these are just the model coefficients (fixed effects) plus the intercept.
#' @returns Posterior credible intervals for selected parameters.
#' @importFrom rstantools posterior_interval
#' @export
#' @method posterior_interval breg_mcmc
posterior_interval.breg_mcmc = function(obj, prob = 0.9, pars = c("(Intercept)", obj$coef_names)){
  samples = as.matrix(obj$stanfit)[,pars]
  intervals = posterior_interval(samples, prob = prob)
  alpha = 1 - prob
  colnames(intervals) = paste(100*c(alpha/2, 1 - alpha/2), "%")
  return(intervals)
}

#' Draw from the posterior distribution of the linear predictor (mu = b0 + b1*x + ...).
#' @param obj A "breg_mcmc" object (fitted model).
#' @param newdata Optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted data are used.
#' @param ndraws Number of posterior draws to use, i.e. number of replicated data sets to simulate. Defaults to NULL (use all).
#' @param seed The seed for random number generation. Set this manually if you want reproducible results.
#' @returns A D x N matrix of samples from the posterior distribution of the linear predictor (mu), where D is the number of draws and N is the number of data points.
#' @importFrom rstantools posterior_linpred
#' @export
#' @method posterior_linpred breg_mcmc
posterior_linpred.breg_mcmc = function(obj, newdata = NULL, ndraws = NULL, seed = sample.int(.Machine$integer.max, size = 1L)){
  sim = simulate(obj = obj, newdata = newdata, ndraws = ndraws, seed = seed)
  all_samples = as.matrix(sim$stanfit)
  cnames = paste0("mu[", 1:sim$N_tilde, "]")
  mu_samples = all_samples[, cnames] |> drop()
  return(mu_samples)
}

#' Compute means of the linear predictor (mu = b0 + b1*x + ...) at all combinations of factor levels (i.e. expected marginal means).
#' @param obj A "breg_mcmc" object (fitted model).
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
