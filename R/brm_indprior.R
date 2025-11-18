#' Fit a regression model with independent Cauchy priors on fully standardized regression coefficients and appropriate contrasts for factors.
#' #'
#' @param formula An object of class formula or brmsformula (or one that can be coerced to that classes): A symbolic description of the model to be fitted. The details of model specification are explained in brmsformula.
#' @param data 	An object of class data.frame (or one that can be coerced to that class) containing data of all variables used in the model.
#' @param family A description of the response distribution and link function to be used in the model. This can be a family function, a call to a family function or a character string naming the family. Every family function has a link argument allowing to specify the link function to be applied on the response variable. If not specified, default links are used. For details of supported families see brmsfamily. By default, a linear gaussian model is applied.
#' @param r The scale of the multivariate Cauchy prior distribution on the regression coefficients (a single positive number).
#' @param seed The seed for random number generation to make results reproducible. If NA (the default), Stan will set the seed randomly.
#' @param chains Number of Markov chains (defaults to 4).
#' @param iter Number of total iterations per chain (including warmup; defaults to 2000).
#' @param warmup A positive integer specifying number of warmup (aka burnin) iterations. This also specifies the number of iterations used for stepsize adaptation, so warmup draws should not be used for inference. The number of warmup should not be larger than iter and the default is iter/2.
#' @returns A fitted brms model.
#' @details This is a wrapper around the "brm" function from brms that ADD DETAILS **

brm_indprior = function(formula, data, family = gaussian(), r = 0.5, seed = NA, chains = 4, iter = 2000, warmup = floor(iter/2)){
  # ** prepare data **
  prep = prepare_data(data, formula)

  # ** define prior **
  intercept_prior = set_prior("", class = "Intercept") # improper flat prior
  has_sigma = family$family %in% c("gaussian", "student", "lognormal", "shifted_lognormal", "skew_normal", "gen_extreme_value", "exgaussian", "logistic_normal", "asym_laplace", "hurdle_lognormal")
  if(has_sigma){ # families that have sigma as a parameter
    b_prior = set_prior("", "b") + set_prior("target += cauchy_lpdf(b | 0, r*sigma/prior_adjust)", check = FALSE)
    sigma_prior = set_prior("", "sigma") + set_prior("target += -2*log(sigma)", check = FALSE) # see https://discourse.mc-stan.org/t/setting-jeffreys-s-prior-on-sigma
    our_prior = intercept_prior + b_prior + sigma_prior
  }
  else{ # families that don't have sigma as a parameter
    b_prior = set_prior("", "b") + set_prior("target += cauchy_lpdf(b | 0, r/prior_adjust)", check = FALSE)
    our_prior = intercept_prior + b_prior
  }

  # ** set up prior adjustments (based on the SDs of numeric predictors) **
  # design matrix (without intercept)
  X = model.matrix(formula, data)[,-1]

  # initialize prior adjustment vector
  prior_adjust = rep(1.0, times = ncol(X))
  names(prior_adjust) = colnames(X)

  # loop through numeric predictor variables and adjust the prior by their SDs
  # TO DO: Fix this so that it works if one variable name is a subset of another variable's name, e.g. "x" and "x1".
  for(i in 1:length(prep$x_numeric_names)){
    has_x = grepl(prep$x_numeric_names[i], colnames(X))
    prior_adjust[has_x] = prior_adjust[has_x]/sd(X[,prep$x_numeric_names[i]])
  }

  # ** define extra variables (stanvars) **
  prior_adjust_stanvar = stanvar(prior_adjust, name = "prior_adjust", scode = "vector[Kc] prior_adjust;", block = "data")
  r_stanvar = stanvar(r, name = "r", scode = "real<lower=0> r;", block = "data") # the scale hyperparameter of the Cauchy hyper g-prior
  our_stanvars = prior_adjust_stanvar + r_stanvar

  # ** fit the model using brms **
  fit = brms::brm(formula = formula,
                  family = family,
                  data = prep$data,
                  prior = our_prior,
                  stanvars = our_stanvars,
                  seed = seed,
                  chains = chains,
                  iter = iter,
                  warmup = warmup,
                  refresh = 0, # don't print annoying updates
                  save_pars = save_pars(all = TRUE) # for bridge sampling
  )

  return(fit)
}
