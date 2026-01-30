#' Fit a Bayesian regression model using Stan.
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
#' ADD THESE
#' DESCRIBE THE FITTED MODEL OBJECT
#' DESCRIBE PRIORS
#' @export
#'
breg = function(formula, data, family = "normal_linear", center = TRUE, prior_scale = 1.0, seed = NA, chains = 4, iter = 10000, warmup = floor(iter/4)){
  # ** set up Stan data **
  stan_data = make_stan_data(formula = formula, data = data, family = family, prior_scale = prior_scale, center = center)

  # ** pick the Stan model to use **
  # NOTE: later this will be more elaborate to deal with mixed effects models etc.
  model_name = family
  intercept_only = !("X" %in% names(stan_data))
  if(intercept_only){
    model_name = paste0(model_name, "_intercept")
  }
  stan_model_to_use = stanmodels[[paste0(model_name, "_est")]]

  # ** set up initialization for sigma **
  # this is particularly important with censored data
  if(family %in% c("normal_linear", "right_censored_linear")){
    sample_sd = sd(stan_data$Y)
    init = function(){
      list(sigma = runif(1, min = 0.5, max = 1.5)*sample_sd)
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

  # ** get formula info **
  formula_info = parse_formula(formula)

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
  names(stanfit)[names(stanfit) == "delta0"] = "delta_Intercept"
  if(!intercept_only){
    coef_names = colnames(stan_data$X)
    names(stanfit)[names(stanfit) %in% paste0("b[",1:n_coef,"]")] = coef_names
    names(stanfit)[names(stanfit) %in% paste0("delta[",1:n_coef,"]")] = paste0("delta_", coef_names)
  } else{
    coef_names = NULL
  }

  # ** assemble a "breg" object **
  output = list(stanfit = stanfit,
                model_name = model_name,
                intercept_only = intercept_only,
                formula = formula,
                formula_info = formula_info,
                stan_data = stan_data,
                data = data[,c(formula_info$lhs, formula_info$fixed_main)], # only relevant variables
                center = center,
                coef_names = coef_names,
                call = match.call(), # this records the function call, so the default "update" method works (I don't need to write my own version)
                b_draws_matrix = b_draws_matrix)
  class(output) = "breg"
  return(output)
}
