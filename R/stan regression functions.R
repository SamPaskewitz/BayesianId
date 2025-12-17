#' @export
lm_indprior = function(formula, data, center = FALSE, r = 0.5, seed = NA, chains = 4, iter = 2000, warmup = floor(iter/2)){
  # **** FIGURE THIS OUT *****

  # ** prepare data **
  prep = prepare_data(data = data, formula = formula, center = center)
  standata = brms::standata(formula, prep$data)

  # ** figure out if it's an intercept-only model **
  intercept_only = (ncol(standata$X) == 1)

  # ** set up prior adjustments (based on the SDs of numeric predictors) **
  if(!intercept_only){
    # add Caucy scale factor (r)
    standata$r = r

    # design matrix (without intercept)
    X = standata$X[,-1]

    # initialize prior adjustment vector
    x_scale = rep(1.0, times = ncol(X))
    names(x_scale) = colnames(X)

    # loop through numeric predictor variables and adjust the prior by their SDs
    # TO DO: Fix this so that it works if one variable name is a subset of another variable's name, e.g. "x" and "x1".
    # OR ELSE: Give a warning if one variable's name is a subset of another's.
    if(!is.null(prep$x_numeric_names)){
      for(i in 1:length(prep$x_numeric_names)){
        has_x = grepl(prep$x_numeric_names[i], colnames(X))
        x_scale[has_x] = x_scale[has_x]*sd(X[,prep$x_numeric_names[i]])
      }
    }

    # add x_scale to standata
    standata$x_scale = x_scale

    # pick the stan model to use
    stan_model_to_use = stanmodels$linear_normal
  } else{
    stan_model_to_use = stanmodels$intercept_normal
  }

  # ** fit the model **
  fit = rstan::sampling(stan_model_to_use,
                        data = standata,
                        seed = seed,
                        chains = chains,
                        iter = iter,
                        warmup = warmup,
                        refresh = 0 # don't print annoying updates
                        )

  class(fit) = "brmsfit"
  return(fit)
}
