#' Fit restricted submodels, given a full model.
#' @param full_model A brmsfit object (fitted regression model) representing the full model, i.e. the model that includes all possible terms.
#' @param model_info Information about submodels, returned by using the function "submodels" on "formula(full_model)".
#'
fit_submodels = function(full_model, model_info){
  fit_list = list()
  fit_list[[1]] = full_model

  # fit restricted models
  if("brmsfit" %in% class(fit_list[[1]])){
    # MCMC model fits with brms
    for(i in 2:model_info$n_models){
      if("prior_adjust" %in% names(full_model$stanvars) & i < model_info$n_models){
        # new_stanvars for ind. prior models
        new_X_names = (brms::standata(model_info$formulas[[i]], full_model$data)$X |> colnames())[-1]
        new_stanvars = stanvar(full_model$stanvars$prior_adjust$sdata[new_X_names], name = "prior_adjust", block = "data") + stanvar(full_model$stanvars$r$sdata, name = "r", scode = "real<lower=0> r;", block = "data")
      } else{
        # new_stanvars for JZS prior or intercept-only model
        new_stanvars = NULL
      }

      # fit restricted model using "update"
      fit_list[[i]] = update(full_model,
                             formula = model_info$formulas[[i]],
                             stanvars = new_stanvars,
                             refresh = 0)
    }
  } else{
    # other types of model fit (e.g. regular old MLE with lm, lmer, glm, etc.)
    for(i in 2:model_info$n_models){
      fit_list[[i]] = update(full_model,
                             formula = model_info$formulas[[i]])
    }
  }

  names(fit_list) = model_info$model_names
  return(fit_list)
}
