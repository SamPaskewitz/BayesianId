#' Fit restricted submodels, given a full model.
#' @param full_model A brmsfit object (fitted regression model) representing the full model, i.e. the model that includes all possible terms.
#' @param model_list A list of submodels, returned by using the function "submodels" on "full_model".
#'
fit_submodels = function(full_model, model_list){
  fit_list = list()
  fit_list[[1]] = full_model

  # fit restricted models
  for(i in 2:model_list$n_models){
    if("prior_adjust" %in% names(full_model$stanvars) & i < model_list$n_models){
      # new_stanvars for ind. prior models
      new_X_names = (brms::standata(model_list$formulas[[i]], full_model$data)$X |> colnames())[-1]
      new_stanvars = stanvar(full_model$stanvars$prior_adjust$sdata[new_X_names], name = "prior_adjust", block = "data") + stanvar(r, name = "r", scode = "real<lower=0> r;", block = "data")
    } else{
      # new_stanvars for JZS prior or intercept-only model
      new_stanvars = NULL
    }

    # fit restricted model using "update"
    fit_list[[i]] = update(full_model,
                           formula = model_list$formulas[[i]],
                           stanvars = new_stanvars,
                           refresh = 0)
  }
  names(fit_list) = model_list$model_names

  return(fit_list)
}
