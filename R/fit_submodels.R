#' Fit restricted submodels, given a full model.
#' @param full_model An object representing the full model, i.e. the model that includes all possible terms.
#' @param model_info Information about submodels, returned by using the function "submodels" on "formula(full_model)".
#'
fit_submodels = function(full_model, model_info){
  fit_list = list()
  fit_list[[1]] = full_model

  # fit restricted models
  for(i in 2:model_info$n_models){
    fit_list[[i]] = update(full_model,
                           formula = model_info$formulas[[i]])
  }

  names(fit_list) = model_info$model_names
  return(fit_list)
}
