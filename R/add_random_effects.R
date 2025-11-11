#' Add appropriate random effects to model formulas.
#' @param model_list A list of models, of the format returned by the "submodels" function.
#' @param random_effects Terms corresponding to predictors that are measured repeatedly from the same participants/subjects/experimental units. Should be a character vector.
#' @param id The participant/subject ID variable (or equivalent "grouping variable").
#' @returns An updated list of models in the same format, with appropriate random effects added.
#' @details This adds random effects following the principles of REF **, which can be summarized as "The random effects should be the same as the corresponding fixed effects for repeated measures predictors". At least for now, this only supports a single level of random effects with a single "grouping variable" (specified with the argument "id").
#'

add_random_effects = function(model_list, random_effects, id){
  for(i in 1:model_list$n_models){
    random_to_include = intersect(model_list$included[[i]], random_effects)
    if(length(random_to_include) == 0){
      random_to_include = c("1")
    }
    model_list$random_effects[[i]] = paste0("(", random_to_include %>% paste(collapse = " + "), " | ", id, ")")
    model_list$formulas[[i]] = formula(paste(c(model_list$model_names[[i]], model_list$random_effects[[i]]), collapse = " + "))
  }
  return(model_list)
}
