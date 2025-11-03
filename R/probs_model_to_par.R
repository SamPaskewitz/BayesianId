#' Convert model probabilities into parameter probablities.
#'
#' @param model_probs A named vector of model probabilities (names = model formulas).
#' @param model_list A list containing information about some models (see restricted_models for details).
#' @returns A named vector of parameter inclusion probabilities (names = model terms/parameters).
#'

probs_model_to_par = function(model_probs, model_list){
  # set up empty vector
  par_probs = rep(0.0, times = model_list$n_fixed)
  names(par_probs) = model_list$fixed_names

  # add up model probabilities to compute parameter probabilities
  for(i in 1:model_list$n_fixed){
    term = model_list$fixed_names[i]
    includes_term = model_list$included_table[names(model_probs), term]
    par_probs[i] = sum(model_probs[includes_term])
  }

  return(par_probs)
}
