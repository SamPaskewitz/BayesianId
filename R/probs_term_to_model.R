#' Convert parameter probabilities into model probablities.
#'
#' @param term_probs A named vector of parameter inclusion probabilities (names = model terms/parameters).
#' @param model_list A list containing information about some models (see restricted_models for details).
#' @returns A named vector of model probabilities (names = model formulas).
#' @details This assumes that the prior probabilities for parameter inclusion are all independent of each other.
#'

probs_term_to_model = function(term_probs, model_list){
  # set up empty vector
  model_probs = rep(0.0, times = model_list$n_models)
  names(model_probs) = model_list$model_names

  # compute model probabilities
  for(i in 1:model_list$n_models){
    model_probs[i] = prod(term_probs[model_list$included[[i]]])*prod(1 - term_probs[model_list$omitted[[i]]])
    # PROBS FOR INTERACTIONS MUST BE CONDITIONAL ON MAIN EFFECTS
  }

  return(model_probs)
}
