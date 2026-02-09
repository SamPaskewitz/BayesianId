#' Convert model probabilities into parameter probablities.
#'
#' @param model_probs A named vector of model probabilities (names = model formulas).
#' @param model_info Information about submodels, returned by using the function "submodels" on "formula(full_model)".
#' @returns A named vector of parameter inclusion probabilities (names = model terms/parameters).
#'

probs_model_to_term = function(model_probs, model_info){
  # set up empty vector
  term_probs = rep(0.0, times = model_info$n_terms)
  names(term_probs) = model_info$term_names

  # add up model probabilities to compute term probabilities
  for(i in 1:model_info$n_terms){
    term = model_info$term_names[i]
    includes_term = model_info$included_table[names(model_probs), term]
    term_probs[i] = sum(model_probs[includes_term])
  }

  # fix probs that are slightly > 1 (by approx error) to be slightly < 1
  term_probs[term_probs > 1] = 1 - .Machine$double.eps

  return(term_probs)
}
