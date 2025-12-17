#' Convert term inclusion probabilities into model probablities.
#'
#' @param main_probs A named vector of inclusion probabilities for main effects.
#' @param intr_condprobs A named vector of inclusion conditional probabilities for interactions, or NULL if there are no interactions.
#' @param model_info Information about submodels, returned by using the function "submodels" on "formula(full_model)".
#' @returns A named vector of model probabilities (names = model formulas).
#' @details Prior probabilities for main effects are assumed to be independent of each other. Prior probabilities for two-way interactions are conditional on the inclusion of both relevant main effects, those for three-way interactions on the inclusion of relevant main effects and two-way interactions, etc.
#' @examples
#' * ADD THESE, ESPECIALLY TO SHOW HOW INTERACTIONS WORK
#'
#'

probs_term_to_model = function(main_probs, intr_condprobs, model_info){
  # check that "term_probs" has the correct names in the correct order
  # ADD THIS

  # check that the probs are between 0 and 1
  if(!all((main_probs > 0) & (main_probs <= 1))){
    stop("Main effect probabilities must be > 0 and <= 1.")
  }
  # FIX FOR INTERACTIONS

  # initialize model_probs
  model_probs = rep(1.0, times = model_info$n_models)
  names(model_probs) = model_info$model_names

  # adjust model probabilities for main effects
  for(i in 1:model_info$n_main){ # main effect probs are not conditional
    main = model_info$main_names[i]
    has_main = model_info$included_table[,main]
    model_probs = model_probs*ifelse(has_main, main_probs[main], (1 - main_probs[main]))
  }

  # adjust model probabilities for interactions (if there are any)
  if(model_info$n_intr > 0){
    for(i in 1:model_info$n_intr){
      intr = model_info$intr_names[i]
      has_intr = model_info$included_table[,intr]
      could_include = lapply(model_info$included, function(x){has_needed(x, intr)}) |> unlist()
      # only adjust if the model could include the interaction
      adjustment = ifelse(test = could_include,
                          yes = ifelse(test = has_intr,
                                       yes = intr_condprobs[intr],
                                       no = 1 - intr_condprobs[intr]),
                          no = 1)
      model_probs = model_probs*adjustment
    }
  }

  return(model_probs)
}
