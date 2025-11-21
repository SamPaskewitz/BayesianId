#' Test if a set of model terms obeys the principle of marginality, i.e. that every interaction has all the needed lower order interactions/main effects.
#' This is only intended to be used in the function "submodels".

test_marginality = function(terms){
  is_interaction = grepl("\\:", terms)
  all_okay = terms[is_interaction] |> sapply(function(intr){has_needed(terms, intr)}) |> all()
  return(all_okay)
}
