#' Test if a set of model terms (fixed effects) contains all the needed terms one order lower than an interaction.
#' This is only intended to be used in the function "test_marginality".

has_needed = function(terms, intr){
  parts = strsplit(intr, "\\:") |> unlist()
  ord = length(parts)
  what_needed = combn(parts, ord - 1, simplify = FALSE) |> lapply(function(x){paste(x, collapse = ":")}) |> unlist()
  return(all(what_needed %in% terms))
}
