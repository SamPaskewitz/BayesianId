#' Test if a set of model terms (fixed effects) contains all the needed terms one order lower than an interaction.
#' @param terms A character vector of model terms.
#' @param intr The interaction, written in a form like "a:b" or "x1:x2:x3", etc.
#' @returns A logical value indicating whether or not the necessary terms are present.
#' @export

has_needed = function(terms, intr){
  parts = strsplit(intr, "\\:") |> unlist()
  ord = length(parts)
  what_needed = combn(parts, ord - 1, simplify = FALSE) |> lapply(function(x){paste(x, collapse = ":")}) |> unlist()
  return(all(what_needed %in% terms))
}
