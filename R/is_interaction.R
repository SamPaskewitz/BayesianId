#' Test whether terms are interactions are not.
#'
is_interaction = function(terms){
  return(grepl("\\:", terms))
}
