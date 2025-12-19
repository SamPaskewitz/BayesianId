#' Print basic information about a "breg" object.
#' @param obj A "breg" object.
#' @export
#' @method print breg
print.breg = function(obj){
  cat("Formula:", as.character(obj$model_info$formulas[[1]])[[1]])
  ## ADD MORE INFORMATION
}
