#' @importFrom emmeans recover_data
#' @export
#' @method recover_data breg
recover_data.breg <- function(object, ...) {
  data = object$data[,object$formula_info$x_names,drop=FALSE]
  attr(data, "call") = object$call
  attr(data, "terms") = model.frame(object$formula, object$data) |> terms() |> delete.response()
  attr(data, "predictors") = object$formula_info$x_names
  attr(data, "responses") = object$formula_info$y_name
  return(data)
}

#' @importFrom emmeans emm_basis
#' @export
#' @method emm_basis breg
emm_basis.breg = function(object, trms, xlev, grid, ...){
  # Based on: https://github.com/rvlenth/emmeans/blob/master/R/brms-support.R
  m = model.frame(trms, grid, na.action = na.pass, xlev = xlev)
  contr = lapply(object$data, function(.) attr(., "contrasts"))
  contr = contr[!sapply(contr, is.null)]
  X = model.matrix(trms, m, contrasts.arg = contr)
  V = vcov(object)
  nbasis = estimability::all.estble
  dfargs = list()
  dffun = function(k, dfargs) Inf
  misc = NULL
  bhat = coef(object)

  return(list(X = X, bhat = bhat, nbasis = nbasis, V = V, dffun = dffun, dfargs = dfargs))
}
