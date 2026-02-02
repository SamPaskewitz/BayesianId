#' Support for the emmeans package.
#' #' These functions should not be directly used.
#' See https://cran.r-project.org/web//packages//emmeans/vignettes/xtending.html
#' Also see the brms version: https://github.com/paul-buerkner/brms/blob/master/R/emmeans.R

#' @export
#' @method recover_data breg
recover_data.breg = function(object){
  df = object$data[,object$formula_info$fixed_main]
  if(object$center){ # center data if needed
    is_factor = sapply(df, is.factor)
    if(!all(is_factor)){
      df[,!is_factor] = scale(df[,!is_factor], center = TRUE, scale = FALSE)
    }
  }
  attr(df, "call") = object$call
  attr(df, "terms") = terms(object$formula)
  attr(df, "predictors") = object$formula_info$fixed_main
  attr(df, "responses") = object$formula_info$lhs
  return(df)
}

#' @export
#' @method emm_basis breg
emm_basis.breg = function(object, trms, xlev, grid, ...){

  # trying to follow the example of brms
  #mu_samples = posterior_linpred(obj = object,
                                 #newdata = grid)
  #bhat = apply(mu_samples, 2, mean) # posterior means of linear predictor (mu)
  #V = cov(bhat) # posterior covariance matrix of linear predictor (mu)
  #X = diag(ncol(mu_samples)) # just following the brms code here (shrug)

  # "straight" emmeans extension (ignoring what brms does)
  m = model.frame(trms, grid, na.action = na.pass, xlev = xlev)
  X = model.matrix(trms, m, contrasts.arg = contr_banova)
  bhat = coef(object)
  V = vcov(object)

  output = list(X = X,
                bhat = bhat,
                nbasis = matrix(NA), # required by emmeans but not used
                V = V,
                dffun = function(k, dfargs){Inf}, # required by emmeans but not used
                dfargs = list() # required by emmeans but not used
                )
  return(output)
}
