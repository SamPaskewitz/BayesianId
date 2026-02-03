#' Prepare data for being converted to a design matrix: convert character variables to factors, give appropriate contrasts for Bayesian inference, and optionally mean-center numeric predictors.
#' @param formula_info
#' @param data A data frame.
#' @param center ****
#' @returns
prepare_data = function(formula_info, data, center = TRUE){
  if(length(formula_info$x_names) == 0){
    return(data[,formula_info$y_name, drop = FALSE])
  } else{
      # ** convert character variables to factors **
      for(i in 1:length(formula_info$x_names)){
        if(is.character(data[,formula_info$x_names[i],drop = FALSE])){
          data[,formula_info$x_names[i],drop = FALSE] = factor(data[,formula_info$x_names[i],drop = FALSE])
        }
      }

      # ** figure out which predictors are factors **
      is_factor = sapply(data[,formula_info$x_names,drop=FALSE], is.factor)
      is_numeric = !is_factor
      x_factor_names = formula_info$x_names[is_factor]
      x_numeric_names = formula_info$x_names[is_numeric]

      # ** give factors proper contrast codes **
      if(any(is_factor)){
        for(i in 1:length(x_factor_names)){
          a = length(levels(data[,x_factor_names[i]])) # number of factor levels
          contrasts(data[, x_factor_names[i]]) = contr_banova(a)
        }
      }

      # ** center numeric predictors (optionally) **
      if(center & any(is_numeric)){
        data[,x_numeric_names] = data[,x_numeric_names] |> scale(center = TRUE, scale = FALSE) |> as.data.frame()
      }

      return(data[,c(formula_info$x_names, formula_info$y_name)])
  }
}
