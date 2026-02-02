#' Set up the model matrix ("X") in the form suitable for passing to stan_data.
#' @param formula An object of class formula or brmsformula (or one that can be coerced to that classes): A symbolic description of the model to be fitted. The details of model specification are explained in brmsformula.
#' @param data A data frame.
#' @param center ****
#' @returns The model matrix, with factors given appropriate contrasts for Bayesian inference, numeric predictors mean-centered (optionally), and without the intercept column.
make_X = function(formula, data, center){
  parsed_formula = parse_formula(formula) # parse formula
  x_names = parsed_formula$fixed_main

  # ** convert character variables to factors **
  for(i in 1:length(x_names)){
    if(is.character(data[,x_names[i],drop = FALSE])){
      data[,x_names[i],drop = FALSE] = factor(data[,x_names[i],drop = FALSE])
    }
  }

  # ** figure out which predictors are factors **
  is_factor = sapply(data[,x_names,drop=FALSE], is.factor)
  is_numeric = !is_factor
  x_factor_names = x_names[is_factor]
  x_numeric_names = x_names[is_numeric]

  # ** give factors proper contrast codes **
  if(any(is_factor)){
    for(i in 1:length(x_factor_names)){
      a = length(levels(data[,x_factor_names[i]])) # number of factor levels
      contrasts(data[, x_factor_names[i]]) = contr_banova(a)
    }
  }

  # ** center numeric predictors (optionally) **
  if(center & any(is_numeric)){
    data[,x_numeric_names] = data[,x_numeric_names] |> scale(center = TRUE, scale = FALSE)
  }

  # ** set up design matrix (minus intercept) **
  formula_without_lhs = formula(paste0("~ ", paste(parsed_formula$fixed, collapse = " + ")))
  X = model.matrix(formula_without_lhs, data = data)[,-1,drop = FALSE]

  return(X)
}
