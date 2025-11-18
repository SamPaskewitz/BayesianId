#' Prepare data for regression by giving factors appropriate contrast codes and (optionally) mean-centering numeric predictors.
#' @param data A data frame.
#' @param formula An object of class formula or brmsformula (or one that can be coerced to that classes): A symbolic description of the model to be fitted. The details of model specification are explained in brmsformula.
#' @param center Should the numeric (non-factor) predictor variables be mean-centered? Defaults to TRUE.
#' @returns A list with the following elements:
#' data: a modified data frame that is ready for regression
#' x_numeric_names: names of data variables that are numeric
#' x_factor_names: names of data variables that are factors
#' @details
#' * DESCRIBE THE CONTRAST CODES ETC
#'

prepare_data = function(data, formula, center = TRUE){
  # make a copy of the data
  prep_data = data

  # figure out which predictors are factors
  # also convert character variables to factors
  x_names = parse_formula(formula)$fixed_main
  is_factor = rep(FALSE, times = length(x_names))
  for(i in 1:length(x_names)){
    if(is.character(prep_data[, x_names[i]])){
      prep_data[, x_names[i]] = factor(prep_data[, x_names[i]])
    }
    is_factor[i] = is.factor(prep_data[, x_names[i]])
  }
  x_factor_names = x_names[is_factor]

  # give factors proper contrast codes (Rouder et al 2012)
  if(any(is_factor)){
    for(i in which(is_factor)){
      a = length(levels(prep_data[, x_names[i]])) # number of factor levels
      contrasts(prep_data[, x_names[i]]) = contr_banova(a)
    }
  }

  # find numeric predictors and mean-center them (optionally)
  if(any(!is_factor)){
    x_numeric_names = x_names[which(!is_factor)]
    if(center){
      prep_data = prep_data |> dplyr::mutate(dplyr::across(x_numeric_names, ~ .x - mean(.x)))
    }
  }
  else{
    x_numeric_names = NULL
  }

  # collect function output
  output = list(data = prep_data,
                x_numeric_names = x_numeric_names,
                x_factor_names = x_factor_names)

  return(output)
}
