#' Prepare data for regression by giving factors appropriate contrast codes and (optionally) mean-centering numeric predictors.
#' @param data A data frame.
#' @param formula An object of class formula or brmsformula (or one that can be coerced to that classes): A symbolic description of the model to be fitted. The details of model specification are explained in brmsformula.
#' @param center Should the numeric (non-factor) predictor variables be mean-centered? Defaults to TRUE.
#' @returns A modified data frame that is ready for regression.
#' @details
#' * DESCRIBE THE CONTRAST CODES ETC
#'

prepare_data = function(data, formula, center = TRUE){
  # make a copy of the data
  prep_data = data

  # figure out the names of predictor variables and which ones are factors
  # also convert character variables to factors
  x_names = parse_formula(formula)$fixed_main
  is_factor = rep(FALSE, times = length(x_names))
  for(i in 1:length(x_names)){
    if(is.character(prep_data[, x_names[i]])){
      prep_data[, x_names[i]] = factor(prep_data[, x_names[i]])
    }
    is_factor[i] = is.factor(prep_data[, x_names[i]])
  }

  # give factors proper contrast codes (Rouder et al 2012)
  if(any(is_factor)){
    for(i in which(is_factor)){
      a = length(levels(prep_data[, x_names[i]])) # number of factor levels
      contrasts(prep_data[, x_names[i]]) = contr_banova(a)
    }
  }

  # mean-center numeric predictors (optionally)
  if(any(!is_factor) & center){
    non_factor_names = x_names[which(!is_factor)]
    prep_data = prep_data |> dplyr::mutate(dplyr::across(non_factor_names, ~ .x - mean(.x)))
  }

  return(prep_data)
}
