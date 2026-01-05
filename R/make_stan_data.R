#' Prepare data for regression by giving factors appropriate contrast codes and (optionally) mean-centering numeric predictors.
#' @param formula An object of class formula or brmsformula (or one that can be coerced to that classes): A symbolic description of the model to be fitted. The details of model specification are explained in brmsformula.
#' @param data A data frame.
#' @param prior_scale ****
#' @param center ****
#' @returns A list with the following elements:
#' data: a modified data frame that is ready for regression
#' x_numeric_names: names of data variables that are numeric
#' x_factor_names: names of data variables that are factors
#' @details
#' * DESCRIBE THE CONTRAST CODES ETC
#'

make_stan_data = function(formula, data, prior_scale = 1, center = TRUE){
  # ***** setup *****
  parsed_formula = parse_formula(formula) # parse formula
  x_names = parsed_formula$fixed_main
  intercept_only = (length(parsed_formula$fixed) == 0) # is it an intercept-only model?

  if(!intercept_only){
    # ***** NOT INTERCEPT-ONLY *****

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
    X = model.matrix(formula, data = data)[,-1,drop = FALSE]

    # ** compute Xcol_scale **

    # initialize with default of 1.0 (factor contrasts retain this)
    Xcol_scale = rep(1.0, times = ncol(X))
    names(Xcol_scale) = colnames(X)

    # figure out which columns of X are numeric variables or interactions
    Xcol_is_numeric = colnames(X) %in% x_numeric_names
    Xcol_is_interaction = sapply(colnames(X), is_interaction)

    # adjust numeric main effects by their SD's
    if(any(Xcol_is_numeric)){
      Xcol_numeric_names = colnames(X)[which(Xcol_is_numeric)]
      Xcol_scale[Xcol_numeric_names] = apply(data[,Xcol_numeric_names,drop=FALSE], 2, sd)
      # adjust interactions by the SD's of any numeric components
      if(any(Xcol_is_interaction)){
        Xcol_interaction_names = colnames(X)[which(Xcol_is_interaction)]
        for(i in 1:length(Xcol_interaction_names)){
          Xcol_name = Xcol_interaction_names[i]
          parts = strsplit(Xcol_name, "\\:") |> unlist()
          numeric_parts = parts[parts %in% x_numeric_names]
          if(length(numeric_parts) > 0){
            Xcol_scale[Xcol_name] = prod(Xcol_scale[numeric_parts])
          }
        }
      }
    }

    # convert to an array so Stan doesn't drop dimensions when there's one column
    Xcol_scale = array(Xcol_scale, dim = ncol(X))
    names(Xcol_scale) = colnames(X)

    # ** collect function output **
    stan_data = list(N = nrow(data),
                     Y = data[, parsed_formula$lhs],
                     Ymean = mean(data[, parsed_formula$lhs]),
                     K = ncol(X),
                     X = X,
                     Xcol_scale = Xcol_scale,
                     prior_only = FALSE,
                     prior_scale = prior_scale)
  } else{
    # ***** INTERCEPT-ONLY *****
    stan_data = list(N = nrow(data),
                     Y = data[, parsed_formula$lhs],
                     Ymean = mean(data[, parsed_formula$lhs]),
                     prior_only = FALSE,
                     prior_scale = prior_scale)
  }

  return(stan_data)
}
