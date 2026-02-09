#' ***
#' @param formula_info **
#' @param data A data frame
#' @param family ****
#' @param prior_scale ****
#' @returns A list with the following elements:
#' data: a modified data frame that is ready for regression
#' x_numeric_names: names of data variables that are numeric
#' x_factor_names: names of data variables that are factors
#' @details
#' * DESCRIBE THE CONTRAST CODES ETC
#'

make_stan_data = function(formula_info, data, family = "normal_linear", prior_scale = 1){
  # ** convert Y to an integer vector if needed **
  if(family %in% c("bernoulli_logistic")){
    Y = as.integer(data[, formula_info$lhs]) - 1
  } else{
    Y = data[, formula_info$lhs]
  }

  if(!formula_info$intercept_only){
    # ***** NOT INTERCEPT-ONLY *****

    # ** set up design matrix (minus intercept) **
    X = model.matrix(formula(formula_info$rhs), data = data)[,-1,drop = FALSE]

    # ** figure out which predictors are factors **
    is_factor = sapply(data[,formula_info$x_names,drop=FALSE], is.factor)
    is_numeric = !is_factor
    x_factor_names = formula_info$x_names[is_factor]
    x_numeric_names = formula_info$x_names[is_numeric]

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
                     Y = Y,
                     Ymean = mean(Y),
                     K = ncol(X),
                     X = X,
                     Xcol_scale = Xcol_scale,
                     prior_only = FALSE,
                     prior_scale = prior_scale)

    # add data for censoring if needed
    if(family == "right_censored_linear"){
      Ymax = max(Y)
      which_ncens = which(Y < Ymax)
      which_cens = which(Y == Ymax)
      stan_data = c(stan_data,
                    list(Nncens = length(which_ncens),
                         Ncens = length(which_cens),
                         Ymax = Ymax,
                         which_ncens = which_ncens,
                         which_cens = which_cens)
                    )
    }
  } else{
    # ***** INTERCEPT-ONLY *****
    stan_data = list(N = nrow(data),
                     Y = Y,
                     Ymean = mean(Y),
                     prior_only = FALSE,
                     prior_scale = prior_scale)

    # add data for censoring if needed
    if(family == "right_censored_linear"){
      Ymax = max(Y)
      which_ncens = which(Y < Ymax)
      stan_data = c(stan_data,
                    list(Nncens = length(which_ncens),
                         Ncens = nrow(data) - length(which_ncens),
                         Ymax = Ymax,
                         which_ncens = which_ncens)
      )
    }
  }

  return(stan_data)
}
