#' Print basic information about a "reg_bma" object.
#' @param obj A "reg_bma" object.
#' @export
#' @method print reg_bma
print.reg_bma = function(obj){
  cat("Full model:", as.character(obj$model_info$formulas[[1]])[[1]])
  cat("\nModel class:", obj$model_class)
  cat("\nNumber of submodels:", obj$model_info$n_models)
}

#' Summarize information about a "reg_bma" object.
#' @param obj A "reg_bma" object.
#' @param type Type of summary. Options are "terms", "est", and "models" (defaults to "terms").
#' @param digits_to_round Number of digits to round results to.
#' @details
#' GIVE INFO ABOUT SUMMARY TYPES
#'
#' @export
#' @method summary reg_bma
summary.reg_bma = function(obj, type = "terms", digits_to_round = 3){
  if(type == "terms"){
    tab = data.frame("p(β≠0)" = obj$prior_term_probs,
                     "p(β≠0|D)" = obj$post_term_probs,
                     "prior_odds" = obj$prior_term_probs/(1 - obj$prior_term_probs),
                     "post_odds" = obj$post_term_probs/(1 - obj$post_term_probs),
                     check.names = FALSE # prevent names from getting messed up
    )
    tab[, "BF"] = tab[, "post_odds"]/tab[, "prior_odds"]
    row.names(tab) = obj$model_info$term_names
  } else if(type == "est"){
    tab = coef(obj)
  } else if(type == "models"){
    tab = data.frame("p(M)" = obj$prior_model_probs,
                     "p(M | D)" = obj$post_model_probs,
                     "prior_odds" = obj$prior_model_odds,
                     "post_odds" = obj$post_model_odds,
                     "BF" = obj$bfs,
                     check.names = FALSE # prevent names from getting messed up
    )
    row.names(tab) = obj$model_info$model_names
    } else{
    warning("Summary type not recognized. Please choose 'terms', 'est', or 'models'.")
    }
  tab = round(tab, digits = digits_to_round)
  return(tab)
}

#' Get BMA estimates of coefficients.
#' @param obj A "reg_bma" object.
#' @returns A table (data frame) with the following information:
#' mean: posterior mean
#' sd: posterior standard deviation
#' 2.5 %: lower end of 95% posterior credible interval
#' 97.5 %: upper end of 95% posterior credible interval
#' @details
#' All estimates (posterior mean, standard deviation, and credible intervals) are computed only using models that include the term in question. In other words, they should be interpreted as estimates of the coefficient IF it is included (i.e. is non-zero).
#' Posterior credible intervals are computed using a normal approximation.
#' NOTE: This does not yet work for factors.
#' @export
#' @method coef reg_bma
coef.reg_bma = function(obj){
  # figure out coefficient names
  coef_names = colnames(model.matrix(obj$fit_list[[1]]))
  n_coef = length(coef_names)

  # set up table for coefficients
  est_table = data.frame(mean = rep(0.0, times = n_coef),
                         sd = rep(0.0, times = n_coef),
                         "2.5 %" = rep(0.0, times = n_coef),
                         "97.5 %" = rep(0.0, times = n_coef),
                         check.names = FALSE)
  row.names(est_table) = coef_names

  # fill out table for coefficients
  for(i in 1:n_coef){
    coef_name = coef_names[i]
    incl = lapply(obj$fit_list, function(x){coef_name %in% colnames(model.matrix(x))}) |> unlist() # Does each model include the coef?
    pi = obj$post_model_odds[incl]/sum(obj$post_model_odds[incl]) # post probs for models that include the coef
    mu = lapply(obj$fit_list[incl], function(x){coef(x)[coef_name]}) |> unlist() # means from models that include the coef
    sigma = lapply(obj$fit_list[incl], function(x){vcov(x)[coef_name, coef_name] |> sqrt()}) |> unlist() # SD's from models that include the coef

    if(sum(incl) > 1){ # more than one model includes the coefficient
      est_table[i, "mean"] = sum(pi*mu)
      est_table[i, "sd"] = sqrt(sum(pi*(sigma^2 + est_table[i, "mean"]^2)) - est_table[i, "mean"]^2)
      est_table[i, "2.5 %"] = qmix(p = 0.025, pi = pi, mu = mu, sigma = sigma)
      est_table[i, "97.5 %"] = qmix(p = 0.975, pi = pi, mu = mu, sigma = sigma)
    } else{ # only one model includes the coef_name
      est_table[i, "mean"] = mu
      est_table[i, "sd"] = sigma
      est_table[i, "2.5 %"] = qnorm(p = 0.025, mean = mu, sd = sigma)
      est_table[i, "97.5 %"] = qnorm(p = 0.975, mean = mu, sd = sigma)
    }
  }

  return(est_table)
}
