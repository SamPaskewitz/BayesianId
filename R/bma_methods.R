#' Print basic information about a "bma" object.
#' @param obj A "bma" object.
#' @export
#' @method print bma
print.bma = function(obj){
  cat("Full model:", obj$model_info$model_names[[1]])
  cat("\nMost probable model:", (obj$post_model_odds |> which.max() |> names())[1])
  cat("\nModel class:", obj$model_class)
  cat("\nNumber of submodels:", obj$model_info$n_models)
}

#' Summarize information about a "bma" object.
#' @param obj A "bma" object.
#' @param type Type of summary. Options are "term_probs", "term_odds", "model_probs", "model_odds", and "est" (defaults to "term_probs").
#' @param pretty Logical. If TRUE, then the output is printed in an easy to read format, but of the "character" data type. If FALSE then the raw numeric output is returned, without rounding etc.
#' @details
#' GIVE INFO ABOUT SUMMARY TYPES
#' The first model in the list is used as the denominator for model comparison Bayes factors, i.e. the Bayes factor for model i is defined as p(D | first model)/p(D | model i).
#' @export
#' @method summary bma
summary.bma = function(obj, type = "term_probs", pretty = TRUE){
  if(type == "term_probs"){
    tab = data.frame("p(β≠0)" = obj$prior_term_probs,
                     "p(β≠0|D)" = obj$post_term_probs,
                     check.names = FALSE # prevent names from getting messed up
    )
    row.names(tab) = obj$model_info$term_names
  } else if(type == "term_odds"){
    tab = data.frame("prior_odds" = obj$prior_term_probs/(1 - obj$prior_term_probs),
                     "post_odds" = obj$post_term_probs/(1 - obj$post_term_probs),
                     check.names = FALSE # prevent names from getting messed up
    )
    tab[, "BF"] = tab[, "post_odds"]/tab[, "prior_odds"]
    row.names(tab) = obj$model_info$term_names
  } else if(type == "est"){
    tab = coef(obj)
  } else if(type == "model_probs"){
    tab = data.frame("p(M)" = obj$prior_model_probs,
                     "p(M | D)" = obj$post_model_probs,
                     check.names = FALSE # prevent names from getting messed up
    )
    row.names(tab) = obj$model_info$model_names
    } else if(type == "model_odds"){
      tab = data.frame("prior_odds" = obj$prior_model_odds,
                       "post_odds" = obj$post_model_odds,
                       "BF" = obj$model_bfs,
                       check.names = FALSE # prevent names from getting messed up
      )
      row.names(tab) = obj$model_info$model_names
    } else{
    warning("Summary type not recognized. Please choose 'terms', 'est', or 'models'.")
    }
  # make output pretty (optionally)
  if(pretty){
    if(type %in% c("term_probs", "model_probs")){
      tab = tab |> round(digits = 3)
      tab[tab[,1] == 1.000, 1] = ">0.999"
      tab[tab[,1] == 0.000, 1] = "<0.001"
      tab[tab[,2] == 1.000, 2] = ">0.999"
      tab[tab[,2] == 0.000, 2] = "<0.001"
    } else if(type %in% c("term_odds", "model_odds")){
      tab = tab |> signif(digits = 3) |> format(scientific = TRUE)
    } else if(type == "est"){
      tab = tab |> signif(digits = 3)
    }
  }
  # return output
  cat("Reminder: all results are conditional on modeling assumptions.")
  return(tab)
}

#' Get BMA estimates of coefficients.
#' @param obj A "bma" object.
#' @returns A table (data frame) with the following information:
#' mean: posterior mean
#' sd: posterior standard deviation
#' 2.5 %: lower end of 95% posterior credible interval
#' 97.5 %: upper end of 95% posterior credible interval
#' @details
#' All estimates (posterior mean, standard deviation, and credible intervals) are computed only using models that include the term in question. In other words, they should be interpreted as estimates of the coefficient IF it is included (i.e. is non-zero).
#' Posterior credible intervals are computed using a normal approximation.
#' @export
#' @method coef bma
coef.bma = function(obj){
  # figure out coefficient names
  coef_names = get_coef_names(obj$fit_list[[1]])
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
    # Does each model include the coef?
    incl = sapply(obj$fit_list, function(x){coef_name %in% get_coef_names(x)})
    # exclude models with effectively zero posterior prob
    incl = incl*(obj$post_model_probs > 1e-2)
    # post probs for models that include the coef ("pi")
    pi = exp(obj$log_model_evidence[incl] + log(obj$prior_model_probs[incl]) - lse(obj$log_model_evidence[incl] + log(obj$prior_model_probs[incl])))
    # posterior means from models that include the coef
    mu = sapply(obj$fit_list[incl], function(x){coef(x)[coef_name]})
    # posterior SD's from models that include the coef
    sigma = sapply(obj$fit_list[incl], function(x){vcov(x)[coef_name, coef_name] |> sqrt()})

    if(sum(incl) > 1){ # more than one model includes the coef
      est_table[i, "mean"] = sum(pi*mu)
      est_table[i, "sd"] = sqrt(sum(pi*(sigma^2 + est_table[i, "mean"]^2)) - est_table[i, "mean"]^2)
      est_table[i, "2.5 %"] = qmix(p = 0.025, pi = pi, mu = mu, sigma = sigma)
      est_table[i, "97.5 %"] = qmix(p = 0.975, pi = pi, mu = mu, sigma = sigma)
    } else{ # only one model includes the coef
      est_table[i, "mean"] = mu
      est_table[i, "sd"] = sigma
      est_table[i, "2.5 %"] = qnorm(p = 0.025, mean = mu, sd = sigma)
      est_table[i, "97.5 %"] = qnorm(p = 0.975, mean = mu, sd = sigma)
    }
  }

  return(est_table)
}
