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
    stop("Summary type not recognized. Please use 'help(summary.bma)' for options.")
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
    # post probs for models that include the coef ("pi")
    pi = exp(obj$log_model_evidence[incl] + log(obj$prior_model_probs[incl]) - lse(obj$log_model_evidence[incl] + log(obj$prior_model_probs[incl])))
    # posterior means from models that include the coef
    mu = sapply(obj$fit_list[incl], function(x){coef(x)[coef_name]})
    # posterior SD's from models that include the coef
    sigma = sapply(obj$fit_list[incl], function(x){vcov(x)[coef_name, coef_name] |> sqrt()})
    # Bayesian model averaging (Hoeting, Madigan, Raftery, & Volinsky, 1999)
    if(sum(incl) > 1){ # more than one model includes the coef
      est_table[i, "mean"] = sum(pi*mu)
      est_table[i, "sd"] = sqrt(sum(pi*(sigma^2 + mu^2)) - est_table[i, "mean"]^2)
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

#' Get BMA estimates of contrasts (for interpreting factors).
#' @param obj A "bma" object.
#' @param factors The names(s) of the factor or factors for which to compute contrasts. If multiple factor names are listed, then the corresponding interaction contrasts are computed. See the examples below.
#' @param ref Integer or character specifying which level/level combination to use as the reference. If there are multiple factors, then combine factor level names in order, separated by spaces. See the examples below.
#' @param pretty Logical. If TRUE, then the output is printed in an easy to read format, but of the "character" data type. If FALSE then the raw numeric output is returned, without rounding etc.
#' @returns A table (data frame) with the following information:
#' mean: posterior mean
#' sd: posterior standard deviation
#' 2.5 %: lower end of 95% posterior credible interval
#' 97.5 %: upper end of 95% posterior credible interval
#' @details
#' This is based on the 'contrast' method of the emmeans package. That package is used to compute the expected marginal means and posterior standard deviations, which are then combined using Bayesian model averaging (BMA). The contrasts computed are based on the 'trt.vs.ctrl' method.
#' All estimates (posterior mean, standard deviation, and credible intervals) are computed only using models that include all of the relevant factors.
#' Posterior credible intervals are computed using a normal approximation.
#' ** ADD EXAMPLES
#' @importFrom emmeans emmeans
#' @importFrom emmeans contrast
#' @export
#' @method contrast bma
contrast.bma = function(obj, factors, ref = 1, pretty = TRUE){
  # check that "factors" is of type "character"
  if(!is.character(factors)){
    stop("The argument 'factors' should be of type 'character'.")
  }
  # Does each model include all the factors?
  if(length(factors) == 1){
    incl = obj$model_info$included_table[,factors]
  } else{
    incl = apply(obj$model_info$included_table[,factors], 1, all) |> as.vector()
  }
  # if no models include all the factors, return an error
  if(sum(incl) == 0){
    stop("None of the models includes all requested factors.")
  }
  # post probs for models that include the factors ("pi")
  pi = exp(obj$log_model_evidence[incl] + log(obj$prior_model_probs[incl]) - lse(obj$log_model_evidence[incl] + log(obj$prior_model_probs[incl])))
  # compute emmeans for all selected models
  emmeans_list = lapply(obj$fit_list[incl], function(x){emmeans(x, specs = factors)})
  # compute contrasts for all selected models
  contrast_list = lapply(emmeans_list, function(x){contrast(x, method = "trt.vs.ctrl", ref = ref, infer = FALSE) |> as.data.frame()})
  nc = nrow(contrast_list[[1]])
  # set up table for BMA contrasts
  contrast_table = data.frame(row.names = contrast_list[[1]]$contrast, mean = rep(0.0, nc), sd = rep(1.0, nc), "2.5 %" = rep(0.0, nc), "97.5 %" = rep(0.0, nc), check.names = FALSE)
  # Bayesian model averaging (Hoeting, Madigan, Raftery, & Volinsky, 1999)
  if(sum(incl) > 1){ # multiple models include the factors
    for(i in 1:nrow(contrast_table)){
      mu = sapply(contrast_list, function(x){x$estimate[i]})
      sigma = sapply(contrast_list, function(x){x$SE[i]})
      contrast_table[i, "mean"] = sum(mu*pi)
      contrast_table[i, "sd"] = sqrt(sum(pi*(sigma^2 + mu^2)) - contrast_table[i, "mean"]^2)
      contrast_table[i, "2.5 %"] = qmix(p = 0.025, pi = pi, mu = mu, sigma = sigma)
      contrast_table[i, "97.5 %"] = qmix(p = 0.975, pi = pi, mu = mu, sigma = sigma)
    }
  } else{ # only one model includes the factors
    contrast_table[,"mean"] = contrast_list[[1]]$estimate
    contrast_table[,"sd"] = contrast_list[[1]]$SE
    contrast_table[,"2.5 %"] = qnorm(p = 0.025, mean = contrast_table[,"mean"], sd = contrast_table[,"sd"])
    contrast_table[,"97.5 %"] = qnorm(p = 0.975, mean = contrast_table[,"mean"], sd = contrast_table[,"sd"])
  }
  # make output "pretty" if desired
  if(pretty){
    contrast_table = contrast_table |> signif(digits = 5)
  }

  return(contrast_table)
}
