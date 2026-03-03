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
    tab = coef(obj)[,c("mean", "sd", "2.5 %", "97.5 %")]
  } else if(type == "dir_BF"){
    # get applicable terms (terms with same names as coefficients)
    to_use = intersect(obj$coef_names, obj$term_names)
    # Bayes factors for the encompassing hypothesis (H1) vs. point null (H0)
    term_odds = summary(obj, type = "term_odds", pretty = FALSE)
    log_BF10 = log(term_odds[to_use, "post_odds"]) - log(term_odds[to_use, "prior_odds"])
    # Bayes factors for directional hypotheses (H2 and H3) vs. the encompassing hypothesis (H1)
    # we use the Savage-Dickey style encompassing prior approach (BF = post/prior)
    # because the prior is symmetric around 0, both prior direction probabilities are 0.5
    log_BF21 = log(coef(obj)[to_use,"p(β<0|D,β≠0)"]) - log(0.5)
    log_BF31 = log(coef(obj)[to_use,"p(β>0|D,β≠0)"]) - log(0.5)
    # Bayes factors for directional hypotheses (H2 and H3) vs. the point null (H0)
    BF20 = exp(log_BF21 + log_BF10)
    BF30 = exp(log_BF31 + log_BF10)
    tab = data.frame("β<0 vs. β=0" = BF20,
                     "β>0 vs. β=0" = BF30,
                     check.names = FALSE # prevent names from getting messed up
                     )
    row.names(tab) = to_use
  } else if(type == "dir_probs"){
    # compute posterior probabilities of directional hypotheses and the point null
    # we assume that the two dir hypotheses and point null are the only hypotheses
    # we will also assume equal prior odds (we might revise this later)
    dir_BF = summary(obj, type = "dir_BF", pretty = FALSE)
    BF20 = dir_BF[,"β<0 vs. β=0"]
    BF30 = dir_BF[,"β>0 vs. β=0"]
    BF00 = 1
    tab = data.frame("p(β<0|D)" = bound_probs(BF20/(BF20 + BF30 + 1)),
                     "p(β>0|D)" = bound_probs(BF30/(BF20 + BF30 + 1)),
                     "p(β=0|D)" = bound_probs(1/(BF20 + BF30 + 1)),
                     check.names = FALSE # prevent names from getting messed up
                     )
    row.names(tab) = row.names(dir_BF)

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
    if(type %in% c("term_probs", "model_probs", "dir_probs")){
      tab = tab |> format_prob()
    } else if(type %in% c("term_odds", "model_odds", "dir_BF")){
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
#' p(β<0|D,β≠0): posterior probability that the coefficient is negative
#' p(β>0|D,β≠0): posterior probability that the coefficient is positive
#' @details
#' All estimates (posterior mean, standard deviation, credible intervals, and probability of direction) are computed only using models that include the term in question. In other words, they should be interpreted as estimates of the coefficient IF it is included (i.e. is non-zero).
#' Posterior credible intervals and directional probabilities are computed using a mixture of normals approximation.
#' @export
#' @method coef bma
coef.bma = function(obj){
  # figure out coefficient names
  coef_names = obj$coef_names
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
      est_table[i, "p(β<0|D,β≠0)"] = pmix(q = 0, pi = pi, mu = mu, sigma = sigma)
    } else{ # only one model includes the coef
      est_table[i, "mean"] = mu
      est_table[i, "sd"] = sigma
      est_table[i, "2.5 %"] = qnorm(p = 0.025, mean = mu, sd = sigma)
      est_table[i, "97.5 %"] = qnorm(p = 0.975, mean = mu, sd = sigma)
      est_table[i, "p(β<0|D,β≠0)"] = pnorm(q = 0, mean = mu, sd = sigma)
    }
  }
  # fix probs that are slightly > 1 (by approx error) to be slightly < 1
  est_table[, "p(β<0|D,β≠0)"] = bound_probs(est_table[, "p(β<0|D,β≠0)"])
  # compute p(c>0|D,c≠0)
  est_table[, "p(β>0|D,β≠0)"] = 1 - est_table[, "p(β<0|D,β≠0)"]

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
#' p(c<0|D,c≠0): posterior probability that the contrast is negative (if non-zero)
#' p(c>0|D,c≠0): posterior probability that the contrast is positive (if non-zero)
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
  for(i in 1:nrow(contrast_table)){
    if(sum(incl) > 1){ # multiple models include the factors
      mu = sapply(contrast_list, function(x){x$estimate[i]})
      sigma = sapply(contrast_list, function(x){x$SE[i]})
      contrast_table[i, "mean"] = sum(mu*pi)
      contrast_table[i, "sd"] = sqrt(sum(pi*(sigma^2 + mu^2)) - contrast_table[i, "mean"]^2)
      contrast_table[i, "2.5 %"] = qmix(p = 0.025, pi = pi, mu = mu, sigma = sigma)
      contrast_table[i, "97.5 %"] = qmix(p = 0.975, pi = pi, mu = mu, sigma = sigma)
      contrast_table[i, "p(c<0|D,c≠0)"] = pmix(q = 0, pi = pi, mu = mu, sigma = sigma)
    } else{ # only one model includes the factors
      mu = contrast_list[[1]]$estimate
      sigma = contrast_list[[1]]$SE
      contrast_table[i, "mean"] = mu
      contrast_table[i, "sd"] = sigma
      contrast_table[i, "2.5 %"] = qnorm(p = 0.025, mean = mu, sd = sigma)
      contrast_table[i, "97.5 %"] = qnorm(p = 0.975, mean = mu, sd = sigma)
      contrast_table[i, "p(c<0|D,c≠0)"] = pnorm(q = 0, mean = mu, sd = sigma)
    }
  }
  # fix probs that are slightly > 1 (by approx error) to be slightly < 1
  contrast_table[, "p(c<0|D,c≠0)"] = bound_probs(contrast_table[, "p(c<0|D,c≠0)"])
  # compute p(c>0|D,c≠0)
  contrast_table[, "p(c>0|D,c≠0)"] = 1 - contrast_table[, "p(c<0|D,c≠0)"]

  # make output "pretty" if desired
  if(pretty){
    contrast_table[,c("mean", "sd", "2.5 %", "97.5 %")] = contrast_table[,c("mean", "sd", "2.5 %", "97.5 %")] |> signif(digits = 5)
    contrast_table[,c("p(c<0|D,c≠0)", "p(c>0|D,c≠0)")] = contrast_table[,c("p(c<0|D,c≠0)", "p(c>0|D,c≠0)")] |> format_prob()
  }

  return(contrast_table)
}

#' Plot the posterior distribution of estimates.
#' @param obj A "bma" object.
#' @param coef The coefficient whose posterior distribution you want to plot.
#' @details The posterior distribution is approximated with a mixture of normal distributions.
#' @importFrom dplyr mutate
#' @import ggplot2
#' @export
#' @method plot bma
plot.bma = function(obj, coef){
  # Does each model include the coef?
  incl = sapply(obj$fit_list, function(x){coef %in% get_coef_names(x)})
  # post probs for models that include the coef ("pi")
  pi = exp(obj$log_model_evidence[incl] + log(obj$prior_model_probs[incl]) - lse(obj$log_model_evidence[incl] + log(obj$prior_model_probs[incl])))
  # posterior means from models that include the coef
  mu = sapply(obj$fit_list[incl], function(x){coef(x)[coef]})
  # posterior SD's from models that include the coef
  sigma = sapply(obj$fit_list[incl], function(x){vcov(x)[coef, coef] |> sqrt()})
  # 0.001 and 0.999 quantiles (to determine the plot's x-axis range)
  lower = qmix(0.001, pi, mu, sigma)
  upper = qmix(0.999, pi, mu, sigma)
  # create a data frame for the plot
  plot_data = data.frame(beta = seq(from = lower, to = upper, length.out = 200)) |>
    mutate(post = dmix(beta, pi, mu, sigma))
  # make the plot
  plot_data |> ggplot(aes(x = beta, y = post)) +
    geom_line(color = "blue") + theme_bw() + ylab("posterior density")
}
