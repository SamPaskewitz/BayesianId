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
#' @param type Type of summary. Options are "dir_probs", "dir_BF", "term_probs", "term_odds", "model_probs", "model_odds", and "est" (defaults to "term_probs"). See "Details".
#' @param pretty Logical. If TRUE, then the output is printed in an easy to read format, but of the "character" data type. If FALSE then the raw numeric output is returned, without rounding etc.
#' @details Here is information about the different summary types.
#' * dir_probs: Posterior probabilities from tests of whether each coefficient is negative (β<0), positive (β>0), or zero (β=0). Coefficients for factor contrast codes are excluded because they are not easily interpretable. The point null hypothesis has the same prior probability as in Bayesian model averaging; the two directional hypotheses have equal prior probabilities (each equal to half the prior probability for term inclusion in BMA).
#' * dir_odds: Bayes factors, prior odds, and posterior odds from tests of whether each coefficient is negative (β<0) or positive (β>0). The comparison hypothesis (the denominator) is the null hypothesis that β=0. Coefficients for factor contrast codes are excluded because they are not easily interpretable. Two directional hypotheses have equal prior odds, each equal to half the prior odds for term inclusion in BMA.
#' * term_probs: Prior and posterior probabilities from tests of whether each model term should be included (β≠0) or omitted (β=0).
#' * term_odds: Bayes factors, prior odds, and posterior odds from tests of whether each model term should be included (β≠0) or omitted (β=0).
#' * model_probs: Prior and posterior probabilities for each model.
#' * model_odds: Bayes factors, prior odds, and posterior odds for each model. The full model is used as the denominator for model comparison, e.g. the Bayes factor for model i is defined as p(D | full model)/p(D | model i).
#' * est: Estimates (posterior mean, SD, and 95% credible interval) for model coefficients. These are computed using a mixture of normals approximation.
#' @md
#' @export
#' @method summary bma
summary.bma = function(obj, type = "term_probs", pretty = TRUE){
  if(type == "term_probs"){
    tab = data.frame("p(β=0)" = 1 - obj$prior_term_probs,
                     "p(β=0|D)" = 1 - obj$post_term_probs,
                     "p(β≠0)" = obj$prior_term_probs,
                     "p(β≠0|D)" = obj$post_term_probs,
                     check.names = FALSE # prevent names from getting messed up
    )
    row.names(tab) = obj$model_info$term_names
  } else if(type == "term_odds"){
    tab = data.frame("prior_odds" = bound_ratios(obj$prior_term_probs/(1 - obj$prior_term_probs)),
                     "post_odds" = bound_ratios(obj$post_term_probs/(1 - obj$post_term_probs)),
                     check.names = FALSE # prevent names from getting messed up
    )
    tab[, "BF"] = bound_ratios(tab[, "post_odds"]/tab[, "prior_odds"])
    row.names(tab) = obj$model_info$term_names
  } else if(type == "est"){
    tab = rbind(coef(obj)[intersect(obj$coef_names, obj$term_names), c("mean", "sd", "2.5 %", "97.5 %")],
                show_emmeans(obj, factors = obj$term_names[obj$is_factor]))
  } else if(type == "dir_odds"){
    tab = dir_tests(obj, type = "odds")
  } else if(type == "dir_probs"){
    tab = dir_tests(obj, type = "probs")
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
    } else if(type %in% c("term_odds", "model_odds", "dir_odds")){
      tab = tab |> signif(digits = 3) |> format(scientific = TRUE)
    } else if(type == "est"){
      tab = tab |> signif(digits = 3)
    }
  }
  # return output
  cat("Reminder: all results are conditional on modeling assumptions.")
  return(tab)
}

#' Compute Bayes factors, posterior odds etc. (or posterior/prior probabilities) for directional hypotheses (with the point null hypothesis as the denominator).
#' @param obj A "bma" object.
#' @param type "odds" or "probs"
#' @details This function is not designed to be used by the analyst and is therefore not exported.
dir_tests = function(obj, type = "odds"){
  # get applicable terms (terms with same names as coefficients, thus excluding factor contrast codes etc.)
  to_use = intersect(obj$coef_names, obj$term_names)
  # log Bayes factors for the encompassing hypothesis (H1) vs. point null (H0)
  prior_odds10 = obj$prior_term_probs[to_use]/(1 - obj$prior_term_probs[to_use])
  post_odds10 = obj$post_term_probs[to_use]/(1 - obj$post_term_probs[to_use])
  log_BF10 = log(bound_ratios(post_odds10/prior_odds10))
  # log Bayes factors for directional hypotheses (H2 and H3) vs. the encompassing hypothesis (H1)
  # we use the Savage-Dickey style encompassing prior approach (BF = post/prior)
  # because the prior is symmetric around 0, both prior directional probabilities are 0.5
  log_BF21 = log(coef(obj)[to_use,"p(β<0|D,β≠0)"]) - log(0.5)
  log_BF31 = log(coef(obj)[to_use,"p(β>0|D,β≠0)"]) - log(0.5)
  # log Bayes factors for directional hypotheses (H2 and H3) vs. the point null (H0)
  log_BF20 = log_BF21 + log_BF10
  log_BF30 = log_BF31 + log_BF10

  # compute log prior odds (prior odds = half of prior odds for term inclusion)
  log_prior_odds20 = log(0.5) + log(prior_odds10)
  log_prior_odds30 = log_prior_odds20

  # compute log posterior odds
  log_post_odds20 = log_BF20 + log_prior_odds20
  log_post_odds30 = log_BF30 + log_prior_odds30

  # put results in a table
  if(type == "odds"){
    tab = data.frame("prior_odds (β<0)" = exp(log_prior_odds20),
                     "post_odds (β<0)" = exp(log_post_odds20),
                     "BF (β<0)" = exp(log_BF20),
                     "prior_odds (β>0)" = exp(log_prior_odds30),
                     "post_odds (β>0)" = exp(log_post_odds30),
                     "BF (β>0)" = exp(log_BF30),
                     check.names = FALSE # prevent names from getting messed up
    )
  } else if(type == "probs"){
    # compute log posterior probs
    post_prob0 <- post_prob2 <- post_prob3 <- rep(0.0, times = length(to_use))
    for(i in 1:length(to_use)){
      log_prob_denom = lse(c(log_post_odds20[i], log_post_odds30[i], 0))
      post_prob0[i] = bound_probs(exp(0 - log_prob_denom))
      post_prob2[i] = bound_probs(exp(log_post_odds20[i] - log_prob_denom))
      post_prob3[i] = bound_probs(exp(log_post_odds30[i] - log_prob_denom))
    }

    tab = data.frame("p(β=0)" = 1/(prior_odds10 + 1),
                     "p(β=0|D)" = post_prob0,
                     "p(β<0)" = 0.5*prior_odds10/(prior_odds10 + 1),
                     "p(β<0|D)" = post_prob2,
                     "p(β>0)" = 0.5*prior_odds10/(prior_odds10 + 1),
                     "p(β>0|D)" = post_prob3,
                     check.names = FALSE # prevent names from getting messed up
                     )
  }
  row.names(tab) = to_use
  return(tab)
}

#' Get BMA estimates of coefficients.
#' @param obj A "bma" object.
#' @returns A table (data frame) with the following information:
#'  * mean: posterior mean
#'  * sd: posterior standard deviation
#'  * 2.5 %: lower end of 95% posterior credible interval
#'  * 97.5 %: upper end of 95% posterior credible interval
#'  * p(β<0|D,β≠0): posterior probability that the coefficient is negative, if it's non-zero
#'  * p(β>0|D,β≠0): posterior probability that the coefficient is positive, if it's non-zero
#' @details
#' All estimates (posterior mean, standard deviation, credible intervals, and probability of direction) are computed only using models that include the term in question. In other words, they should be interpreted as estimates of the coefficient IF it is included (i.e. is non-zero).
#' Posterior credible intervals and directional probabilities are computed using a mixture of normals approximation.
#' @md
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

#' Get a list of emmeans objects for submodels that include all the factors in the "factors" argument, along with their re-normalized probabilities (pi).
#' This function is designed only for use internally (within the package), so it is not exported.
emmeans_by_model = function(obj, factors){
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
  return(list(emmeans_list = emmeans_list, pi = pi, incl = incl))
}

#' Get BMA estimates of factor level means (for interpreting factors).
#' @param obj A "bma" object.
#' @param factors The names(s) of the factor or factors for which to compute means.
#' @param pretty Logical. If TRUE, then the output is printed in an easy to read format, but of the "character" data type. If FALSE then the raw numeric output is returned, without rounding etc.
#' @returns A table (data frame) with the following information:
#' * mean: posterior mean
#' * sd: posterior standard deviation
#' * 2.5 %: lower end of 95% posterior credible interval
#' * 97.5 %: upper end of 95% posterior credible interval
#' @details
#' This is based on the emmeans package. That package is used to compute the expected marginal means and posterior standard deviations, which are then combined using Bayesian model averaging (BMA).
#' All estimates (posterior mean, standard deviation, and credible intervals) are computed only using models that include all of the relevant factors.
#' Posterior credible intervals are computed using a normal approximation.
#' @md
#' @importFrom emmeans emmeans
#' @export
show_emmeans = function(obj, factors, pretty = TRUE){
  # get emmeans for all selected models
  em_list = emmeans_by_model(obj, factors)
  emmeans_list = em_list$emmeans_list
  pi = em_list$pi
  incl = em_list$incl
  # set up table for BMA factor means
  if(length(factors) == 1){
    row_names = summary(emmeans_list[[1]])[ , factors]
  } else{
    row_names = apply(summary(emmeans_list[[1]])[ , factors], 1, paste, collapse = ".")
  }
  nm = length(row_names)
  means_table = data.frame(row.names = row_names, mean = rep(0.0, nm), sd = rep(1.0, nm), "2.5 %" = rep(0.0, nm), "97.5 %" = rep(0.0, nm), check.names = FALSE)
  # Bayesian model averaging (Hoeting, Madigan, Raftery, & Volinsky, 1999)
  for(i in 1:nrow(means_table)){
    if(sum(incl) > 1){ # multiple models include the factors
      mu = sapply(emmeans_list, function(x){summary(x)$emmean[i]})
      sigma = sapply(emmeans_list, function(x){summary(x)$SE[i]})
      means_table[i, "mean"] = sum(mu*pi)
      means_table[i, "sd"] = sqrt(sum(pi*(sigma^2 + mu^2)) - means_table[i, "mean"]^2)
      means_table[i, "2.5 %"] = qmix(p = 0.025, pi = pi, mu = mu, sigma = sigma)
      means_table[i, "97.5 %"] = qmix(p = 0.975, pi = pi, mu = mu, sigma = sigma)
    } else{ # only one model includes the factors
      mu = summary(emmeans_list[[1]])$emmean[i]
      sigma = summary(emmeans_list[[1]])$SE[i]
      means_table[i, "mean"] = mu
      means_table[i, "sd"] = sigma
      means_table[i, "2.5 %"] = qnorm(p = 0.025, mean = mu, sd = sigma)
      means_table[i, "97.5 %"] = qnorm(p = 0.975, mean = mu, sd = sigma)
    }
  }
  # make output "pretty" if desired
  if(pretty){
    means_table = means_table |> signif(digits = 5)
  }
  return(means_table)
}

#' Get BMA estimates of contrasts (for interpreting factors).
#' @param obj A "bma" object.
#' @param factors The names(s) of the factor or factors for which to compute contrasts. If multiple factor names are listed, then the corresponding interaction contrasts are computed. See the examples below.
#' @param ref Integer or character specifying which level/level combination to use as the reference. If there are multiple factors, then combine factor level names in order, separated by spaces. Optionally you can input "grand_mean" to use the average over all levels as the reference (i.e. compute effect contrasts).
#' @param pretty Logical. If TRUE, then the output is printed in an easy to read format, but of the "character" data type. If FALSE then the raw numeric output is returned, without rounding etc.
#' @returns A table (data frame) with the following information:
#' * mean: posterior mean
#' * sd: posterior standard deviation
#' * 2.5 %: lower end of 95% posterior credible interval
#' * 97.5 %: upper end of 95% posterior credible interval
#' * p(c<0|D,c≠0): posterior probability that the contrast is negative (if non-zero)
#' * p(c>0|D,c≠0): posterior probability that the contrast is positive (if non-zero)
#' @details
#' This is based on the 'contrast' method of the emmeans package. That package is used to compute the expected marginal means and posterior standard deviations, which are then combined using Bayesian model averaging (BMA). The contrasts computed are based on the 'trt.vs.ctrl' method unless the argument 'ref' is set to 'grand_mean', in which case they are based on the 'eff' method.
#' All estimates (posterior mean, standard deviation, and credible intervals) are computed only using models that include all of the relevant factors.
#' Posterior credible intervals are computed using a normal approximation.
#' @md
#' @importFrom emmeans emmeans
#' @importFrom emmeans contrast
#' @export contrast
#' @export
#' @method contrast bma
contrast.bma = function(obj, factors, ref = 1, pretty = TRUE){
  # get emmeans for all selected models
  em_list = emmeans_by_model(obj, factors)
  emmeans_list = em_list$emmeans_list
  pi = em_list$pi
  incl = em_list$incl
  # compute contrasts for all selected models
  if(ref == "grand_mean"){
    contrast_list = lapply(emmeans_list, function(x){contrast(x, method = "eff", infer = FALSE) |> as.data.frame()})
  } else{
    contrast_list = lapply(emmeans_list, function(x){contrast(x, method = "trt.vs.ctrl", ref = ref, infer = FALSE) |> as.data.frame()})
  }
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
      mu = contrast_list[[1]]$estimate[i]
      sigma = contrast_list[[1]]$SE[i]
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

#' Plot the posterior distribution of a regression coefficient.
#' @param obj A "bma" object.
#' @param coef The coefficient whose posterior distribution you want to plot.
#' @details The posterior distribution is approximated with a mixture of normals approximation. Only models that include the coefficient are included, i.e. the distribution plotted is the posterior distribution for the coefficient given that it is non-zero.
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
