#' Compute Bayes factors, posterior odds etc. (or posterior/prior probabilities) for directional hypotheses (with the point null hypothesis as the denominator).
#' @param obj A "bma" object.
#' @param type "odds, "bf", or "probs"
#' @details This function is not designed to be used by the analyst and is therefore not exported. Instead, its results are shown using the "summary" method for bma objects.
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
    tab = data.frame("prior_odds (β<0 vs β=0)" = exp(log_prior_odds20),
                     "post_odds (β<0 vs β=0)" = exp(log_post_odds20),
                     "prior_odds (β>0 vs β=0)" = exp(log_prior_odds30),
                     "post_odds (β>0 vs β=0)" = exp(log_post_odds30),
                     check.names = FALSE # prevent names from getting messed up
    )
  } else if(type == "bf") {
    tab = data.frame("BF (β<0 vs. β=0)" = exp(log_BF20),
                     "BF (β>0 vs. β=0)" = exp(log_BF30),
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

