#' Conduct parameter inclusion tests. This is the most important function in the "BayesianId" package.
#'
#' @param brmsfit A brmsfit object (fitted regression model).
#' @param prior_par_probs Optional: a named vector giving the prior inclusion probabilities for parameters (names = parameters/terms). By default (if NULL) these are set to 0.5, which corresponds to uniform prior probabilities across models DOUBLE CHECK *.
#' @param bf_method Specifies the method used for computing Bayes factors. Current options are ** LIST AND GIVE DETAILS
#' @returns A list containing the following two elements: par_table (parameter prior and posterior probabilities/odds, Bayes factors), model_table (model prior and posterior probabilities/odds, Bayes factors)
#'
par_tests = function(brmsfit,
                     prior_par_probs = NULL
                     ){
  # get a list of models (full model plus restricted models)
  model_list = restricted_models(brmsfit$formula)

  # NEED TO WRITE AND TEST CODE FOR EVERYTHING BELOW

  # convert parameter prior probs to model prior probs
  prior_model_probs = probs_par_to_model(prior_par_probs, model_list$included_table)

  # compute Bayes factors for all models
  bfs = model_bfs(brmsfit, method = bf_method)

  # compute model posterior probabilties
  prior_model_odds = prior_model_probs/prior_model_probs[1]
  post_model_odds = bfs*prior_model_odds
  post_model_probs = post_model_odds/sum(post_model_odds)

  # convert model posterior probs to parameter posterior probs
  post_par_probs = probs_model_to_par(post_model_probs, model_list$included_table)

  # make nice output tables for parameters and models
  par_table = data.frame("p(β≠0)" = prior_par_probs,
                         "p(β≠0|D)" = post_par_probs,
                         "prior odds" = prior_par_probs/(1 - prior_par_probs),
                         "post odds" = post_par_probs/(1 - post_par_probs)
                         )
  par_table[, "BF"] = par_table[, "post odds"]/par_table[, "prior odds"]
  row.names(par_table) = model_list$fixed_names

  model_table = data.frame("p(M)" = prior_model_probs,
                           "p(M | D)" = post_model_probs,
                           "prior odds" = prior_model_odds,
                           "post odds" = post_model_odds,
                           "BF" = bfs
                           )
  row.names(model_table) = model_list$formula_strings

  return(list(par_table = par_table, model_table = model_table))
}
