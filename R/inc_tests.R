#' Conduct term inclusion tests. This is the most important function in the "BayesianId" package.
#'
#' @param full_model A brmsfit object (fitted regression model) representing the full model, i.e. the model that includes all possible terms.
#' @param prior_term_probs Optional: a named vector giving the prior inclusion probabilities for parameters (names = parameters/terms). By default (if NULL) these are set to 0.5, which corresponds to uniform prior probabilities across models DOUBLE CHECK *.
#' @param digits_to_round Number of digits to round results to.
#' @returns A list containing the following two elements: inc_table (term inclusion prior and posterior probabilities/odds, Bayes factors), model_table (model prior and posterior probabilities/odds, Bayes factors)
#'
inc_tests = function(full_model,
                     prior_term_probs = NULL,
                     digits_to_round = 2
                     ){
  # get a list of submodels (full model plus restricted models)
  model_list = submodels(full_model$formula)

  # by default, give models equal prior probability
  if(is.null(prior_term_probs)){
    prior_model_probs = rep(1/model_list$n_models, times = model_list$n_models)
    names(prior_model_probs) = model_list$model_names
    prior_term_probs = probs_model_to_term(prior_model_probs, model_list)
  }
  # otherwise, convert parameter prior probs to model prior probs
  else{
    # * NEED TO UPDATE THIS TO WORK WITH INTERACTIONS
    prior_model_probs = probs_term_to_model(prior_term_probs, model_list$included_table)
  }

  # fit all submodels
  fit_list = list()
  fit_list[[1]] = full_model
  for(i in 2:model_list$n_models){
    if(!is.null(model_list$included[[i]])){ # models with predictors
      fit_list[[i]] = update(full_model,
                             formula = model_list$formulas[[i]],
                             refresh = 0)
    }
    else{ # intercept-only model
      fit_list[[i]] = update(full_model,
                             formula = model_list$formulas[[i]],
                             stanvars = NULL, # remove stanvars for intercept-only model
                             refresh = 0)
    }

  }
  names(fit_list) = model_list$model_names

  # compute Bayes factors for all models
  bfs = model_bfs(fit_list)

  # compute model posterior probabilties
  prior_model_odds = prior_model_probs/prior_model_probs[1]
  post_model_odds = bfs*prior_model_odds
  post_model_probs = post_model_odds/sum(post_model_odds)

  # convert model posterior probs to parameter posterior probs
  post_term_probs = probs_model_to_term(post_model_probs, model_list)

  # make nice output tables for parameters and models
  inc_table = data.frame("p(β≠0)" = prior_term_probs,
                         "p(β≠0|D)" = post_term_probs,
                         "prior odds" = prior_term_probs/(1 - prior_term_probs),
                         "post odds" = post_term_probs/(1 - post_term_probs),
                         check.names = FALSE # prevent names from getting messed up
                         )
  inc_table[, "BF"] = inc_table[, "post odds"]/inc_table[, "prior odds"]
  inc_table = round(inc_table, digits = digits_to_round)
  row.names(inc_table) = model_list$term_names

  model_table = data.frame("p(M)" = prior_model_probs,
                           "p(M | D)" = post_model_probs,
                           "prior odds" = prior_model_odds,
                           "post odds" = post_model_odds,
                           "BF" = bfs,
                           check.names = FALSE # prevent names from getting messed up
                           )
  model_table = round(model_table, digits = digits_to_round)
  row.names(model_table) = model_list$model_names

  return(list(inc_table = inc_table, model_table = model_table, fit_list = fit_list))
}
