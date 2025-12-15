#' Conduct term inclusion tests. This is the most important function in the "BayesianId" package.
#'
#' @param full_model A brmsfit object (fitted regression model) representing the full model, i.e. the model that includes all possible terms.
#' @param prior_main_probs Optional: a named vector giving the prior inclusion probabilities for main effects. By default (if NULL) these are set to 0.5.
#' @param prior_intr_condprobs Optional: a named vector giving the prior inclusion conditional probabilities for interactions (given that the necessary main effects/lower order interactions are present. By default (if NULL) these are set to 0.5 if there are interactions, are to NULL if there are no interactions.
#' @param digits_to_round Number of digits to round results to.
#' @returns A list containing the following:
#' inc_table (term inclusion prior and posterior probabilities/odds, Bayes factors)
#' model_table (model prior and posterior probabilities/odds, Bayes factors)
#' fit_list (list of fitted submodels)
#' @export
#'
inc_tests = function(full_model,
                     prior_main_probs = NULL,
                     prior_intr_condprobs = NULL,
                     digits_to_round = 3
                     ){
  # get a list of submodels (full model plus restricted models)
  model_list = submodels(formula(full_model))

  # use default prior probs if not manually specified
  if(is.null(prior_main_probs)){ # default
    prior_main_probs = rep(0.5, times = model_list$n_main)
    names(prior_main_probs) = model_list$main_names
  }
  if(is.null(prior_intr_condprobs)){
    if(is.null(model_list$intr_names)){ # default if there are no interactions
      prior_intr_condprobs = NULL
    }
    else{ # default if there are interactions
      prior_intr_condprobs = rep(0.5, times = model_list$n_intr)
      names(prior_intr_condprobs) = model_list$intr_names
    }
  }

  # convert prior term inclusion probabilities to model probabilities
  prior_model_probs = probs_term_to_model(prior_main_probs, prior_intr_condprobs, model_list)

  # get marginal term inclusion probabilities
  if(model_list$n_intr == 0){ # no interactions
    prior_term_probs = prior_main_probs
  }
  else{ # have interactions -> get back from models
    prior_term_probs = probs_model_to_term(prior_model_probs, model_list)
  }

  # fit all submodels
  fit_list = fit_submodels(full_model, model_list)

  # compute Bayes factors for all models
  bfs = model_bfs(fit_list)

  # compute model posterior probabilties
  prior_model_odds = prior_model_probs/prior_model_probs[1]
  post_model_odds = bfs*prior_model_odds
  post_model_probs = post_model_odds/sum(post_model_odds)

  # convert model posterior probs to term posterior probs
  post_term_probs = probs_model_to_term(post_model_probs, model_list)

  # make nice output tables for term and models
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
