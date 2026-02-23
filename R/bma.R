#' Fit nested regression submodels and compute Bayes factors (for Bayesian Model Averaging: BMA).
#'
#' @param full_model An object representing the full model, i.e. the model that includes all possible terms.
#' @param prior_main_probs Optional: a named vector giving the prior inclusion probabilities for main effects. By default (if NULL) these are set to 0.5.
#' @param prior_intr_condprobs Optional: a named vector giving the prior inclusion conditional probabilities for interactions (given that the necessary main effects/lower order interactions are present. By default (if NULL) these are set to 0.5 if there are interactions, are to NULL if there are no interactions.
#' @returns An object of class "bma".
#' @details
#' Fully Bayesian estimation is supported using "breg" (from this package).
#' Maximum likelihood estimates are supported (with the BIC approximation), so long as they have the following methods:
#' \itemize{
#'  \item update
#'  \item vcov
#'  \item coef
#'  \item predict
#'  \item loglik
#'  \item nobs
#' }
#' ** CHECK AND ADD Additional details...
#'
#' @export
#'
bma = function(full_model,
               prior_main_probs = NULL,
               prior_intr_condprobs = NULL
){
  # get info about submodels (full model plus restricted models)
  model_info = submodels(formula(full_model))

  # use default prior probs if not manually specified
  if(is.null(prior_main_probs)){ # default
    prior_main_probs = rep(0.5, times = model_info$n_main)
    names(prior_main_probs) = model_info$main_names
  }
  if(is.null(prior_intr_condprobs)){
    if(is.null(model_info$intr_names)){ # default if there are no interactions
      prior_intr_condprobs = NULL
    }
    else{ # default if there are interactions
      prior_intr_condprobs = rep(0.5, times = model_info$n_intr)
      names(prior_intr_condprobs) = model_info$intr_names
    }
  }

  # convert prior term inclusion probabilities to model probabilities
  prior_model_probs = probs_term_to_model(prior_main_probs, prior_intr_condprobs, model_info)

  # get marginal term inclusion probabilities
  if(model_info$n_intr == 0){ # no interactions
    prior_term_probs = prior_main_probs
  }
  else{ # have interactions -> get back from models
    prior_term_probs = probs_model_to_term(prior_model_probs, model_info)
  }

  # fit all submodels
  fit_list = fit_submodels(full_model, model_info)

  # compute Bayes factors and posterior model odds/probabilities
  log_evidence = model_log_evidence(fit_list)
  bfs = exp(log_evidence - lse(log_evidence))
  prior_model_odds = prior_model_probs/prior_model_probs[1]
  post_model_probs = exp(log_evidence + log(prior_model_probs) - lse(log_evidence + log(prior_model_probs)))
  post_model_odds = post_model_probs/post_model_probs[1]

  # convert model posterior probs to term posterior probs
  post_term_probs = probs_model_to_term(post_model_probs, model_info)

  # put results together
  output = list(prior_term_probs = prior_term_probs,
                post_term_probs = post_term_probs,
                prior_model_odds = prior_model_odds,
                prior_model_probs = prior_model_probs,
                post_model_odds = post_model_odds,
                post_model_probs = post_model_probs,
                log_model_evidence = log_evidence,
                model_bfs = bfs,
                fit_list = fit_list,
                model_class = class(full_model),
                model_info = model_info,
                data = model.frame(full_model)
                )
  output$is_factor = sapply(output$data, is.factor) | sapply(output$data, is.character)
  class(output) = "bma"

  return(output)
}
