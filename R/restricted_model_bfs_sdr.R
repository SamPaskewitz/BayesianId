#' Use the Savage-Dickey Ratio with a brms model to compute Bayes factors for restricted models.
#'
#' @param brmsfit A fitted brms model.
#' @returns A table (data frame) of approximate Bayes factors.
#' @details ADD THESE

restricted_model_bfs_sdr = function(brmsfit){
  # set up empty table for model Bayes factors etc.
  rmodels = restricted_models(brms_model$formula)
  model_table = data.frame(BF = rep(1, times = rmodels$n_models))
  model_table[,'P(M)'] = rep(1/rmodels$n_models, times = rmodels$n_models)
  print(model_table)
  model_table[,'prior odds'] = model_table[,'P(M)']/model_table[1,'P(M)']
  row.names(model_table) = rmodels$formula_strings
  model_table[rmodels$formula_strings[1], "BF"] = 1.0

  # compute Bayes factors using the Savage-Dickey Ratio
  sbd = std_beta_draws(brms_model)
  robust_est = robust::covRob(sbd)
  mu = robust_est$center
  Sigma = robust_est$cov
  for(i in 2:rmodels$n_models){
    terms = paste0("b_", rmodels$omitted[[i]])
    n_terms = length(terms)
    log_post = mnormt::dmnorm(rep(0, n_terms),
                              mean = mu[terms],
                              varcov = Sigma[terms, terms],
                              log = TRUE)
    log_prior = n_terms*dcauchy(0, location = 0, scale = sqrt(2)/4, log = TRUE)
    model_table[rmodels$formula_strings[i], "BF"] = exp(log_post - log_prior)
  }

  # compute model posterior odds and probabilities
  model_table[,'post odds'] = model_table[,'prior odds']*model_table[,'BF']
  model_table[,'P(M|D)'] = model_table[,'post odds']/sum(model_table[,'post odds'])

  # compute prior and posterior probabilities for term inclusion
  incl_table = data.frame()
  for(i in 1:rmodels$n_fixed){
    term = rmodels$fixed_names[i]
    includes_term = rmodels$included_table[,term]
    incl_table[i, 'P(incl)'] = sum(model_table[includes_term, 'P(M)'])
    incl_table[i, 'P(incl|D)'] = sum(model_table[includes_term, 'P(M|D)'])
  }
  row.names(incl_table) = rmodels$fixed_names

  return(list(incl_table = incl_table,
              model_table = model_table))
}
