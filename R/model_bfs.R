#' Compute Bayes factors for a list of models.
#' @param model_list A list of brmsfit objects, i.e. fitted regression models.
#' @returns A vector of Bayes factors.
#' @details The first model in the list is used as the denominator for comparisons, i.e. the Bayes factor for model i is defined as p(D | first model)/p(D | model i). Currently the Bayes factors are computed using bridge sampling. I may change this or add other methods in the future.

model_bfs = function(fit_list){
  n_models = length(fit_list)

  # compute log model evidence (log marginal likelihoods)
  log_evidence = rep(0.0, times = n_models)
  for(i in 1:n_models){
    log_evidence[[i]] = brms::bridge_sampler(fit_list[[i]], silent = TRUE)$logml
  }

  bfs = rep(1.0, times = n_models)
  # the first BF is always 1, so we start the loop from 2
  for(i in 2:n_models){
    bfs[i] = exp(log_evidence[[i]] - log_evidence[[1]])
  }

  return(bfs)
}
