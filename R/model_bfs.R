#' Compute Bayes factors for a list of models.
#' @param model_list A list of fitted regression models, either brmsfit objects or MLE fits from "lm", "glm", "lmer" etc.
#' @returns A vector of Bayes factors.
#' @details The first model in the list is used as the denominator for comparisons, i.e. the Bayes factor for model i is defined as p(D | first model)/p(D | model i). If the fitted models are brmsfit objects, the Bayes factors are computed using bridge sampling. If not, it is assummed that they were fitted by MLE, and Bayes factors are computed using the BIC approximation.

model_bfs = function(fit_list){
  n_models = length(fit_list)

  # compute log model evidence (log marginal likelihoods)
  log_evidence = rep(0.0, times = n_models)
  if(class(fit_list[[1]]) == "brmsfit"){
    # use bridge sampling for MCMC
    for(i in 1:n_models){
      log_evidence[i] = brms::bridge_sampler(fit_list[[i]], silent = TRUE)$logml
    }
  } else{
    # otherwise use the BIC approximation for MLE
    log_evidence = -0.5*(lapply(fit_list, BIC) |> unlist())
  }

  # compute Bayes factors, with the first model in the denominator
  bfs = exp(log_evidence - log_evidence[1])

  return(bfs)
}
