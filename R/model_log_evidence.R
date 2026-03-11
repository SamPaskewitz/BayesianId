#' Compute log model evidence (marginal likelihood) for a list of models.
#' @param model_list A list of fitted regression models, either breg objects or MLE fits from "lm", "glm", "lmer" etc.
#' @returns A vector of log evidence.
#' @details If the fitted models are brmsfit objects, the log model evidence is computed using bridge sampling. If not, it is assummed that they were fitted by MLE, and log model evidence is computed using the BIC approximation.

model_log_evidence = function(fit_list){
  n_models = length(fit_list)

  # compute log model evidence (log marginal likelihoods)
  log_evidence = rep(0.0, times = n_models)
  if("breg" %in% class(fit_list[[1]])){
    log_evidence = lapply(fit_list, function(x){x$log_evidence}) |> unlist()
  } else{
    # otherwise use the BIC approximation for MLE
    log_evidence = -0.5*(lapply(fit_list, BIC) |> unlist())
  }

  return(log_evidence)
}
