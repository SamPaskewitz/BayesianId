#' Compute Bayes factors for submodels using the Savage-Dickey Density Ratio.
#' @param full_model A brmsfit object (fitted regression model) representing the full model, i.e. the model that includes all possible terms.
#' @param model_list A list of submodels, returned by using the function "submodels" on "formula(full_model)".
#' @details For now, this only works for fixed effects models. FINISH
#'
model_bfs_sdr = function(full_model, model_list){
  # initialize Bayes factors
  bfs = rep(1.0, times = model_list$n_models)
  names(bfs) = model_list$model_names

  # get prior scale for fixed effects
  if("sigma" %in% fit$prior$class){
    sigma_draws = as_draws_df(full_model)$sigma
    prior_scale = outer(sigma_draws,
                        full_model$stanvars$prior_adjust$sdata*full_model$stanvars$r$sdata)
  }
  else{
    prior_scale = outer(rep(1.0, times = length(full_model$stanvars$prior_adjust$sdata)),
                        full_model$stanvars$prior_adjust$sdata*full_model$stanvars$r$sdata)
  }
  colnames(prior_scale) = paste0("b_", colnames(prior_scale))

  # get beta draws (without the intercept) and rescale them
  beta_draws = full_model |> as_draws_df() |> dplyr::select(starts_with("b_") & !contains("Intercept"))
  scaled_beta_draws = beta_draws/prior_scale

  # compute Bayes factors for restricted models
  for(i in 2:model_list$n_models){
    # setup
    omitted = paste0("b_", model_list$omitted[[i]])
    n_omitted = length(omitted)
    # log prior for Savage-Dickey
    log_prior = n_omitted*dcauchy(0, location = 0, scale = 1, log = TRUE)
    # log posterior for Savage-Dickey
    if(n_omitted > 1){
      kernel_est = kdevine::kdevine(scaled_beta_draws[, omitted] |> as.matrix())
      log_post = kdevine::dkdevine(rep(0.0, times = n_omitted), kernel_est) |> log() |> sum()
    } else{
      kernel_est = kdevine::kde1d(scaled_beta_draws[, omitted] |> as.vector())
      log_post = kdevine::dkde1d(0.0, kernel_est) |> log()
    }
    # use Savage-Dickey ratio to compute the Bayes factor
    bfs[i] = exp(log_post - log_prior)
  }

  return(bfs)
}
