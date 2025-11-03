#' Get standardized regression coefficient draws from a brms model.
#'
#' @param brms_model A fitted brms model (with a sigma parameter).
#' @returns Draws of standardized betas (= beta/sigma), ignoring the intercept.

std_beta_draws = function(brms_model){
  beta_draws = brms_model %>% as_draws_df() %>% select(starts_with("b_") & !contains("Intercept"))
  sigma_draws = brms_model %>% as_draws_df() %>% select(starts_with("sigma"))
  std_beta_draws = beta_draws
  for(i in 1:ncol(std_beta_draws)){
    std_beta_draws[,i] = std_beta_draws[,i]/sigma_draws$sigma
  }
  return(std_beta_draws)
}
