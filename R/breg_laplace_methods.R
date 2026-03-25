#' Compute posterior credible intervals for model parameters (intercept plus fixed effects).
#' @param obj A "breg_laplace" object (fitted model).
#' @param prob Probability of the interval (e.g. 0.9 for a 90\% interval).
#' @returns Posterior credible intervals.
#' @aliases posterior_interval
#' @importFrom rstantools posterior_interval
#' @export posterior_interval
#' @export
posterior_interval.breg_laplace = function(obj, prob = 0.9){
  alpha = 1 - prob
  intervals = data.frame(lower = qnorm(p = alpha/2, mean = obj$post_mean, sd = obj$post_sd),
                         upper = qnorm(p = 1 - alpha/2, mean = obj$post_mean, sd = obj$post_sd)
                         ) |> as.matrix()
  colnames(intervals) = paste(100*c(alpha/2, 1 - alpha/2), "%")
  return(intervals)
}
