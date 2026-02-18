#' Make contrast codes for Bayesian ANOVA.
#' @param a Number of factor levels (conditions).
#' @returns A matrix with contrast codes.
#' @details Special care needs to be taken with Bayesian ANOVA to make sure that all factor level means (and pairwise mean differences) have equal prior distributions. R's default coding scheme (treatment coding) does not accomplish this: priors will not be equal. This function implements a normalized forward Helmert contrast, which will produce equal priors across factor levels as described in the paper below (note that they don't explicitly say that they're describing normalized forward Helmert contrasts in that paper, but that is what they're doing).
#' Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M. (2012). Default Bayes factors for ANOVA designs. Journal of Mathematical Psychology, 56(5), 356–374. https://doi.org/10.1016/j.jmp.2012.08.001
#'
#' Note that the contrasts have been adapted to facilitate putting the prior on pair-wise mean differences, as in bayestestR::contr.equalprior_pairs.
#'
contr_banova = function(a){
  contr = matrix(0.0, nrow = a, ncol = a - 1)
  for(cl in 1:(a-1)){
    # forward Helmert contrast
    n = a - cl + 1
    contr[cl, cl] = (n-1)/n
    contr[(cl+1):a, cl] = -1/n
    # normalize
    contr[,cl] = contr[,cl]/sqrt(sum(contr[,cl]^2))
  }
  # divide by sqrt(2) as in bayestestR::contr.equalprior_pairs
  contr = contr/sqrt(2)
  return(contr)
}
