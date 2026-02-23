#' Log-sum-exp trick (stable computation of the log of a sum of exponentials).
#' @param a A vector of numbers.
#' @returns log-sum-exp of the vector a.
#' @details
#' The log-sum-exp function is defined as:
#' \deqn{\text{LSE}(\vec{a}) = \log\Big(\sum_j \exp(a_j)\Big)}
#' If the components of \eqn{\vec{a}} are large, then this will not be numerically stable.
#' To compute the log-sum-exp in a numerically stable way, we can use the equivalent expression:
#' \deqn{\text{LSE}(\vec{a}) = \max(\vec{a}) + \log\Big(\sum_j \exp(a_j - \max(\vec{a}))\Big)}
#' This is what the function below does.
#'
lse = function(a){
  m = max(a)
  return(m + log(sum(exp(a - m))))
}

#' Cumulative distribution function for a (univariate) mixture of normals.
#' @param q The quantile for which we want to find the probability.
#' @param pi A vector of mixture weights.
#' @param mu A vector of means.
#' @param sigma A vector of standard deviations.
#' @returns The probability.
#'
pmix = function(q, pi, mu, sigma){
  return(sum(pi*pnorm(q, mean = mu, sd = sigma)))
}

#' Probability density function for a (univariate) mixture of normals.
#' @param x The value for which we want to find the probability density.
#' @param pi A vector of mixture weights.
#' @param mu A vector of means.
#' @param sigma A vector of standard deviations.
#' @returns The probability density.
#'
dmix = function(x, pi, mu, sigma){
  if(length(x) == 1){
    return(sum(pi*dnorm(x, mean = mu, sd = sigma)))
  } else{
    fun = function(x){sum(pi*dnorm(x, mean = mu, sd = sigma))}
    return(sapply(x, fun))
  }
}

#' Quantile function for a (univariate) mixture of normals.
#' @param p The probability for which we want to find the quantile.
#' @param pi A vector of mixture weights.
#' @param mu A vector of means.
#' @param sigma A vector of standard deviations.
#' @returns The quantile.
#'
qmix = function(p, pi, mu, sigma){
  q = optimize(function(x){(pmix(x, pi, mu, sigma) - p)^2},
               interval = range(qnorm(p, mu, sigma))*c(0.75, 1.25))$minimum
  return(q)
}

#' Test whether terms are interactions are not.
#' @param terms A vector of model terms.
#' @returns A logical vector specifying whether each term is an interaction.
#'
is_interaction = function(terms){
  return(grepl("\\:", terms))
}

#' Test if a set of model terms (fixed effects) contains all the needed terms one order lower than an interaction.
#' @param terms A character vector of model terms.
#' @param intr The interaction, written in a form like "a:b" or "x1:x2:x3", etc.
#' @returns A logical value indicating whether or not the necessary terms are present.
#'
has_needed = function(terms, intr){
  parts = strsplit(intr, "\\:") |> unlist()
  ord = length(parts)
  what_needed = combn(parts, ord - 1, simplify = FALSE) |> sapply(function(x){paste(x, collapse = ":")})
  return(all(what_needed %in% terms))
}

#' Get the names of coefficients from a fitted model.
#' @param obj A fitted regression model.
#' @returns The names of the regression coefficients.
#'
get_coef_names = function(obj){
  if("brmsfit" %in% class(obj)){
    return(fixef(obj) |> row.names())
  } else{
    return(coef(obj) |> names())
  }
}
