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

#' Quantile function for a (univariate) mixture of normals.
#' @param p The probability for which we want to find the quantile.
#' @param pi A vector of mixture weights.
#' @param mu A vector of means.
#' @param sigma A vector of standard deviations.
#' @returns The quantile.
#'
qmix = function(p, pi, mu, sigma){
  q = optimize(function(x){(pmix(x, pi, mu, sigma) - p)^2},
               interval = range(qnorm(p, mu, sigma)))$minimum
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
  what_needed = combn(parts, ord - 1, simplify = FALSE) |> lapply(function(x){paste(x, collapse = ":")}) |> unlist()
  return(all(what_needed %in% terms))
}
