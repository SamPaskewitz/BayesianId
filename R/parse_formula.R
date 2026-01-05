#' Parse a regression model formula into its component parts.
#' @param formula An object of class formula or brmsformula (or one that can be coerced to that classes): A symbolic description of the model to be fitted. The details of model specification are explained in brmsformula.
#' @returns A list containing the following elements: lhs = left hand side, fixed = fixed effects, random = random effects, fixed_main = fixed effects that are not interactions, fixed_interaction = fixed effects that are interactions.
#'
parse_formula = function(formula){
  # Deal with brms formulas
  if("brmsformula" %in% class(formula)){
    formula = formula$formula
  }

  # Get the complete left hand side (split around "~", then take part 1)
  lhs = strsplit(as.character(formula), "\\s*\\~\\s*")[[2]]

  # Split the right hand side into terms
  rhs_terms = attr(terms(formula), which = "term.labels")

  # Split the right hand side terms into fixed and random (identify random terms by "|")
  is_random = grepl("\\|", rhs_terms)
  if(any(is_random)){ # there are random terms
    fixed = rhs_terms[!is_random]
    random = list()
    brm_parse = brms::brmsterms(formula)
    group = brm_parse$dpars$mu$re$group
    form = brm_parse$dpars$mu$re$form
    for(i in 1:length(group)){
      random[[group[i]]] = c("1", attr(terms(form[[i]]), which = "term.labels"))
    }
  }
  else{ # there are no random terms (all terms are fixed)
    fixed = rhs_terms
    random = NULL
  }

  # Identify interaction terms in the fixed effects
  is_interaction = grepl("\\:", fixed)
  if(any(is_interaction)){ # interactions
    fixed_main = fixed[!is_interaction]
    fixed_interaction = fixed[is_interaction]
  }
  else{ # no interactions
    fixed_main = fixed
    fixed_interaction = NULL
  }

  # Package everything into a list
  return(list(lhs = lhs,
              fixed = fixed,
              n_fixed = length(fixed),
              random = random,
              fixed_main = fixed_main,
              fixed_interaction = fixed_interaction)
  )
}
