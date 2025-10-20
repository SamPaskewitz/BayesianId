parse_formula = function(fmla){
  # Deal with brms formulas
  if("brmsformula" %in% class(fmla)){
    fmla = fmla$formula
  }

  # Get the complete left hand side (split around "~", then take part 1)
  lhs = strsplit(as.character(fmla), "\\s*\\~\\s*")[[2]]

  # Split the right hand side into terms (split around "+")
  rhs_terms = attr(terms(fmla), which = "term.labels")

  # Split the right hand side terms into fixed and random (identify random terms by "|")
  is_random = grepl("\\|", rhs_terms)
  if(any(is_random)){ # there are random terms
    fixed = rhs_terms[!is_random]
    random = paste0("(", rhs_terms[is_random], ")")
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
              random = random,
              fixed_main = fixed_main,
              fixed_interaction = fixed_interaction)
  )
}
