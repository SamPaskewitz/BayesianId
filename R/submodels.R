#' Find all the submodels (full model plus restricted models) given a regression model formula.
#' @param formula An object of class formula or brmsformula (or one that can be coerced to that classes): A symbolic description of the full model, i.e. the model including all terms being considered.
#' @returns A list containing the following elements:
#' model_names = model formulas as strings
#' formulas = model formulas
#' included = terms included in each model
#' omitted = terms omitted in each model
#' included_table = same info as "included" except in a data frame
#' n_models = number of models
#' n_terms = number of fixed effects
#' term_names = names of fixed effects. The models are listed in each component of the output in the same order, with the full model given first.
#' @details This function ignores random effects; appropriate random effects for submodels can be obtained using the "add_random_effects" function. GIVE FURTHER DETAILS**
#'
submodels = function(formula){
  # Parse the model formula
  model_parts = parse_formula(formula)
  n_terms = length(model_parts$fixed)
  all_fixed = model_parts$fixed
  all_random = model_parts$random

  # Obtain all combinations of model terms/effects
  combo_list = list()
  for(m in n_terms:1){
    combo_list = c(combo_list,
                   combn(all_fixed, m = m, simplify = FALSE))
  }

  # Only keep combinations that respect the principle of marginality
  included = Filter(test_marginality, combo_list)
  included[length(included) + 1] = list(NULL) # for intercept-only model
  n_models = length(included)

  # Get "included" plus intercept
  included_plus_intercept = included
  for(i in 1:n_models){
    included_plus_intercept[[i]] = c("1", included_plus_intercept[[i]])
  }

  # Add all random effects with a corresponding fixed effect
  if(is.null(all_random)){
    random_included = NULL
  } else{
    random_included = list()
    group = names(all_random)
    for(i in 1:n_models){
      rexpr = list()
      for(j in 1:length(group)){
        rterms = intersect(all_random[[j]], included_plus_intercept[[i]])
        rexpr[[j]] = paste0("(", paste(rterms, collapse = " + "), " | ", group[[j]], ")")
      }
      random_included[[i]] = paste(rexpr, collapse = " + ")
    }
  }

  # Construct formulas for submodels
  formula_list = list()
  for(i in 1:n_models){
    rhs_fixed = paste(included_plus_intercept[[i]], collapse = " + ")
    if(is.null(random_included)){
      rhs = rhs_fixed
    } else{
      rhs = paste(c(rhs_fixed, random_included[[i]]), collapse = " + ")
    }
    formula_list[[i]] = formula(paste(c(model_parts$lhs, rhs), collapse = " ~ "))
  }

  # Construct model names
  model_names = c()
  for(i in 1:(n_models-1)){
    model_names[i] = paste("~", paste(included[[i]], collapse = " + "))
  }
  model_names[n_models] = "~ 1"

  # Make a list of which terms from the full model are omitted in each restricted model
  omitted = list()
  omitted[[1]] = c()
  for(i in 2:length(included)){
    omitted[[i]] = setdiff(all_fixed, included[[i]])
  }

  # Make a table indicating whether or not each term is present in each model
  included_table = data.frame()
  for(i in 1:n_models){
    for(j in 1:n_terms){
      term = all_fixed[j]
      included_table[i, term] = term %in% included[[i]]
    }
  }
  row.names(included_table) = model_names

  # Package results
  result = list(model_names = model_names,
                formulas = formula_list,
                included = included,
                omitted = omitted,
                included_table = included_table,
                n_models = n_models,
                n_terms = n_terms,
                term_names = all_fixed
                )
  return(result)
}
