#' Find all the submodels (full model plus restricted models) given a regression model formula.
#' @param formula An object of class formula or brmsformula (or one that can be coerced to that classes): A symbolic description of the full model, i.e. the model including all terms being considered.
#' @returns A list containing the following elements: model_names = model formulas as strings, formulas = model formulas, included = parameters (terms) included in each model, omitted = parameters omitted in each model, random_effects = list of random effects (NULL from this function but can be added to), included_table = same info as "included" except in a data frame, n_models = number of models, n_terms = number of fixed effects, term_names = names of fixed effects. The models are listed in each component of the output in the same order, with the full model given first.
#' @details This function ignores random effects; appropriate random effects for submodels can be obtained using the "add_random_effects" function. GIVE FURTHER DETAILS**
#'
submodels = function(formula){
  # Parse the model formula
  model_parts = parse_formula(formula)
  n_terms = length(model_parts$fixed)
  all_fixed = model_parts$fixed

  # Obtain all combinations of model terms/effects
  combo_list = list()
  for(m in n_terms:1){
    combo_list = c(combo_list,
                   combn(all_fixed, m = m, simplify = FALSE))
  }

  # Remove combinations where there is an interaction without all relevant main effects
  to_remove = c() # list of combos to remove
  for(i in 1:length(combo_list)){ # loop through combos
    is_interaction = grepl("\\:", combo_list[[i]])
    if(any(is_interaction)){
      interactions = combo_list[[i]][is_interaction] # interactions in current combo
      mains = combo_list[[i]][!is_interaction] # main effects in current combo
      n_interactions = length(interactions)
      for(j in 1:n_interactions){ # loop through interactions in the current combo
        needed_mains = strsplit(interactions[j], "\\s*\\:\\s*")[[1]] # main effects that should be in the model
        has_needed_mains = all(needed_mains %in% mains)
        if(!has_needed_mains){ # check if has all needed main effects
          to_remove = c(to_remove, i) # mark the combo to be removed
        }
      }
    }
  }
  if(length(to_remove) > 0){
    combo_list = combo_list[-to_remove] # remove combinations if without all needed main effects
  }

  # Construct formulas for restricted models
  model_names = c(); formula_list = list()
  for(i in 1:length(combo_list)){
    rhs = paste(combo_list[[i]], collapse = " + ")
    model_names[i] = paste(c(model_parts$lhs, rhs), collapse = " ~ ")
    formula_list[[i]] = formula(model_names[i])
  }
  # Add intercept-only model
  model_names[length(model_names) + 1] = paste(c(model_parts$lhs, "1"), collapse = " ~ ")
  n_models = length(model_names) # count the number of models
  formula_list[[n_models]] = formula(model_names[n_models])
  combo_list[n_models] = list(NULL)

  # Make a list of which terms from the full model are omitted in each restricted model
  omitted = list()
  omitted[[1]] = c()
  for(i in 2:length(combo_list)){
    omitted[[i]] = setdiff(all_fixed, combo_list[[i]])
  }

  # Make a table indicating whether or not each term is present in each model
  included_table = data.frame()
  for(i in 1:n_models){
    for(j in 1:n_terms){
      term = all_fixed[j]
      included_table[i, term] = term %in% combo_list[[i]]
    }
  }
  row.names(included_table) = model_names

  # Package results
  included = combo_list
  result = list(model_names = model_names,
                formulas = formula_list,
                included = combo_list,
                omitted = omitted,
                random_effects = NULL,
                included_table = included_table,
                n_models = n_models,
                n_terms = n_terms,
                term_names = all_fixed
                )
  return(result)
}
