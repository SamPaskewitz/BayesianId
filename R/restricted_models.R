#' Find all the restricted models given a regression model formula.
#' @param formula An object of class formula or brmsformula (or one that can be coerced to that classes): A symbolic description of the full model, i.e. the model including all terms being considered.
#' @returns A list containing the following elements: formula_strings = model formulas as strings, formulas = model formulas, included = parameters (terms) included in each model, omitted = parameters omitted in each model, included_table = same info as "included" except in a data frame, n_models = number of models, n_fixed = number of fixed effects, fixed_names = names of fixed effects. The models are listed in each component of the output in the same order, with the full model given first.
#' @details GIVE DETAILS ABOUT THE PRINCIPLE OF MARGINALITY *
#'
restricted_models = function(formula){
  # Parse the model formula
  model_parts = parse_formula(formula)
  n_fixed = length(model_parts$fixed)
  all_fixed = model_parts$fixed

  # Obtain all combinations of model terms/effects
  combo_list = list()
  for(m in n_fixed:1){
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
  formula_strings = c(); formula_list = list()
  for(i in 1:length(combo_list)){
    rhs = paste(c(combo_list[[i]], model_parts$random), collapse = " + ")
    formula_strings[i] = paste(c(model_parts$lhs, rhs), collapse = " ~ ")
    formula_list[[i]] = formula(formula_strings[i])
  }
  # Add intercept-only model
  formula_strings[length(formula_strings) + 1] = paste(c(model_parts$lhs, "1"), collapse = " ~ ")
  formula_list[[length(formula_list) + 1]] = formula(formula_strings[i])
  n_models = length(formula_strings) # count the number of models
  combo_list[n_models] = list(NULL)

  # Make a list of which terms from the full model are omitted in each restricted model
  omitted = list()
  omitted[[1]] = c()
  for(i in 2:length(combo_list)){
    omitted[[i]] = setdiff(all_fixed, combo_list[[i]])
  }

  # Make a table indicating whether or not each fixed effect is present in each model
  included_table = data.frame()
  for(i in 1:n_models){
    for(j in 1:n_fixed){
      term = all_fixed[j]
      included_table[i, term] = term %in% combo_list[[i]]
    }
  }
  row.names(included_table) = formula_strings

  # Package results
  included = combo_list
  result = list(formula_strings = formula_strings,
                formulas = formula_list,
                included = combo_list,
                omitted = omitted,
                included_table = included_table,
                n_models = n_models,
                n_fixed = n_fixed,
                fixed_names = all_fixed
                )
  return(result)
}
