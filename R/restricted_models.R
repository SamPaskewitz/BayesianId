restricted_models = function(fmla){
  # Parse the model formula
  model_parts = parse_formula(fmla)
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
          to_remove = c(to_remove, i) # mark the combo to be remove
        }
      }
    }
  }
  if(length(to_remove) > 0){
    combo_list = combo_list[-to_remove] # remove combinations if without all needed main effects
  }
  combo_list[[length(combo_list) + 1]] = c("1") # add intercept-only model

  # Construct formulas for restricted models
  formula_strings = c(); formula_list = list()
  for(i in 1:length(combo_list)){
    rhs = paste(c(combo_list[[i]], model_parts$random), collapse = " + ")
    formula_strings[i] = paste(c(model_parts$lhs, rhs), collapse = " ~ ")
    formula_list[[i]] = formula(formula_strings[i])
  }

  # Make a list of which terms from the full model are omitted in each restricted model
  omitted = list()
  omitted[[1]] = NULL
  for(i in 2:length(combo_list)){
    omitted[[i]] = setdiff(all_fixed, combo_list[[i]])
  }

  # Package results
  result = list(formula_strings = formula_strings,
                formulas = formula_list,
                included = combo_list,
                omitted = omitted)
  return(result)
}
