model_bfs = function(){
  # FINISH

  # compile the model
  compiled_model = cmdstanr::cmdstan_model(cmdstanr::write_stan_file(stan_code))

  # find MAP (fit the model by optimization)
  optim_result = compiled_model$optimize(data = stan_data,
                                         seed = seed,
                                         sig_figs = 14,
                                         refresh = 0,
                                         algorithm = "lbfgs",
                                         tol_obj = 1e-12,
                                         tol_rel_obj = 10000,
                                         tol_grad = 1e-08,
                                         tol_rel_grad = 1e+07,
                                         tol_param = 1e-08,
                                         show_messages = FALSE,
                                         jacobian = TRUE # do MAP fit instead of MLE
  )
}
