# Notes ----------------------------------------------------------------------------------
#   Goal:   Predict birthweight
#   Time:   ~ 64 minutes


# Data notes -----------------------------------------------------------------------------


# Other ----------------------------------------------------------------------------------
#   Helpful for manual CV:  https://rsample.tidymodels.org/reference/manual_rset.html

# To-do list -----------------------------------------------------------------------------
#   - Create a 'tune' option
#   - Could do something with 'natality_test'


# Run scripts: Prep data; set up and tune models -----------------------------------------
  # Run the preceding scripts
  # source(here::here('R', '04-analysis', 'did', '01c-ml-tune-params.R'))
  source(here::here('R', '04-analysis', 'did', '01b-ml-setup-models.R'))


# Load tuning data -----------------------------------------------------------------------
  # RF: Load CV hyperparameter grid
  rf_cv = here(
    'data-clean', 'prediction', 'tuning',
    'rf-cv-grid-train80-noindicators-0.qs'
  ) %>% qs::qread()
# NOTE Increasing trees to 200 did not improve performance (see *-noindicators-1.qs)


# # RF: Train selected model on all pre-period data ----------------------------------------
#   # Define the RF recipe for all the pre-period data
#   rf_recipe_final = 
#     recipe( 
#       dbwt ~ .,
#       data = natality_pre
#     ) %>% # Update role of year and county (both types) to ID
#     update_role(
#       year, 
#       fips_occ, fips_res, row,
#       new_role = 'id variable'
#     ) %>% # Drop imputation indicators
#     update_role(
#       matches('_na[0-9]?$'),
#       new_role = 'id variable'
#     )
#   # Finalize the workflow with the 'best' (minimum RMSE) model
#   rf_wf_final = finalize_workflow(
#     x = workflow() %>% add_model(rf_model) %>% add_recipe(rf_recipe_final),
#     parameters = select_best(rf_cv, metric = 'rmse')
#   )
#   # Train the model on the 'pre' data (<=1995)
#   rf_final = rf_wf_final %>% fit(natality_pre)
#   invisible(gc())
#   # Predict onto the whole dataset
#   rf_pred = rf_final %>% predict(new_data = natality_dt)
#   # Add predictions to natality dataset
#   natality_dt[, dbwt_pred := rf_pred$.pred]
#   # Save
#   write_fst(
#     x = natality_dt,
#     path = here(
#       'data-clean',
#       'natality-micro-rf-train80-noindicators-0-full.fst'
#     ),
#     compress = 100
#   )


# RF: Train selected model using CV in pre-period ----------------------------------------
  # Define custom CV assignment: 5 folds for pre data; post data is always validation
  set.seed(456)
  # First sample pre-period observations into 5 folds
  sampled_folds = sample(
    x = rep(1:5, length.out = natality_pre[,.N]),
    size = natality_pre[,.N],
    replace = FALSE
  ) |> as.integer()
  # Combine the two datasets
  natality_full = rbindlist(list(natality_pre, natality_post), use.names = TRUE, fill = TRUE)
  rm(natality_pre, natality_post, natality_split, natality_test, natality_train, natality_dt)
  invisible(gc())
  # Build the rsample object
  indices = list(
    list(
      analysis = setdiff(1:natality_full[,.N], which(sampled_folds == 1)),
      assessment = which(sampled_folds == 1)
    ),
    list(
      analysis = setdiff(1:natality_full[,.N], which(sampled_folds == 2)),
      assessment = which(sampled_folds == 2)
    ),
    list(
      analysis = setdiff(1:natality_full[,.N], which(sampled_folds == 3)),
      assessment = which(sampled_folds == 3)
    ),
    list(
      analysis = setdiff(1:natality_full[,.N], which(sampled_folds == 4)),
      assessment = which(sampled_folds == 4)
    ),
    list(
      analysis = setdiff(1:natality_full[,.N], which(sampled_folds == 5)),
      assessment = which(sampled_folds == 5)
    )
  )
  # Make the splits
  splits = lapply(indices, make_splits, data = natality_full)
  # Define the RF recipe for all the pre-period data
  rf_recipe_final = 
    recipe( 
      dbwt ~ .,
      data = natality_full
    ) %>% # Update role of year and county (both types) to ID
    update_role(
      year, 
      fips_occ, fips_res, row,
      new_role = 'id variable'
    ) %>% # Drop imputation indicators
    update_role(
      matches('_na[0-9]?$'),
      new_role = 'id variable'
    )
  # Finalize the workflow with the 'best' (minimum RMSE) model
  rf_wf_final = finalize_workflow(
    x = workflow() %>% add_model(rf_model) %>% add_recipe(rf_recipe_final),
    parameters = select_best(rf_cv, metric = 'rmse')
  )


tictoc::tic()
  # Iterating over splits: Generate out-of-sample predictions
  blah = lapply(
    X = seq_along(indices),
    FUN = function(i) {
      # Announce the fold
      cat('Fold ', i, ': ', Sys.time() |> as.character(), '\n')
      # Clean up
      invisible(gc())
      # Set up CV: Train on 4/5 of 'pre'; predict onto 1/5 of 'pre' and all 'post'
      cv_i = manual_rset(splits[i], paste0('Split ', 1))
      # Fit and predict
      rf_i = rf_wf_final %>% fit_resamples(
        natality_full,
        resamples = cv_i,
        control = control_resamples(save_pred = TRUE)
      )
      # Collect the predictions
      pred_i = rf_i |> collect_predictions()
      # Convert to data table
      setDT(pred_i)
      # Save
      write_fst(
        x = pred_i,
        path = here(
          'data-clean',
          paste0(
            'natality-micro-rf-train80-noindicators-0-full-cvpred-split',
            i,
            '.fst'
          )
        ),
        compress = 100
      )
      # Clean up
      rm(pred_i)
      invisible(gc())
      # Return 'success'
      return('success')
    }
  )
tictoc::toc()


  # Add predictions to natality dataset
  natality_dt[, dbwt_pred := rf_pred$.pred]
  # Save
  write_fst(
    x = natality_dt,
    path = here(
      'data-clean',
      'natality-micro-rf-train80-noindicators-0-full-cv.fst'
    ),
    compress = 100
  )

