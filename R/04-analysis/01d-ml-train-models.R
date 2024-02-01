# Notes ----------------------------------------------------------------------------------
#   Goal:   Predict birthweight
#   Time:   ~ 1 hour per fold.


# Data notes -----------------------------------------------------------------------------


# Other ----------------------------------------------------------------------------------
#   Helpful for manual CV:  https://rsample.tidymodels.org/reference/manual_rset.html

# To-do list -----------------------------------------------------------------------------
#   - Create a 'tune' option
#   - Could do something with 'natality_test'


# Run scripts: Prep data; set up and tune models -----------------------------------------
  # Run the preceding scripts
  # source(here::here('R', '04-analysis', '01c-ml-tune-params.R'))
  source(here::here('R', '04-analysis', '01b-ml-setup-models.R'))


# Load tuning data -----------------------------------------------------------------------
  # RF: Load CV hyperparameter grid
  rf_cv = here(
    'data', 'clean', 'prediction', 'tuning',
    'rf-cv-grid-train80-noindicators-2.qs'
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
#       'data', 'clean',
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
  rm(natality_pre, natality_post, natality_split, natality_test, natality_train)
  rm(natality_dt)
  invisible(gc())
  # Add a new row ID to merge with predictions
  natality_full[, .row := 1:.N]
  # Find indices of post years (1996 and beyond)
  indices_post = natality_full[year >= 1996, which = TRUE]
  # Build the rsample object
  indices = list(
    list(
      analysis = setdiff(
        x = 1:natality_full[,.N],
        y = c(which(sampled_folds == 1), indices_post)
      ),
      assessment = c(which(sampled_folds == 1), indices_post)
    ),
    list(
      analysis = setdiff(
        x = 1:natality_full[,.N],
        y = c(which(sampled_folds == 2), indices_post)
      ),
      assessment = c(which(sampled_folds == 2), indices_post)
    ),
    list(
      analysis = setdiff(
        x = 1:natality_full[,.N],
        y = c(which(sampled_folds == 3), indices_post)
      ),
      assessment = c(which(sampled_folds == 3), indices_post)
    ),
    list(
      analysis = setdiff(
        x = 1:natality_full[,.N],
        y = c(which(sampled_folds == 4), indices_post)
      ),
      assessment = c(which(sampled_folds == 4), indices_post)
    ),
    list(
      analysis = setdiff(
        x = 1:natality_full[,.N],
        y = c(which(sampled_folds == 5), indices_post)
      ),
      assessment = c(which(sampled_folds == 5), indices_post)
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
          'data', 'clean', 'prediction', 'output',
          paste0(
            'natality-micro-rf-train80-noindicators-2-full-cvpred-split',
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

  # Load the splits' predictions
  rf_pred = mclapply(
    X = here(
      'data', 'clean', 'prediction', 'output'
    ) |> dir(
      pattern = 'noindicators-2.*split[0-9]\\.fst',
      full.names = TRUE
    ),
    FUN = read_fst,
    as.data.table = TRUE,
    mc.cores = 5
  ) |> rbindlist(use.names = TRUE, fill = TRUE)
  # Clean up
  invisible(gc())
  # Collapse to row (we have 5 predictions per post-period row)
  rf_final = collap(
    X = rf_pred,
    by = ~ .row,
    FUN = fmean,
    cols = c('dbwt', '.pred'),
    sort = TRUE
  )
  # Key both datasets using row
  setkey(natality_full, .row)
  # Add predictions to natality dataset (ordering should match; row "id" will not)
  natality_full[, dbwt_pred := rf_final$.pred]
  natality_full[, dbwt_check := rf_final$dbwt]
  natality_full[, .row_check := rf_final$.row]
  # Checks
  natality_full[dbwt_check != dbwt, .N]
  natality_full[.row_check != .row, .N]
  # Save
  write_fst(
    x = natality_full,
    path = here(
      'data', 'clean', 'prediction', 'output',
      'natality-micro-rf-train80-noindicators-2-full-cvpred.fst'
    ),
    compress = 100
  )

