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
  # Source settings
  source(here::here('R', '04-analysis', '01-settings.R'))
  # Run the preceding scripts
  # source(here::here('R', '04-analysis', '01c-ml-tune-params.R'))
  source(here::here('R', '04-analysis', '01b-ml-setup-models.R'))


# Load tuning data -----------------------------------------------------------------------
  # RF: Load CV hyperparameter grid
  rf_cv = here(
    'data', 'clean', 'prediction', 'tuning',
    paste0(outcome_var, '-', 'rf-cv-grid-train80-noindicators-2.qs')
  ) %>% qs::qread()


# Set up metric --------------------------------------------------------------------------
  # Determine metric set based on outcome
  if (outcome_var %in% c('i_lbw', 'i_vlbw', 'i_preterm')) {
    the_metrics = metric_set(
      accuracy, f_meas
    )
  } else {
    the_metrics = metric_set(
      rmse, rsq
    )
  }
  # Find the "primary" metric
  primary_metric = attr(the_metrics, 'metrics') |> names() |> head(1)


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
  sampled_folds =
    sample(
      x = rep(1:5, length.out = natality_pre[, .N]),
      size = natality_pre[, .N],
      replace = FALSE
    ) |>
    as.integer()
  # Combine the two datasets
  natality_full = rbindlist(
    list(natality_pre, natality_post),
    use.names = TRUE,
    fill = TRUE
  )
  rm(natality_pre, natality_post, natality_split, natality_test, natality_train)
  rm(natality_dt)
  invisible(gc())
  # Add a new row ID to merge with predictions
  natality_full[, .row := seq_len(.N)]
  # Find indices of post years (1996 and beyond)
  indices_post = natality_full[year >= 1996, which = TRUE]
  invisible(gc())
  # Build the rsample object
  indices = list(
    list(
      analysis = setdiff(
        x = 1:natality_full[, .N],
        y = c(which(sampled_folds == 1), indices_post)
      ),
      assessment = c(which(sampled_folds == 1), indices_post)
    ),
    list(
      analysis = setdiff(
        x = 1:natality_full[, .N],
        y = c(which(sampled_folds == 2), indices_post)
      ),
      assessment = c(which(sampled_folds == 2), indices_post)
    ),
    list(
      analysis = setdiff(
        x = 1:natality_full[, .N],
        y = c(which(sampled_folds == 3), indices_post)
      ),
      assessment = c(which(sampled_folds == 3), indices_post)
    ),
    list(
      analysis = setdiff(
        x = 1:natality_full[, .N],
        y = c(which(sampled_folds == 4), indices_post)
      ),
      assessment = c(which(sampled_folds == 4), indices_post)
    ),
    list(
      analysis = setdiff(
        x = 1:natality_full[, .N],
        y = c(which(sampled_folds == 5), indices_post)
      ),
      assessment = c(which(sampled_folds == 5), indices_post)
    )
  )
  rm(indices_post)
  # Make the splits
  splits = lapply(indices, make_splits, data = natality_full)
  # Define the RF recipe for all the pre-period data
  rf_recipe_final =
    recipe(
      paste0(outcome_var, ' ~ .') |> as.formula(),
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
  # Finalize the workflow with the 'best' (minimum RMSE or highest accuracy) model
  rf_wf_final = finalize_workflow(
    x = workflow() %>% add_model(rf_model) %>% add_recipe(rf_recipe_final),
    parameters = select_best(rf_cv, metric = primary_metric)
  )

  # Iterating over splits: Generate out-of-sample predictions
  blah = lapply(
    # X = seq_along(indices),
    X = 2:5,
    FUN = function(i) {
      # Announce the fold
      cat('Fold ', i, ': ', Sys.time() |> as.character(), '\n')
      # Clean up
      invisible(gc())
      # Set up CV: Train on 4/5 of 'pre'; predict onto 1/5 of 'pre' and all 'post'
      cv_i = manual_rset(splits[i], paste0('Split ', 1))
      # Fit the model on the training folds (2604 seconds)
      rf_i = rf_wf_final |> fit(data = natality_full[splits[[i]]$in_id])
      invisible(gc())
      # Predict
      pred_i = predict(
        object = rf_i,
        new_data = natality_full[splits[[i]]$out_id],
        type = 'prob'
      )
      rm(rf_i)
      # Convert to data table
      setDT(pred_i)
      # Add desired features
      pred_i[, `:=`(
        split = i,
        .pred_prob = .pred_1,
        .pred = as.integer(.pred_1 > .5),
        .row = splits[[i]]$out_id
      )]
      # Drop class probabilities
      pred_i[, c('.pred_0', '.pred_1') := NULL]
# NOTE Commented-out lines are hit memory issues.
      # # Fit and predict
      # rf_i = rf_wf_final %>% fit_resamples(
      #   natality_full,
      #   resamples = cv_i,
      #   control = control_resamples(save_pred = TRUE)
      # )
      # # Collect the predictions
      # pred_i = rf_i |> collect_predictions()
      # # Convert to data table
      # setDT(pred_i)
      # Save
      write_fst(
        x = pred_i,
        path = here(
          'data', 'clean', 'prediction', 'output',
          paste0(
            outcome_var,
            '-natality-micro-rf-train80-noindicators-2-full-cvpred-split',
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
  rf_pred =
    mclapply(
      X = here(
        'data', 'clean', 'prediction', 'output'
      ) |> dir(
        pattern = paste0(outcome_var, '.*noindicators-2.*split[0-9]\\.fst'),
        full.names = TRUE
      ),
      FUN = read_fst,
      as.data.table = TRUE,
      mc.cores = 5
    ) |>
    rbindlist(use.names = TRUE, fill = TRUE)
  # Clean up
  invisible(gc())
  # Collapse to row (we have 5 predictions per post-period row)
  rf_final = collap(
    X = rf_pred,
    by = ~ .row,
    FUN = fmean,
    cols = c(outcome_var, '.pred'),
    sort = TRUE
  )
  # Key both datasets using row
  setkey(natality_full, .row)
  setkey(rf_final, .row)
  # Add predictions to natality dataset (ordering should match; row "id" will not)
  set(
    x = natality_full,
    j = paste0(outcome_var, '_pred'),
    value = rf_final$.pred
  )
  # Add the 'true' value to check the match
  set(
    x = natality_full,
    j = paste0(outcome_var, '_check'),
    value = rf_final[[outcome_var]]
  )
  set(
    x = natality_full,
    j = paste0('.row_check_', outcome_var),
    value = rf_final$.row
  )
  # Checks
  qtab(natality_full[[outcome_var]] == natality_full[[paste0(outcome_var, '_check')]])
  qtab(natality_full[['.row']] == natality_full[[paste0('.row_check_', outcome_var)]])
  # Save
  write_fst(
    x = natality_full,
    path = here(
      'data', 'clean', 'prediction', 'output',
      paste0(
        outcome_var,
        '-natality-micro-rf-train80-noindicators-2-full-cvpred.fst'
      )
    ),
    compress = 100
  )
