# Training ML model: LASSO version 
library(pacman)
p_load(
  here, fst, data.table, tidymodels, ranger, qs, vip, rpart.plot,
  lubridate, glmnet
)

# Prepping data  --------------------------------------------------------------
  # First loading the raw data
  sample_watershed_dt =
    read.fst(
      path = here('data-clean/ml-water/sample-watershed-dt.fst'),
      as.data.table = TRUE
    )[,-c('site_no','quarter','region')]#,'sample_date'
  # Adding sample month formatted as a date
  sample_watershed_dt[,
    sample_month := ymd(paste(year(sample_date), month(sample_date), '01'))
  ]
  # Splitting into testing and training
  # Order by date
  setorder(sample_watershed_dt, sample_date)
  # Indices of training and testing: Test on 1st full year of data
  i_train = sample_watershed_dt[sample_date >= ymd('2015-10-01'), which = T]
  i_test = sample_watershed_dt[sample_date < ymd('2015-10-01'), which = T]
  # Create the split
  data_split = make_splits(
    list(analysis = i_train, assessment = i_test),
    data = sample_watershed_dt
  )
  # Grab training and testing subsets
  training_dt = data_split |> training() |> copy()
  testing_dt = data_split |> testing() |> copy()

# Cross validation ------------------------------------------------------------
  # Range of penalties to test over
  lambdas = 10^seq(from = 1, to = -3, length = 1e3)
  # Setting up CV 
  cv_splits = vfold_cv(training_dt, v = 5)
  # Manually create rolling splits in training dataset
    initial = 12*1.25 # Months in analysis 
    assess = 6 # Months in assessment
    # Ordering backwards since we are predicting historially
    training_dt |> setorder(-sample_date) 
    data_dates = training_dt$sample_month %>% unique()
    # Function to make the splits 
    make_rolling_splits = function(i){
      # Get time periods for analysis and assessment
      analysis_dates = seq(
        data_dates[i + initial - 1], 
        data_dates[i], 
        by = "month"
      )
      assess_dates = seq(
        data_dates[i + initial + assess - 1], 
        data_dates[i + initial],
        by = "month"
      )
      # Indices of analysis and assessment
      i_analysis = training_dt[sample_month %in% analysis_dates, which = T]
      i_assess = training_dt[sample_month %in% assess_dates, which = T]
      # Create the split
      rolling_split = make_splits(
        list(analysis = i_analysis, assessment = i_assess),
        data = training_dt
      )
      return(rolling_split)
    }
    # Indices to pass to the make_rolling_split function
    split_i = 1:(length(data_dates) - initial - assess + 1)
    # Create list of the splits for each time period
    rolling_splits = map(
      split_i,
      make_rolling_splits
    )
    # Combine the splits into an rset object
    rolling_cv =
      manual_rset(
        splits = rolling_splits,
        ids = paste0("rolling_split",split_i)
      )
    # Clean up
    rm(rolling_splits, initial, assess, data_dates,i_test,i_train)

  # Now more complicated versions for linear models 
  linear_recipe_base = 
    recipe(training_dt) |>
    update_role(
      contains('pred_glyphosate_awt'),
      #contains('all_yield_diff_percentile_gmo_d'),
      #contains('percentile_gm_acres'),
      contains('percentile_corn_acres'),
      contains('percentile_soy_acres'),
      contains('percentile_cotton_acres'),
      contains('percentile_other_acres'),
      contains('ppt'),
      #contains('pct_irrigated'),
      contains('kffact'),
      contains('slopelen'),
      new_role = 'predictor'
    ) |>
    step_dummy(
      month, 
      role = 'predictor'
    ) 
  linear_recipe_interactions = 
    linear_recipe_base |>
    # Two way interactions (3)
    step_interact(terms = ~kffact_d0:pred_glyphosate_awt_d0)|>
    step_interact(terms = ~kffact_d50:pred_glyphosate_awt_d50)|> 
    step_interact(terms = ~kffact_d100:pred_glyphosate_awt_d100)|>
    step_interact(terms = ~kffact_d150:pred_glyphosate_awt_d150)|>
    step_interact(terms = ~kffact_d200:pred_glyphosate_awt_d200)|>
    step_interact(terms = ~kffact_d250:pred_glyphosate_awt_d250)|>
    step_interact(terms = ~kffact_d300:pred_glyphosate_awt_d300)|>
    step_interact(terms = ~slopelen_d0:pred_glyphosate_awt_d0)|>
    step_interact(terms = ~slopelen_d50:pred_glyphosate_awt_d50)|>
    step_interact(terms = ~slopelen_d100:pred_glyphosate_awt_d100)|>
    step_interact(terms = ~slopelen_d150:pred_glyphosate_awt_d150)|>
    step_interact(terms = ~slopelen_d200:pred_glyphosate_awt_d200)|>
    step_interact(terms = ~slopelen_d250:pred_glyphosate_awt_d250)|>
    step_interact(terms = ~slopelen_d300:pred_glyphosate_awt_d300)|>
    step_interact(terms = ~ppt_growing_season_d0:pred_glyphosate_awt_d0)|>
    step_interact(terms = ~ppt_growing_season_d50:pred_glyphosate_awt_d50)|>
    step_interact(terms = ~ppt_growing_season_d100:pred_glyphosate_awt_d100)|>
    step_interact(terms = ~ppt_growing_season_d150:pred_glyphosate_awt_d150)|>
    step_interact(terms = ~ppt_growing_season_d200:pred_glyphosate_awt_d200)|>
    step_interact(terms = ~ppt_growing_season_d250:pred_glyphosate_awt_d250)|>
    step_interact(terms = ~ppt_growing_season_d300:pred_glyphosate_awt_d300)|>
    # Three way interactions SxK, SxP, KxP
    step_interact(terms = ~slopelen_d0:kffact_d0:pred_glyphosate_awt_d0)|> 
    step_interact(terms = ~slopelen_d50:kffact_d50:pred_glyphosate_awt_d50)|> 
    step_interact(terms = ~slopelen_d100:kffact_d100:pred_glyphosate_awt_d100)|> 
    step_interact(terms = ~slopelen_d150:kffact_d150:pred_glyphosate_awt_d150)|> 
    step_interact(terms = ~slopelen_d200:kffact_d200:pred_glyphosate_awt_d200)|> 
    step_interact(terms = ~slopelen_d250:kffact_d250:pred_glyphosate_awt_d250)|> 
    step_interact(terms = ~slopelen_d300:kffact_d300:pred_glyphosate_awt_d300)|> 
    step_interact(terms = ~ppt_growing_season_d0:slopelen_d0:pred_glyphosate_awt_d0)|> 
    step_interact(terms = ~ppt_growing_season_d50:slopelen_d50:pred_glyphosate_awt_d50)|> 
    step_interact(terms = ~ppt_growing_season_d100:slopelen_d100:pred_glyphosate_awt_d100)|> 
    step_interact(terms = ~ppt_growing_season_d150:slopelen_d150:pred_glyphosate_awt_d150)|> 
    step_interact(terms = ~ppt_growing_season_d200:slopelen_d200:pred_glyphosate_awt_d200)|> 
    step_interact(terms = ~ppt_growing_season_d250:slopelen_d250:pred_glyphosate_awt_d250)|> 
    step_interact(terms = ~ppt_growing_season_d300:slopelen_d300:pred_glyphosate_awt_d300)|>
    step_interact(terms = ~ppt_growing_season_d0:kffact_d0:pred_glyphosate_awt_d0)|> 
    step_interact(terms = ~ppt_growing_season_d50:kffact_d50:pred_glyphosate_awt_d50)|> 
    step_interact(terms = ~ppt_growing_season_d100:kffact_d100:pred_glyphosate_awt_d100)|> 
    step_interact(terms = ~ppt_growing_season_d150:kffact_d150:pred_glyphosate_awt_d150)|> 
    step_interact(terms = ~ppt_growing_season_d200:kffact_d200:pred_glyphosate_awt_d200)|> 
    step_interact(terms = ~ppt_growing_season_d250:kffact_d250:pred_glyphosate_awt_d250)|> 
    step_interact(terms = ~ppt_growing_season_d300:kffact_d300:pred_glyphosate_awt_d300)|>
    # Four way interaction
    step_interact(terms = ~ppt_growing_season_d0:slopelen_d0:kffact_d0:pred_glyphosate_awt_d0)|>
    step_interact(terms = ~ppt_growing_season_d50:slopelen_d50:kffact_d50:pred_glyphosate_awt_d50)|>
    step_interact(terms = ~ppt_growing_season_d100:slopelen_d100:kffact_d100:pred_glyphosate_awt_d100)|>
    step_interact(terms = ~ppt_growing_season_d150:slopelen_d150:kffact_d150:pred_glyphosate_awt_d150)|>
    step_interact(terms = ~ppt_growing_season_d200:slopelen_d200:kffact_d200:pred_glyphosate_awt_d200)|>
    step_interact(terms = ~ppt_growing_season_d250:slopelen_d250:kffact_d250:pred_glyphosate_awt_d250)|>
    step_interact(terms = ~ppt_growing_season_d300:slopelen_d300:kffact_d300:pred_glyphosate_awt_d300)
    
    glyph_recipe_linear_simple = 
      linear_recipe_base |>
      update_role(
        gly_result, 
        new_role = 'outcome'
      )
    glyph_recipe_linear_interactions = 
      linear_recipe_interactions |>
      update_role(
        gly_result, 
        new_role = 'outcome'
      )
    ampa_recipe_linear_simple = 
      linear_recipe_base |>
      update_role(
        ampa_result, 
        new_role = 'outcome'
      )
    ampa_recipe_linear_interactions = 
      linear_recipe_interactions |>
      update_role(
        ampa_result, 
        new_role = 'outcome'
      )
  # Define lasso model 
  model_lasso = 
    linear_reg(
      penalty = tune(),
      mixture = 1
    ) |>
    set_engine("glmnet")
  # Create workflows
  wf_glyph_lasso_simple = 
    workflow() |>
    add_model(model_lasso) |>
    add_recipe(glyph_recipe_linear_simple)
  wf_glyph_lasso_interactions = 
    workflow() |>
    add_model(model_lasso) |>
    add_recipe(glyph_recipe_linear_interactions)
  wf_ampa_lasso_simple = 
    workflow() |>
    add_model(model_lasso) |>
    add_recipe(ampa_recipe_linear_simple)
  wf_ampa_lasso_interactions = 
    workflow() |>
    add_model(model_lasso) |>
    add_recipe(ampa_recipe_linear_interactions)
  # Doing cross validation
  cv_glyph_lasso_simple = 
    tune_grid(
      object = wf_glyph_lasso_simple,
      resamples = rolling_cv,
      grid = data.frame(penalty = lambdas),
      metrics = metric_set(rmse, rsq),
      control = control_grid(save_pred = TRUE)
    )
  cv_glyph_lasso_interactions = 
    tune_grid(
      object = wf_glyph_lasso_interactions,
      resamples = rolling_cv,
      grid = data.frame(penalty = lambdas),
      metrics = metric_set(rmse, rsq),
      control = control_grid(save_pred = TRUE)
    )
  cv_ampa_lasso_simple = 
    tune_grid(
      object = wf_ampa_lasso_simple,
      resamples = rolling_cv,
      grid = data.frame(penalty = lambdas),
      metrics = metric_set(rmse, rsq),
      control = control_grid(save_pred = TRUE)
    )
  cv_ampa_lasso_interactions = 
    tune_grid(
      object = wf_ampa_lasso_interactions,
      resamples = rolling_cv,
      grid = data.frame(penalty = lambdas),
      metrics = metric_set(rmse, rsq),
      control = control_grid(save_pred = TRUE)
    )
    # Checking the results 
    autoplot(cv_glyph_lasso_simple, metric = 'rmse') +
      labs(y = 'RMSE', title = 'Glyphosate, no interactions') + 
      theme_minimal() + 
      scale_color_viridis_d()
    autoplot(cv_glyph_lasso_interactions, metric = 'rmse') +
      labs(y = 'RMSE', title = 'Glyphosate, with interactions') + 
      theme_minimal() + 
      scale_color_viridis_d()
    autoplot(cv_ampa_lasso_simple, metric = 'rmse') +
      labs(y = 'RMSE', title = 'AMPA, no interactions') + 
      theme_minimal() + 
      scale_color_viridis_d()
    autoplot(cv_ampa_lasso_interactions, metric = 'rmse') +
      labs(y = 'RMSE', title = 'AMPA, with interactions') + 
      theme_minimal() + 
      scale_color_viridis_d()
# Checking results against test data
  # First for simple model (no interactions)
  best_glyph_lasso_simple = select_best(
    cv_glyph_lasso_simple, 
    metric = 'rmse'
  )
  final_glyph_wf_lasso_simple = 
    wf_glyph_lasso_simple |> 
    finalize_workflow(best_glyph_lasso_simple)
  test_glyph_lasso_simple = 
    final_glyph_wf_lasso_simple |> 
    last_fit(data_split)
  # Now for interacted model
  best_glyph_lasso_interactions = select_best(
    cv_glyph_lasso_interactions, 
    metric = 'rmse'
  )
  final_glyph_wf_lasso_interactions = 
    wf_glyph_lasso_interactions |> 
    finalize_workflow(best_glyph_lasso_interactions)
  test_glyph_lasso_interactions = 
    final_glyph_wf_lasso_interactions |> 
    last_fit(data_split)
  # Checking fit 
  collect_metrics(test_glyph_lasso_simple)
  collect_metrics(test_glyph_lasso_interactions)
  extract_fit_parsnip(test_glyph_lasso_simple) |>
    tidy() |> 
    print(n = 10000)
# Fitting test AMPA model
  # First the simple model 
  best_ampa_lasso_simple = 
    select_best(
      cv_ampa_lasso_simple, 
      metric = 'rmse'
    )
  final_ampa_wf_lasso_simple = 
    wf_ampa_lasso_simple |> 
    finalize_workflow(best_ampa_lasso_simple)
  test_ampa_lasso_simple = 
    final_ampa_wf_lasso_simple |> 
    last_fit(data_split)
  # Now the interactions
  best_ampa_lasso_interactions = 
    select_best(
      cv_ampa_lasso_interactions, 
      metric = 'rmse'
    )
  final_ampa_wf_lasso_interactions = 
    wf_ampa_lasso_interactions |> 
    finalize_workflow(best_ampa_lasso_interactions)
  test_ampa_lasso_interactions = 
    final_ampa_wf_lasso_interactions |> 
    last_fit(data_split)
  # Checking performance 
  collect_metrics(test_ampa_lasso_simple)
  collect_metrics(test_ampa_lasso_interactions)
  extract_fit_parsnip(test_ampa_lasso_interactions) |>
    tidy() |> 
    print(n = 1000)


# Finally, fitting on all data ------------------------------------------------
ampa_fit_lasso = 
    workflow() |>
    add_model(
        linear_reg(
        penalty = best_ampa_lasso_interactions$penalty[1],
        mixture = 1
        ) |>
        set_engine("glmnet")
    ) |>
    add_recipe(ampa_recipe_linear_interactions) |>
    fit(data = sample_watershed_dt)
glyph_fit_lasso = 
    workflow() |>
    add_model(
        linear_reg(
        penalty = best_glyph_lasso_interactions$penalty[1],
        mixture = 1
        ) |>
        set_engine("glmnet")
    ) |>
    add_recipe(glyph_recipe_linear_interactions) |>
    fit(data = sample_watershed_dt)
# Saving the results 
qsave(
  ampa_fit_lasso, 
  here('data-clean/ml-water/ampa-lasso-final.qs')
)
qsave(
  glyph_fit_lasso, 
  here('data-clean/ml-water/glyph-lasso-final.qs')
)
