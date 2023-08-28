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
  # Manually create rolling splits in training dataset
  initial = 15 # Months in analysis 
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

# Creating the recipes --------------------------------------------------------
  linear_recipe_base = 
    recipe(training_dt) |>
    update_role(
      contains('pred_glyphosate_awt'),
      contains('percentile_corn_acres'),
      contains('percentile_soy_acres'),
      contains('percentile_cotton_acres'),
      contains('percentile_other_acres'),
      contains('ppt'),
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
  # Now the actual recipes
  glyph_recipe_base = 
    linear_recipe_base |>
    update_role(
      gly_result, 
      new_role = 'outcome'
    )
  ampa_recipe_base = 
    linear_recipe_base |>
    update_role(
      ampa_result, 
      new_role = 'outcome'
    )
  # With interactions
  glyph_recipe_linear_interactions = 
    linear_recipe_interactions |>
    update_role(
      gly_result, 
      new_role = 'outcome'
    )
  ampa_recipe_linear_interactions = 
    linear_recipe_interactions |>
    update_role(
      ampa_result, 
      new_role = 'outcome'
    )
  
# Setting up models and workflows ---------------------------------------------  
  # Define lasso model 
  model_lasso = 
    linear_reg(
      penalty = tune(),
      mixture = 1
    ) |>
    set_engine("glmnet")
  # Define RF model
  model_rf = 
    rand_forest(
      mtry = tune(),
      trees = 200,
      min_n = tune()
    ) |>
    set_mode("regression") |>
    set_engine(
      "ranger", 
      importance = "impurity"
    )
  # Create workflows
  wf_glyph_lasso = 
    workflow() |>
    add_model(model_lasso) |>
    add_recipe(glyph_recipe_linear_interactions)
  wf_ampa_lasso = 
    workflow() |>
    add_model(model_lasso) |>
    add_recipe(ampa_recipe_linear_interactions)
  wf_glyph_rf = 
    workflow() |>
    add_model(model_rf) |>
    add_recipe(glyph_recipe_base)
  wf_ampa_rf = 
    workflow() |>
    add_model(model_rf) |>
    add_recipe(ampa_recipe_base)

# Runnning cross validation ---------------------------------------------------
  # Picking parameters to tune over 
  # Range of penalties in LASSO model
  lambdas = 10^seq(from = 1, to = -3, length = 1e3)
  # Find the number of predictor variables
  n_glyph_pred = glyph_recipe_base %>%
    prep() %$%
    var_info %>%
    filter(role == 'predictor') %>% 
    nrow()
  n_ampa_pred = ampa_recipe_base %>%
    prep() %$%
    var_info %>%
    filter(role == 'predictor') %>% 
    nrow()
  # Create grid to tune over
  rf_glyph_grid = grid_regular(
    mtry(range = c(2, n_glyph_pred)),
    min_n(range = c(1, 50)),
    levels = c(round(n_glyph_pred/3, 0),10)
  )
  rf_ampa_grid = grid_regular(
    mtry(range = c(2, n_ampa_pred)),
    min_n(range = c(1, 50)),
    levels = c(round(n_ampa_pred/3, 0),10)
  )
  # Running LASSO Cross validation
  cv_glyph_lasso = 
    tune_grid(
      object = wf_glyph_lasso,
      resamples = rolling_cv,
      grid = data.frame(penalty = lambdas),
      metrics = metric_set(rmse, rsq),
      control = control_grid(save_pred = TRUE)
    )
  cv_ampa_lasso = 
    tune_grid(
      object = wf_ampa_lasso,
      resamples = rolling_cv,
      grid = data.frame(penalty = lambdas),
      metrics = metric_set(rmse, rsq),
      control = control_grid(save_pred = TRUE)
    )
  # Running Random Forest Cross validation
  cv_glyph_rf =
    tune_grid(
      object = wf_glyph_rf,
      resamples = rolling_cv,
      grid = rf_glyph_grid,
      metrics = metric_set(rmse, rsq),
      control = control_grid(save_pred = TRUE)
    )
  cv_ampa_rf =
    tune_grid(
      object = wf_ampa_rf,
      resamples = rolling_cv,
      grid = rf_ampa_grid,
      metrics = metric_set(rmse, rsq),
      control = control_grid(save_pred = TRUE)
    )  
  # Checking the results 
  autoplot(cv_glyph_rf, metric = 'rmse') +
    labs(y = 'RMSE', title = 'Glyphosate, random forest') + 
    theme_minimal() + 
    scale_color_viridis_d()
  autoplot(cv_glyph_lasso, metric = 'rmse') +
    labs(y = 'RMSE', title = 'Glyphosate, LASSO') + 
    theme_minimal() + 
    scale_color_viridis_d()
  autoplot(cv_ampa_rf, metric = 'rmse') +
    labs(y = 'RMSE', title = 'AMPA, random forest') + 
    theme_minimal() + 
    scale_color_viridis_d()
  autoplot(cv_ampa_lasso, metric = 'rmse') +
    labs(y = 'RMSE', title = 'AMPA, LASSO') + 
    theme_minimal() + 
    scale_color_viridis_d()
  # Choosing best parameters
  best_glyph_rf = select_best(cv_glyph_rf, metric = 'rmse')
  best_ampa_rf = select_best(cv_ampa_rf, metric = 'rmse')
  best_glyph_lasso = select_best(cv_glyph_lasso, metric = 'rmse')
  best_ampa_lasso = select_best(cv_ampa_lasso, metric = 'rmse')
  # Creating final workflow
  final_glyph_wf_rf = wf_glyph_rf |> finalize_workflow(best_glyph_rf)
  final_ampa_wf_rf = wf_ampa_rf |> finalize_workflow(best_ampa_rf)
  final_glyph_wf_lasso = wf_glyph_lasso |> finalize_workflow(best_glyph_lasso)
  final_ampa_wf_lasso = wf_ampa_lasso |> finalize_workflow(best_ampa_lasso)
  # Fitting on entire training set 
  test_glyph_rf = final_glyph_wf_rf |> last_fit(data_split)
  test_ampa_rf = final_ampa_wf_rf |> last_fit(data_split)
  test_glyph_lasso = final_glyph_wf_lasso |> last_fit(data_split)
  test_ampa_lasso = final_ampa_wf_lasso |> last_fit(data_split)
  # Saving the test fit   
  qsave(
    x = test_glyph_rf,
    file = here('data-clean/ml-water/test-glyph-fit-rf.qs')
  )
  qsave(
    x = test_ampa_rf,
    file = here('data-clean/ml-water/test-ampa-fit-rf.qs')
  )
  qsave(
    x = test_glyph_lasso,
    file = here('data-clean/ml-water/test-glyph-fit-lasso.qs')
  )
  qsave(
    x = test_ampa_lasso,
    file = here('data-clean/ml-water/test-ampa-fit-lasso.qs')
  )

# Finally, fitting on all data ------------------------------------------------
  # LASSO Models 
  ampa_fit_lasso = 
    workflow() |>
    add_model(
      linear_reg(
        penalty = best_ampa_lasso$penalty[1],
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
        penalty = best_glyph_lasso$penalty[1],
        mixture = 1
      ) |>
      set_engine("glmnet")
    ) |>
    add_recipe(glyph_recipe_linear_interactions) |>
    fit(data = sample_watershed_dt)
  # Random forest models 
  ampa_fit_rf = 
    workflow() |>
    add_model(
      rand_forest(
        mtry = best_ampa_rf$mtry[1],
        trees = 200,
        min_n = best_ampa_rf$min_n[1]
      ) |>
      set_mode("regression") |>
      set_engine(
        "ranger", 
        importance = "impurity"
      )
    ) |>
    add_recipe(ampa_recipe_base) |>
    fit(data = sample_watershed_dt)
  glyph_fit_rf = 
    workflow() |>
    add_model(
      rand_forest(
        mtry = best_glyph_rf$mtry[1],
        trees = 200,
        min_n = best_glyph_rf$min_n[1]
      ) |>
      set_mode("regression") |>
      set_engine(
        "ranger", 
        importance = "impurity"
      )
    ) |>
    add_recipe(glyph_recipe_base) |>
    fit(data = sample_watershed_dt)
  # Saving the results 
  qsave(
    ampa_fit_lasso, 
    here('data-clean/ml-water/final-fit-lasso-ampa.qs')
  )
  qsave(
    glyph_fit_lasso, 
    here('data-clean/ml-water/final-fit-lasso-glyph.qs')
  )
  qsave(
    ampa_fit_rf, 
    here('data-clean/ml-water/final-fit-rf-ampa.qs')
  )
  qsave(
    glyph_fit_rf, 
    here('data-clean/ml-water/final-fit-rf-glyph.qs')
  )
