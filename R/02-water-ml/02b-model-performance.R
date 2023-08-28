# Out-of-sample prediction performance for LASSO and RF models  
library(pacman)
p_load(
  here, fst, data.table, tidymodels, ranger, qs, vip, rpart.plot,
  lubridate, glmnet
)


# Checking performance -----------------------------------------
  # RMSE 
  
  # Variable Importance 
  
  # Make predictions 

  # Plot results for glyphosate 

  # Plot results for AMPA 



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
  # Creating recipes 
  glyph_recipe = 
    recipe(training_dt) |>
    update_role(
      gly_result, 
      new_role = 'outcome'
    ) |>
    update_role(
      contains('glyphosate_nat_100km'),
      contains('yield_diff'),
      contains('corn_acres'),
      contains('soy_acres'),
      contains('cotton_acres'),
      contains('other_acres'),
      contains('ppt'),
      #contains('pct_irrigated'),
      contains('kffact'),
      contains('slopelen'),
      month, 
      new_role = 'predictor'
    ) |>
    update_role(
      hybas_id, year, 
      new_role = 'id variable'
    )
  # AMPA recipe
  ampa_recipe = 
    recipe(training_dt) |>
    update_role(
      ampa_result, 
      new_role = 'outcome'
    ) |>
    update_role(
      contains('glyphosate_nat_100km'),
      contains('yield_diff'),
      contains('corn_acres'),
      contains('soy_acres'),
      contains('cotton_acres'),
      contains('other_acres'),
      #contains('ppt'),
      #contains('pct_irrigated'),
      contains('kffact'),
      contains('slopelen'),
      month, 
      new_role = 'predictor'
    ) |>
    update_role(
      hybas_id, year, 
      new_role = 'id variable'
    )
  

# Defining models -------------------------------------------------------------
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
  
  # Define workflows
  wf_glyph_rf = 
    workflow() |>
    add_model(model_rf) |>
    add_recipe(glyph_recipe)
  wf_ampa_rf = 
    workflow() |>
    add_model(model_rf) |>
    add_recipe(ampa_recipe)

  
# Cross validation ------------------------------------------------------------
  # Find the number of predictor variables
  n_glyph_pred = glyph_recipe %>%
    prep() %$%
    var_info %>%
    filter(role == 'predictor') %>% 
    nrow()
  n_ampa_pred = ampa_recipe %>%
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
    # rolling_cv_folds = 
    #   rbind(
    #     map_dfr(
    #       1:length(split_i),
    #       ~rolling_cv$splits[[.]] %>% 
    #         assessment() %>% 
    #         .[,.(mindate = min(sample_date), maxdate = max(sample_date))]
    #       )  %>% 
    #       mutate(
    #         set = "Assessment",
    #         fold = 1:length(split_i)
    #       ),
    #     map_dfr(
    #       1:length(split_i),
    #       ~rolling_cv$splits[[.]] %>% 
    #         analysis() %>% 
    #           .[,.(mindate = min(sample_date), maxdate = max(sample_date))]
    #       ) %>% 
    #       mutate(
    #         set = "Analysis",
    #         fold = 1:length(split_i)
    #       )
    #   )
    # # Plot
    # rolling_cv_folds_plot = 
    #   rolling_cv_folds %>% 
    #   ggplot(aes(x = fold, color = set)) +
    #   geom_linerange(aes(ymin = mindate, ymax = maxdate), size = 2) +
    #   scale_x_continuous(
    #     breaks = 1:length(split_i),
    #     labels = 1:length(split_i),
    #     minor_breaks = NULL,
    #     name = "Fold"
    #   ) +
    #   scale_y_date(breaks = "year", labels = date_format("%b %Y"))+
    #   scale_color_discrete(name = "")+
    #   coord_flip() +
    #   theme_minimal()

  # ---------------------------------------------------------------------------  
  cv_glyph_rf =
    tune_grid(
      object = wf_glyph_rf,
      resamples = rolling_cv,
      grid = rf_glyph_grid,
      metrics = metric_set(rmse, rsq),
      control = control_grid(save_pred = TRUE)
    )
  qsave(
    cv_glyph_rf, 
    file = here('data-clean/ml-water/cv-glyph-ydiff-rf.qs')
  )
  cv_ampa_rf =
    tune_grid(
      object = wf_ampa_rf,
      resamples = rolling_cv,
      grid = rf_ampa_grid,
      metrics = metric_set(rmse, rsq),
      control = control_grid(save_pred = TRUE)
    )
  qsave(
    cv_ampa_rf, 
    file = here('data-clean/ml-water/cv-ampa-ydiff-rf.qs')
  )


  # These models are trained with glyphosate (potentially endogenous)
  cv_glyph_rf_G = qread(file = here('data-clean/ml-water/cv-glyph-rf.qs'))
  cv_ampa_rf_G = qread(file = here('data-clean/ml-water/cv-ampa-rf.qs'))
  # These models are trained on instruments (plausibly exogenous)
  cv_glyph_rf = qread(file = here('data-clean/ml-water/cv-glyph-ydiff-rf.qs'))
  cv_ampa_rf = qread(file = here('data-clean/ml-water/cv-ampa-ydiff-rf.qs'))
  # Checking the results 
  ggsave(
    filename = here('figures/water-ml/cv-glyph.jpeg'),
    autoplot(cv_glyph_rf, metric = 'rmse') +
      labs(y = 'RMSE') + 
      theme_minimal() + 
      scale_color_viridis_d(),
    width =  7, height = 5,
    bg = 'white'
  )
  ggsave(
    filename = here('figures/water-ml/cv-ampa.jpeg'),
    autoplot(cv_ampa_rf, metric = 'rmse') +
      labs(y = 'RMSE') + 
      theme_minimal() + 
      scale_color_viridis_d(),
    width =  7, height = 5,
    bg = 'white'
  )


# Checking performance vs test set --------------------------------------------
  # Choosing the best 
  best_glyph_rf = select_best(cv_glyph_rf, metric = 'rmse')
  best_ampa_rf = select_best(cv_ampa_rf, metric = 'rmse')
  final_glyph_wf = wf_glyph_rf |> finalize_workflow(best_glyph_rf)
  final_ampa_wf = wf_ampa_rf |> finalize_workflow(best_ampa_rf)
  
  # Fitting on entire training set 
  test_glyph_fit = final_glyph_wf |> last_fit(data_split)
  test_ampa_fit = final_ampa_wf |> last_fit(data_split)
  
  # Checking the results
  collect_metrics(test_glyph_fit)
  collect_metrics(test_ampa_fit)
  ggsave(
    filename = here('figures/water-ml/vip-glyph.jpeg'),
    test_glyph_fit |>
      extract_fit_parsnip() |>
      vip(num_features = 50) + 
      theme_minimal(),
    width = 5, height = 12,
    bg ='white'
  )
  ggsave(
    filename = here('figures/water-ml/vip-ampa.jpeg'),
    test_ampa_fit |>
      extract_fit_parsnip() |>
      vip(num_features = 50) + 
      theme_minimal(),
    width = 5, height = 12,
    bg ='white'
  )
  # Saving the results 
  qsave(
    x = test_glyph_fit,
    file = here('data-clean/ml-water/test-glyph-fit-ydiff-rf.qs')
  )
  qsave(
    x = test_ampa_fit,
    file = here('data-clean/ml-water/test-ampa-fit-ydiff-rf.qs')
  )