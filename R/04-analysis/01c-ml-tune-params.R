# Notes ----------------------------------------------------------------------------------
#   Goal:   Tune model for predicting birthweight
#   Time:   ~ 45 hours w/ grid of 5 mtry [3,15], 2 trees [50,100], 2 min-n [10^3, 10^2]


# Data notes -----------------------------------------------------------------------------


# To-do list -----------------------------------------------------------------------------
#   - Add option to drop imputation flags.


# Run scripts: Prep data and set up models -----------------------------------------------
  # Run the preceding scripts
  source(here::here('R', '04-analysis', 'did', '01b-ml-setup-models.R'))


# RF: Tune with CV -----------------------------------------------------------------------
  # Define CV assignment
  set.seed(123)
  cv_def = vfold_cv(natality_train, v = 5)
  # Tune on the grid 
  rf_cv =
    rf_wf %>% 
    tune_grid(
      cv_def,
      grid = rf_grid,
      metrics = metric_set(
        rmse, rsq
      )
      # control = control_grid(save_pred = TRUE)
    )
  # Save result
  qs::qsave(
    x = rf_cv,
    file = here(
      'data-clean', 'prediction', 'tuning',
      'rf-cv-grid-train80-noindicators-2.qs'
    ),
    preset = 'fast'
  )


# Cleaning -------------------------------------------------------------------------------
  # Garbage control
  invisible(gc())