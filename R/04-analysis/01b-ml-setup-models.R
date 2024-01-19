# Notes ----------------------------------------------------------------------------------
#   Goal:   Tune model for predicting birthweight
#   Time:   ~ 11 seconds in addition to running 01a


# Data notes -----------------------------------------------------------------------------


# To-do list -----------------------------------------------------------------------------
#   - Add option to drop imputation flags.


# Run scripts: Prep data -----------------------------------------------------------------
  # Run the script that loads and preps data
  source(here::here('R', '04-analysis', '01a-ml-prep-data.R'))


# RF: Set up recipe ----------------------------------------------------------------------
  # Define the recipe
  rf_recipe = 
    recipe( 
      dbwt ~ .,
      data = natality_train
    ) %>% # Update role of year and county (both types) to ID
    update_role(
      year, 
      fips_occ, fips_res, row,
      new_role = 'id variable'
    ) %>% # Drop imputation indicators
# ADJUST Toggle to include or drop indicator variables for imputed values
    update_role(
      matches('_na[0-9]?$'),
      new_role = 'id variable'
    )


# RF: Set up model and workflow ----------------------------------------------------------
  # Define the model
  rf_model = rand_forest(
    mtry = tune(),
    trees = tune(),
    min_n = tune()
  ) %>% set_mode(
    'regression'
  ) %>% set_engine(
    'ranger',
    # importance = 'impurity',
    num.threads = parallel::detectCores()
  )
  # Define the workflow
  rf_wf = workflow() %>% 
    add_model(rf_model) %>% 
    add_recipe(rf_recipe)
  # Find the number of predictor variables
  n_pred = rf_recipe %>%
    prep() %$%
    var_info %>%
    filter(role == 'predictor') %>% 
    nrow()


# RF: Set up tuning grid -----------------------------------------------------------------
  # Create grid to tune over
  # # Run 0:
  # rf_grid = grid_regular(
  #   mtry(range = c(3, 15)),
  #   trees(range = c(50, 100)),
  #   min_n(range = c(3, 2), trans = log10_trans()),
  #   levels = c(
  #     5,
  #     2,
  #     2
  #   )
  # )
  # # Run 1:
  # rf_grid = grid_regular(
  #   mtry(range = c(3, 12)),
  #   trees(range = c(100, 200)),
  #   min_n(range = c(4, 3), trans = log10_trans()),
  #   levels = c(
  #     4,
  #     2,
  #     2
  #   )
  # )
  # Run 2:
  rf_grid = grid_regular(
    mtry(range = c(3, 15)),
    trees(range = c(150, 150)),
    min_n(range = c(4, 2), trans = log10_trans()),
    levels = c(
      5,
      1,
      3
    )
  )


# Cleaning -------------------------------------------------------------------------------
  # Garbage control
  invisible(gc())