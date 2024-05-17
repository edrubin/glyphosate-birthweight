# Out-of-sample prediction performance for LASSO and RF models  
library(pacman)
p_load(
  here, fst, data.table, tidymodels, ranger, qs, vip, rpart.plot,
  lubridate, glmnet
)

# Loading data ----------------------------------------------------------------
  # First the models 
  test_glyph_rf = qread(here('data/results/ml-water/test-glyph-fit-rf.qs'))
  test_ampa_rf = qread(here('data/results/ml-water/test-ampa-fit-rf.qs'))
  test_glyph_lasso = qread(here('data/results/ml-water/test-glyph-fit-lasso.qs'))
  test_ampa_lasso = qread(here('data/results/ml-water/test-ampa-fit-lasso.qs'))

# Checking performance --------------------------------------------------------
  # RMSE and R2 
  collect_metrics(test_glyph_rf)
  collect_metrics(test_glyph_lasso)
  collect_metrics(test_ampa_rf)
  collect_metrics(test_ampa_lasso)
  # Variable Importance for RF models
  ggsave(
    filename = here('figures/water-ml/vip-glyph.jpeg'),
    test_glyph_rf |>
      extract_fit_parsnip() |>
      vip(num_features = 50) + 
      theme_minimal(),
    width = 5, height = 12,
    bg ='white'
  )
  ggsave(
    filename = here('figures/water-ml/vip-ampa.jpeg'),
    test_ampa_rf |>
      extract_fit_parsnip() |>
      vip(num_features = 50) + 
      theme_minimal(),
    width = 5, height = 12,
    bg ='white'
  )
  # Putting predictions together nicely 
  pred_dt = 
    rbind(
      data.table(test_ampa_lasso$.predictions[[1]])[,.(
        chemical = 'AMPA',
        model = 'LASSO',
        actual = ampa_result, 
        pred = .pred
      )],
      data.table(test_ampa_rf$.predictions[[1]])[,.(
        chemical = 'AMPA',
        model = 'Random Forest',
        actual = ampa_result, 
        pred = .pred
      )],
      data.table(test_glyph_lasso$.predictions[[1]])[,.(
        chemical = 'Glyphosate',
        model = 'LASSO',
        actual = gly_result, 
        pred = .pred
      )],
      data.table(test_glyph_rf$.predictions[[1]])[,.(
        chemical = 'Glyphosate',
        model = 'Random Forest',
        actual = gly_result, 
        pred = .pred
      )]
    )
  # Plot results for glyphosate 
  pred_glyph_p = 
    pred_dt[chemical == 'Glyphosate'] |>
    ggplot(aes(x = actual, y = pred, color = model, shape = model)) + 
    geom_point(alpha = 0.2) +
    geom_smooth(se = TRUE) +
    scale_x_continuous("Actual", limits = c(0,1.0))+ 
    scale_y_continuous("Predicted", limits = c(0,1.0)) + 
    scale_shape_manual(name = 'Model', values = c(16,17))+
    scale_color_viridis_d(
      option = 'magma', 
      begin = 0.2, end = 0.9, 
      name = 'Model'
    ) + 
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
    theme_minimal() 
  ggsave(
    plot = pred_glyph_p, 
    filename = here('figures/water-ml/pred-v-actual-glyph.jpeg'), 
    width = 5, height = 3, 
    bg = 'white'
  )
  # Now for AMPA
  pred_ampa_p = 
    pred_dt[chemical == 'AMPA'] |>
    ggplot(aes(x = actual, y = pred, color = model, shape = model)) + 
    geom_point(alpha = 0.2) +
    geom_smooth(se = TRUE) +
    scale_x_continuous("Actual", limits = c(0,1.0))+ 
    scale_y_continuous("Predicted", limits = c(0,1.0)) + 
    scale_shape_manual(name = 'Model', values = c(16,17))+
    scale_color_viridis_d(
      option = 'magma', 
      begin = 0.2, end = 0.9, 
      name = 'Model'
    ) + 
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
    theme_minimal() 
  ggsave(
    plot =  pred_ampa_p, 
    filename = here('figures/water-ml/pred-v-actual-ampa.jpeg'), 
    width = 5, height = 3, 
    bg = 'white'
  )
  # Density of predicted and actual 
  pred_density_p = 
    pred_dt |>
    melt(
      id.vars = c('chemical','model'), 
      measure.vars = c('actual','pred')
    ) %>%
    .[variable == 'pred' | (variable == 'actual' & model == 'LASSO'),.(
      chemical, 
      data = ifelse(variable == 'actual','Actual',model) |>
        factor(levels = c('LASSO','Random Forest','Actual')), 
      value
    )]|>
    ggplot(aes(x = value, fill = data, color = data)) + 
    geom_density(alpha = 0.3) +
    scale_color_viridis_d(
      option = 'magma', 
      end = 0.9, 
      name = '',
      aesthetics = c('color','fill')
    ) +
    labs(
      x = 'Value', 
      y = 'Density'
    ) + 
    facet_wrap(~chemical, scales = 'free') + 
    theme_minimal() + 
    theme(legend.position = 'bottom')
  ggsave(
    plot = pred_density_p, 
    filename = here('figures/water-ml/pred-density.jpeg'), 
    width = 7, height = 3.5, 
    bg = 'white'
  )
