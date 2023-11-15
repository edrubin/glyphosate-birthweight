# Testing B-Spline heterogeneity 
library(pacman)
p_load(
  here, data.table, fst, fixest, splines, stringr,
  janitor
)

# Loading the data ------------------------------------------------------------
  # The micro-natality dataset
  natality_dt = read.fst(
    here('data/clean/mini-data.fst'),
    as.data.table = TRUE
  )[!is.na(dbwt_pred_pctl_pre)]
  # Create deciles and vigintiles
  natality_dt[, `:=`(
    pred_q10 = cut(
      x = dbwt_pred_pctl_pre,
      breaks = 10,
      labels = 1:10, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_q20 = cut(
      x = dbwt_pred_pctl_pre,
      breaks = 20,
      labels = 1:20, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    )
  )]

# Estimating the model --------------------------------------------------------
  # 2SLS: Event study with splines 
  tsls_spline_mod = feols(
    data = natality_dt, 
    cluster = ~year + state_fips,
    fml = dbwt ~ 
      # Controls  
      alachlor_km2 + atrazine_km2 + cyanazine_km2 + fluazifop_km2 + 
      metolachlor_km2 + metribuzin_km2 + nicosulfuron_km2 + 
      unemployment_rate 
      | # Fixed effects 
      year_month + fips_res + fips_occ
      | # IV
      bs(dbwt_pred_pctl_pre, df = spline_df, intercept = FALSE):glyph_km2 ~ 
        1 + i(year, all_yield_diff_percentile_gmo, ref = 1995)
  )
  # 2SLS: Event study with quintile het 
  tsls_quintile_mod = feols(
    data = natality_dt, 
    cluster = ~year + state_fips,
    fml = dbwt ~ 
      # Controls  
      alachlor_km2 + atrazine_km2 + cyanazine_km2 + fluazifop_km2 + 
      metolachlor_km2 + metribuzin_km2 + nicosulfuron_km2 + 
      unemployment_rate 
      | # Fixed effects 
      year_month + fips_res + fips_occ
      | # IV
      i(pred_q5, glyph_km2) ~ 
        1 + i(year, all_yield_diff_percentile_gmo, ref = 1995)
  )
  tsls_decile_mod = feols(
    data = natality_dt, 
    cluster = ~year + state_fips,
    fml = dbwt ~ 
      # Controls  
      alachlor_km2 + atrazine_km2 + cyanazine_km2 + fluazifop_km2 + 
      metolachlor_km2 + metribuzin_km2 + nicosulfuron_km2 + 
      unemployment_rate 
      | # Fixed effects 
      year_month + fips_res + fips_occ
      | # IV
      i(pred_q10, glyph_km2) ~ 
        1 + i(year, all_yield_diff_percentile_gmo, ref = 1995)
  )
  tsls_vigintile_mod = feols(
    data = natality_dt, 
    cluster = ~year + state_fips,
    fml = dbwt ~ 
      # Controls  
      alachlor_km2 + atrazine_km2 + cyanazine_km2 + fluazifop_km2 + 
      metolachlor_km2 + metribuzin_km2 + nicosulfuron_km2 + 
      unemployment_rate 
      | # Fixed effects 
      year_month + fips_res + fips_occ
      | # IV
      i(pred_q20, glyph_km2) ~ 
        1 + i(year, all_yield_diff_percentile_gmo, ref = 1995)
  )
  tsls_split_mod = feols(
    data = natality_dt, 
    cluster = ~year + state_fips,
    fsplit = ~pred_q5,
    fml = dbwt ~ 
      # Controls  
      alachlor_km2 + atrazine_km2 + cyanazine_km2 + fluazifop_km2 + 
      metolachlor_km2 + metribuzin_km2 + nicosulfuron_km2 + 
      unemployment_rate 
      | # Fixed effects 
      year_month + fips_res + fips_occ
      | # IV
      glyph_km2 ~ 
        1 + i(year, all_yield_diff_percentile_gmo, ref = 1995)
  )

# Comparing the results -------------------------------------------------------
  # Extracting spline coefs and std errors 
  get_spline_coefs = function(mod, x){
    # Extracting spline coefs 
    spline_coef = matrix(coef(mod, keep = 'bs'))
    # Getting vcov matrix 
    vcov_mat = 
      data.table(vcov(mod), keep.rownames = TRUE)[str_detect(rn, 'bs')] |>
      get_vars(vars = 'bs', regex = TRUE) |>
      as.matrix()
    # Creating output table 
    spline_mat = data.table(
      bs(x, df = length(spline_coef), intercept = TRUE)
    )[# Simplifying matrix to 1000 obs
      seq(1, length(x), by  = floor(length(x)/1e4))
    ] |> as.matrix()
    # Calculating estimate and standard errors
    effect_dt_raw = data.table(
      x = x[seq(1, length(x), by  = floor(length(x)/1e4))],
      est = c(spline_mat %*% spline_coef),
      se = diag(spline_mat %*% vcov_mat %*% t(spline_mat)) |> sqrt()
    )
    # Adding outcome and fixed effects to results 
    effect_dt = effect_dt_raw[,.(
      bin_type = 'Splines',
      dbwt_pred_pctl_pre = x,
      coef = est, 
      coef_l = est - 1.96*se, 
      coef_h = est + 1.96*se,
      se
    )] 
    return(effect_dt)
  }

# Adding quintiles to the table
effect_dt = 
  rbind(
    # Getting spline results
    get_spline_coefs(tsls_spline_mod, natality_dt$dbwt_pred_pctl_pre),
    # First the interacted quintiles
    merge(
      data.table(
        dbwt_pred_pctl_pre = 0:100/100,
        pred_q5 =  c(floor(0:99/20) + 1, 5)
      ),
      coeftable(tsls_quintile_mod, keep = 'glyph_km2')|>
        data.table() |>
        clean_names() %>%
        .[,pred_q5 := 1:5],
      by = 'pred_q5'
    )[,.(
      bin_type = 'Quintiles-interacted',
      dbwt_pred_pctl_pre, 
      coef = estimate, 
      coef_l = estimate + qnorm(0.025)*std_error, 
      coef_h = estimate + qnorm(0.975)*std_error,
      se = std_error
    )],
    # Now the split quintiles
    merge(
      data.table(
        dbwt_pred_pctl_pre = 0:100/100,
        pred_q5 =  c(floor(0:99/20) + 1, 5) |> as.character()
      ),
      coeftable(tsls_split_mod, keep = 'glyph_km2')|>
        data.table() |>
        clean_names(),
      by.x = 'pred_q5',
      by.y = 'sample'
    )[,.(
      bin_type = 'Quintiles-split estimation',
      dbwt_pred_pctl_pre, 
      coef = estimate, 
      coef_l = estimate + qnorm(0.025)*std_error, 
      coef_h = estimate + qnorm(0.975)*std_error,
      se = std_error
    )],
    # Now the decile mod
    merge(
      data.table(
        dbwt_pred_pctl_pre = 0:100/100,
        pred_q10 =  c(floor(0:99/10) + 1, 10)
      ),
      coeftable(tsls_decile_mod, keep = 'glyph_km2')|>
        data.table() |>
        clean_names() %>%
        .[,pred_q10 := 1:10],
      by = 'pred_q10'
    )[,.(
      bin_type = 'Deciles-interacted',
      dbwt_pred_pctl_pre, 
      coef = estimate, 
      coef_l = estimate + qnorm(0.025)*std_error, 
      coef_h = estimate + qnorm(0.975)*std_error,
      se = std_error
    )],
    # Now the viginitle mod
    merge(
      data.table(
        dbwt_pred_pctl_pre = 0:100/100,
        pred_q20 =  c(floor(0:99/5) + 1, 20)
      ),
      coeftable(tsls_vigintile_mod, keep = 'glyph_km2')|>
        data.table() |>
        clean_names() %>%
        .[,pred_q20 := 1:20],
      by = 'pred_q20'
    )[,.(
      bin_type = 'Vigintiles-interacted',
      dbwt_pred_pctl_pre, 
      coef = estimate, 
      coef_l = estimate + qnorm(0.025)*std_error, 
      coef_h = estimate + qnorm(0.975)*std_error,
      se = std_error
    )]
  )


  ggplot(
    effect_dt[
      #str_detect(bin_type, 'Quintiles') & 
      bin_type !='Vigintiles-interacted'& 
      bin_type !='Deciles-interacted'
    ], 
    aes(
      x = dbwt_pred_pctl_pre, 
      y = coef, 
      ymin = coef_l, ymax = coef_h,
      color = bin_type, 
      fill = bin_type
    )
  ) + 
  geom_line() + 
  geom_ribbon(color = NA, alpha= 0.3) + 
  scale_color_brewer(
    name = "",
    palette = 'Dark2',
    aesthetics = c('fill','color')
  ) + 
  scale_x_continuous(
    name = 'Predicted BW Percentile',
    labels = scales::label_percent()
  ) +
  scale_y_continuous(
    name = "Effect of GLY on BW"
  ) +
  theme_minimal() 
  