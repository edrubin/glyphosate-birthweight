# Main results ----------------------------------------------------------------
library(pacman)
p_load(
  here, data.table, fst, fixest, qs,
  stringr, janitor, ggplot2, magrittr
)

# Function to extract data from event study models ----------------------------
extract_event_study_coefs = function(mod_path){
  # Loading the model 
  mod = qread(mod_path)
  # Extracting coefficients and ci 
  mod_dt = 
    cbind(
      data.table(coeftable(mod)),
      data.table(confint(mod))[,.(ci_l = `2.5 %`, ci_h = `97.5 %`)]
    ) |>
    clean_names()
  # Add reference coefficient 
  mod_dt %<>%
    rbind(
      mod_dt[,.(
        id, fixef, lhs, rhs, 
        coefficient = paste0(
          'year::1995:',
          str_extract(rhs, '(?<=i\\(year, ).*(?=, ref = 1995\\))')
        ),
        estimate = 0, ci_l = 0, ci_h = 0
        )
      ]|> unique(),
      use.names = TRUE, fill = TRUE
    )
  # Adding info from filename 
  mod_dt[,':='(
    spatial = str_extract(mod_path, '(?<=_spatial-).*(?=_het)'),
    cluster = str_extract(mod_path, '(?<=_cl-).*(?=_glynl)')
  )]
  # Some cleaning of fixef names
  mod_dt[,':='(
    year = str_extract(coefficient, '(?<=year::)\\d{4}') |> as.integer(),
    fixef_num = fcase(
      fixef == 'year_month + fips_res + fips_occ', 0,
      fixef == 'year_month + fips_res + fips_occ + sex + mage + mrace + mhisp + meduc + mar + birth_facility + restatus + total_birth_order', 1,
      fixef == 'year_month + fips_res + fips_occ + fage + fhisp + frace', 2,
      fixef == 'year_month + fips_res + fips_occ + sex + mage + mrace + mhisp + meduc + mar + birth_facility + restatus + total_birth_order + fage + fhisp + frace', 3
    ),
    control_num = fcase(
      str_detect(rhs, 'atrazine') & str_detect(rhs, 'unemployment'), 3,
      str_detect(rhs, 'atrazine'), 1,
      str_detect(rhs, 'unemployment'), 2,
      default = 0
    ),
    trt = str_extract(rhs, '(?<=i\\(year, ).*(?=, ref = 1995\\))')
  )]
  mod_dt[,
    var_of_interest := str_detect(coefficient, trt)
  ]
  # return the results 
  return(mod_dt)
}

# Running for all models 
mod_paths = 
  str_subset(
    list.files(here('data/results/micro'), full.names = TRUE),
    'est_rf'
  )
mod_dt = lapply(mod_paths, extract_event_study_coefs) |> 
  rbindlist(use.names = TRUE, fill = TRUE)


# Function to plot the event study --------------------------------------------
outcome_in = 'dbwt'
fixef_num_in = '3'
control_num_in = '3'
trt_in = 'all_yield_diff_percentile_gmo'
spatial_in = 'rural'
sample_in = 'NANA'
pink       = '#e64173'
red_pink   = '#e64173'
turquoise  = '#20B2AA'
orange     = '#FFA500'
red        = '#fb6107'
blue       = '#648FFF'
dark_blue  = '#3b3b9a'
green      = '#8bb174'
grey_light = 'grey70'
grey_mid   = 'grey50'
grey_dark  = 'grey20'
purple     = '#6A5ACD'
slate      = '#314f4f'
# Setting height and width 
width_in = 6
height_in = 3


plot_reduced_form = function(
  mod_dt, 
  outcome_in, 
  fixef_num_in, 
  control_num_in, 
  trt_in, 
  spatial_in, 
  sample_in
){
  # Making plot 
  rf_event_p = 
    ggplot(
      data = mod_dt[
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == fixef_num_in &
        control_num == control_num_in &
        trt == trt_in &
        spatial == spatial_in &
        paste0(sample_var, sample) == sample_in
      ],
      aes(x = year, y = estimate, ymin = ci_l, ymax = ci_h)
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', size = 1, alpha = 1, linetype = 'dashed') +
    # Current plot
    geom_ribbon(fill = pink, alpha = 0.5) +
    geom_point(color = pink, size = 2.5) +
    geom_line(size = 0.3, color = pink) +
    # General figure stuff
    scale_y_continuous('Birth weight (g)', breaks = seq(0, -120, -20)) +
    scale_x_continuous('Year', breaks = seq(1990, 2015, 5), minor_breaks = NULL) +
    theme_minimal(base_size = 16) +
    theme(axis.title.x = element_blank())
  rf_event_p
  ggsave(
    rf_event_p, 
    filename = here('figures/nature-draft/rf-event-main.pdf'),
    device = cairo_pdf, 
    width = width_in, height = height_in
  )
}


