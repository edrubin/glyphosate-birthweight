# Notes ----------------------------------------------------------------------------------
#   Goal:   Plot summaries of predictions by percentile.
#   Time:   Very short (<1 minute)


# Data notes -----------------------------------------------------------------------------
#   - Summaries are in data/clean/prediction/summaries/*.fst
#   - Filenames have five components:
#       1.  The birthweight used to create the distribution/percentiles:
#             - 'pctlactual' (percentile using actual bwt)
#             - 'pctlpred' (percentile using predicted bwt)
#       2.  The population SUMMARIZED:
#             - 'ruralocc' (rural occurrence)
#             - 'ruralres' (rural residence)
#             - 'allbirths' (all births)
#       3.  The population used to build the distribution (the ECDF) in the pre-period:
#             - 'ecdfocc' (rural occurrence)
#             - 'ecdfres' (rural residence)
#       4.  The temporal subset (if any):
#             - 'alltime' (the full sample)
#             - 'pre' (births prior to 1996)
#             - 'post' (births after 1996, inclusive)


# Setup ----------------------------------------------------------------------------------
  # Load packages
  library(pacman)
  p_load(data.table, collapse, stringr, fst, magrittr, ggplot2, scales, here)


# Function: Plot percentile-based summaries ----------------------------------------------
# Function that loads data and then creates/saves the plots
#   - `bwt_type`, `pop_sum`, `pop_ecdf` together select the file (described above)
#   - `sub_time` ('alltime', 'pre', 'post') determines temporal subsets
#   - `save_dir` directory where the plots will be saved
#   - `bin_width` is the width of the bins for the plots
#      Note: bins are already percentiles, so bin_width cannot give non-whole bins
  # The function
  plot_summaries = function(
    bwt_type, pop_sum, pop_ecdf, sub_time, save_dir, bin_width = .01
  ) {
    # Confirm packages are loaded
    require(pacman)
    require(data.table)
    require(collapse)
    require(stringr)
    require(fst)
    require(magrittr)
    require(ggplot2)
    require(scales)
    require(here)
    # Define variable subsets
    vars_dbwt = c('dbwt', 'dbwt_pred')
    vars_age = c('m_age', 'f_age')
    vars_ord = c('total_birth_order', 'live_birth_order')
    vars_pct = c('i_female', 'i_m_black', 'i_m_white', 'i_married')
    vars_edu = c('i_m_educ_nohs', 'i_m_educ_hs', 'i_m_educ_bs')
    # All variables
    vars_all = c('pctl', 'n', vars_dbwt, vars_age, vars_ord, vars_pct, vars_edu)
    # Load the data
    pctl_dt =
      here(
        'data', 'clean', 'prediction', 'summaries',
        paste(bwt_type, pop_sum, pop_ecdf, sub_time, sep = '-') |> paste0('.fst')
      ) |>
      read_fst(as.data.table = TRUE)
    # Select the desired variables
    pctl_dt = pctl_dt[, ..vars_all]
    # Melt the data
    plot_dt = pctl_dt |> pivot(ids = c('pctl', 'n'), how = 'longer', factor = FALSE)
    # Collapse to desired bin width
    plot_dt[, bin := pctl %/% bin_width * bin_width]
    plot_dt =
      plot_dt[, .(value = fmean(value, w = n)), by = .(variable, pctl = bin)]
    # Create factors for labels
    plot_dt[, var_f := factor(
      variable,
      levels = vars_all,
      labels = c(
        'Percentile', 'N',
        'Birthweight', 'Predicted Birthweight',
        'Mother Age', 'Father Age',
        'Total Birth Order', 'Live Birth Order',
        'Pct. Female', 'Pct. Black Mother', 'Pct. White Mother', 'Pct. Married',
        'Pct. No HS', 'Pct. HS Grad.', 'Pct. BS Grad.'
      )
    )]
    # Create x-axis label
    if (bwt_type == 'pctlactual') {
      x_lab = 'Percentile (of actual birthweight)'
    } else {
      x_lab = 'Percentile (of predicted birthweight)'
    }
    # Plot: Birthweight and predicted birthweight
    plot_bwt =
      plot_dt |>
      fsubset(variable %in% vars_dbwt) |>
      ggplot(aes(x = pctl)) +
      geom_vline(xintercept = 0) +
      # geom_point(aes(y = value, color = var_f), size = 1.8) +
      geom_line(aes(y = value, color = var_f), linewidth = 1.2) +
      scale_fill_viridis_d('', option = 'magma', end = 0.9) +
      scale_color_viridis_d('', option = 'magma', end = 0.9) +
      theme_minimal(base_size = 12) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = x_lab,
        y = 'Birthweight (grams)'
      ) +
      theme(legend.position = 'bottom')
    # Plot: Mother's and father's age
    plot_age =
      plot_dt |>
      fsubset(variable %in% vars_age) |>
      ggplot(aes(x = pctl)) +
      geom_vline(xintercept = 0) +
      # geom_point(aes(y = value, color = var_f), size = 1.8) +
      geom_line(aes(y = value, color = var_f), linewidth = 1.2) +
      scale_fill_viridis_d('', option = 'magma', end = 0.9) +
      scale_color_viridis_d('', option = 'magma', end = 0.9) +
      theme_minimal(base_size = 12) +
      labs(
        x = x_lab,
        y = 'Age (years)'
      ) +
      theme(legend.position = 'bottom')
    # Plot: Birth order
    plot_ord =
      plot_dt |>
      fsubset(variable %in% vars_ord) |>
      ggplot(aes(x = pctl)) +
      geom_vline(xintercept = 0) +
      # geom_hline(yintercept = 0) +
      # geom_point(aes(y = value, color = var_f), size = 1.8) +
      geom_line(aes(y = value, color = var_f), linewidth = 1.2) +
      scale_fill_viridis_d('', option = 'magma', end = 0.9) +
      scale_color_viridis_d('', option = 'magma', end = 0.9) +
      theme_minimal(base_size = 12) +
      labs(
        x = x_lab,
        y = 'Birth order'
      ) +
      theme(legend.position = 'bottom')
    # Plot: Birth demographics
    plot_dem =
      plot_dt |>
      fsubset(variable %in% vars_pct) |>
      ggplot(aes(x = pctl)) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      # geom_point(aes(y = value, color = var_f), size = 1.8) +
      geom_line(aes(y = value, color = var_f), linewidth = 1.2) +
      scale_fill_viridis_d('', option = 'magma', end = 0.9) +
      scale_color_viridis_d('', option = 'magma', end = 0.9) +
      theme_minimal(base_size = 12) +
      labs(
        x = x_lab,
        y = 'Percent of births'
      ) +
      theme(legend.position = 'bottom')
    # Plot: Education
    plot_edu =
      plot_dt |>
      fsubset(variable %in% vars_edu) |>
      ggplot(aes(x = pctl)) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      # geom_point(aes(y = value, color = var_f), size = 1.8) +
      geom_line(aes(y = value, color = var_f), linewidth = 1.2) +
      scale_fill_viridis_d('', option = 'magma', end = 0.9) +
      scale_color_viridis_d('', option = 'magma', end = 0.9) +
      theme_minimal(base_size = 12) +
      labs(
        x = x_lab,
        y = 'Percent of births'
      ) +
      theme(legend.position = 'bottom')
    # Save the plots
    ggsave(
      plot = plot_bwt,
      filename = here(
        save_dir,
        paste(
          bwt_type, pop_sum, pop_ecdf, sub_time,
          paste0('bwt-', round(1 / bin_width), '.png'),
          sep = '-'
        )
      ),
      width = 7, height = 4,
      bg = 'white'
    )
    # Save the plots
    ggsave(
      plot = plot_age,
      filename = here(
        save_dir,
        paste(
          bwt_type, pop_sum, pop_ecdf, sub_time,
          paste0('age-', round(1 / bin_width), '.png'),
          sep = '-'
        )
      ),
      width = 7, height = 4,
      bg = 'white'
    )
    # Save the plots
    ggsave(
      plot = plot_ord,
      filename = here(
        save_dir,
        paste(
          bwt_type, pop_sum, pop_ecdf, sub_time,
          paste0('ord-', round(1 / bin_width), '.png'),
          sep = '-'
        )
      ),
      width = 7, height = 4,
      bg = 'white'
    )
    # Save the plots
    ggsave(
      plot = plot_dem,
      filename = here(
        save_dir,
        paste(
          bwt_type, pop_sum, pop_ecdf, sub_time,
          paste0('dem-', round(1 / bin_width), '.png'),
          sep = '-'
        )
      ),
      width = 7, height = 4,
      bg = 'white'
    )
    # Save the plots
    ggsave(
      plot = plot_edu,
      filename = here(
        save_dir,
        paste(
          bwt_type, pop_sum, pop_ecdf, sub_time,
          paste0('edu-', round(1 / bin_width), '.png'),
          sep = '-'
        )
      ),
      width = 7, height = 4,
      bg = 'white'
    )
  }


# Plots: Predicted birthweight, rural residence, all time --------------------------------
  # Predicted birthweight
  plot_summaries(
    bwt_type = 'pctlpred',
    pop_sum = 'ruralres',
    pop_ecdf = 'ecdfres',
    sub_time = 'alltime',
    save_dir = here('figures', 'descriptive', 'demographic-pctl'),
    bin_width = .01
  )
  plot_summaries(
    bwt_type = 'pctlpred',
    pop_sum = 'ruralres',
    pop_ecdf = 'ecdfres',
    sub_time = 'alltime',
    save_dir = here('figures', 'descriptive', 'demographic-pctl'),
    bin_width = .02
  )


# Plots: Predicted birthweight, rural residence, pre/post --------------------------------
  # Predicted birthweight
  plot_summaries(
    bwt_type = 'pctlpred',
    pop_sum = 'ruralres',
    pop_ecdf = 'ecdfres',
    sub_time = 'pre',
    save_dir = here('figures', 'descriptive', 'demographic-pctl'),
    bin_width = .02
  )
  plot_summaries(
    bwt_type = 'pctlpred',
    pop_sum = 'ruralres',
    pop_ecdf = 'ecdfres',
    sub_time = 'post',
    save_dir = here('figures', 'descriptive', 'demographic-pctl'),
    bin_width = .02
  )


# Plots: Predicted birthweight, rural occurence ------------------------------------------
  # Predicted birthweight
  plot_summaries(
    bwt_type = 'pctlpred',
    pop_sum = 'ruralocc',
    pop_ecdf = 'ecdfocc',
    sub_time = 'alltime',
    save_dir = here('figures', 'descriptive', 'demographic-pctl'),
    bin_width = .02
  )


# Plots: Actual birthweight --------------------------------------------------------------
  # Actual birthweight, all time
  plot_summaries(
    bwt_type = 'pctlactual',
    pop_sum = 'ruralres',
    pop_ecdf = 'ecdfres',
    sub_time = 'alltime',
    save_dir = here('figures', 'descriptive', 'demographic-pctl'),
    bin_width = .02
  )
  # Actual birthweight, pre-period
  plot_summaries(
    bwt_type = 'pctlactual',
    pop_sum = 'ruralres',
    pop_ecdf = 'ecdfres',
    sub_time = 'pre',
    save_dir = here('figures', 'descriptive', 'demographic-pctl'),
    bin_width = .02
  )
  # Actual birthweight, post-period
  plot_summaries(
    bwt_type = 'pctlactual',
    pop_sum = 'ruralres',
    pop_ecdf = 'ecdfres',
    sub_time = 'post',
    save_dir = here('figures', 'descriptive', 'demographic-pctl'),
    bin_width = .02
  )
