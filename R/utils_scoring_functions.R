# functions needed to run thermal_suitability_scoring.R
# MTG 02/20/2026


library(dplyr)
library(broom)

#----------------------------------
# linear trend calculation
#----------------------------------

calc_trend <- function(df, value_col) {
  
  # Remove rows where value is NA
  df <- df %>%
    filter(!is.na(.data[[value_col]]))
  
  # Need at least 3 non-missing points
  if (nrow(df) < 3) {
    return(tibble(
      term     = c("(Intercept)", "year"),
      estimate = NA_real_,
      p.value  = NA_real_
    ))
  }
  
  # Compute SD safely
  value_sd <- sd(df[[value_col]])
  
  # If SD is NA or zero, skip
  if (is.na(value_sd) || value_sd == 0) {
    return(tibble(
      term     = c("(Intercept)", "year"),
      estimate = NA_real_,
      p.value  = NA_real_
    ))
  }
  
  model <- try(
    lm(reformulate("year", value_col), data = df),
    silent = TRUE
  )
  
  if (inherits(model, "try-error")) {
    return(tibble(
      term     = c("(Intercept)", "year"),
      estimate = NA_real_,
      p.value  = NA_real_
    ))
  }
  
  broom::tidy(model)
}


#----------------------------------
# Helper: safely extract slope + p-value
#----------------------------------

get_slope_pval <- function(trend_tbl) {
  
  year_row <- trend_tbl %>%
    filter(term == "year")
  
  if (nrow(year_row) == 0) {
    return(list(slope = NA_real_, pval = NA_real_))
  }
  
  list(
    slope = year_row$estimate[[1]],
    pval  = year_row$p.value[[1]]
  )
}


#----------------------------------
# End state scoring
#----------------------------------

score_end_state_suitable <- function(pct) {
  dplyr::case_when(
    pct >= 80 ~ -2,
    pct >= 60 ~ -1,
    pct >= 40 ~  1,
    pct >= 20 ~  2,
    TRUE      ~  3
  )
}

score_end_state_heat <- function(val) {
  dplyr::case_when(
    val <= -1  ~ -2,
    val <=  0  ~ -1,
    val <=  1  ~  1,
    val <=  2  ~  2,
    TRUE       ~  3
  )
}


#----------------------------------
# Trend scoring
#----------------------------------

score_trend <- function(slope, pval, type) {
  
  # Handle NA safely
  if (is.na(slope) || is.na(pval)) return(0)
  
  if (!isTRUE(pval < 0.05)) return(0)
  
  if (type == "suitable") {
    if (slope > 0) return(-1)
    if (slope < 0) return(1)
  }
  
  if (type == "heat") {
    if (slope > 0) return(1)
    if (slope < 0) return(-1)
  }
  
  return(0)
}


#----------------------------------
# Species-year risk score
#----------------------------------

score_species_year <- function(df_species, terminal_year) {
  
  df_sub <- df_species %>%
    filter(year <= terminal_year) %>%
    arrange(year)
  
  end_row <- df_sub %>%
    filter(year == terminal_year)
  
  if (nrow(end_row) == 0) {
    return(tibble(year = terminal_year, risk_score = NA_real_))
  }
  
  #----------------------------------
  # End state scores
  #----------------------------------
  
  end_suit <- score_end_state_suitable(end_row$pct_suitable)
  end_heat <- score_end_state_heat(end_row$heat_stress)
  
  #----------------------------------
  # Long-term trend
  #----------------------------------
  
  long_suit_trend <- calc_trend(df_sub, "pct_suitable")
  long_heat_trend <- calc_trend(df_sub, "heat_stress")
  
  long_suit_vals <- get_slope_pval(long_suit_trend)
  long_heat_vals <- get_slope_pval(long_heat_trend)
  
  long_suit_score <- score_trend(
    long_suit_vals$slope,
    long_suit_vals$pval,
    "suitable"
  )
  
  long_heat_score <- score_trend(
    long_heat_vals$slope,
    long_heat_vals$pval,
    "heat"
  )
  
  #----------------------------------
  # Short-term trend (last 5 years)
  #----------------------------------
  
  df_short <- df_sub %>%
    filter(year >= terminal_year - 4)
  
  short_suit_trend <- calc_trend(df_short, "pct_suitable")
  short_heat_trend <- calc_trend(df_short, "heat_stress")
  
  short_suit_vals <- get_slope_pval(short_suit_trend)
  short_heat_vals <- get_slope_pval(short_heat_trend)
  
  short_suit_score <- score_trend(
    short_suit_vals$slope,
    short_suit_vals$pval,
    "suitable"
  )
  
  short_heat_score <- score_trend(
    short_heat_vals$slope,
    short_heat_vals$pval,
    "heat"
  )
  
  #----------------------------------
  # Total score (bounded)
  #----------------------------------
  
  total <- sum(
    end_suit,
    end_heat,
    long_suit_score,
    long_heat_score,
    short_suit_score,
    short_heat_score
  )
  
  total <- max(min(total, 4), -4)
  
  tibble(
    year = terminal_year,
    risk_score = total
  )
}
