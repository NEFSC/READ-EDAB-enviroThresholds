# scoring_functions_V6.R
# Functions needed to run thermal_suitability_scoring_V6.R
# Updated to evaluate only pct_suitable (perc_within_hist)

library(dplyr)
library(broom)

#----------------------------------
# linear trend calculation
#----------------------------------

calc_trend <- function(df, value_col) {
  
  # Remove rows where value is NA
  df <- df %>% filter(!is.na(.data[[value_col]]))
  
  # Need at least 3 non-missing points
  if (nrow(df) < 3) {
    return(tibble(term = c("(Intercept)", "year"), estimate = NA_real_, p.value = NA_real_))
  }
  
  # Compute SD safely
  value_sd <- sd(df[[value_col]])
  
  # If SD is NA or zero, skip
  if (is.na(value_sd) || value_sd == 0) {
    return(tibble(term = c("(Intercept)", "year"), estimate = NA_real_, p.value = NA_real_))
  }
  
  model <- try(lm(reformulate("year", value_col), data = df), silent = TRUE)
  
  if (inherits(model, "try-error")) {
    return(tibble(term = c("(Intercept)", "year"), estimate = NA_real_, p.value = NA_real_))
  }
  
  broom::tidy(model)
}

#----------------------------------
# Helper: safely extract slope + p-value
#----------------------------------

get_slope_pval <- function(trend_tbl) {
  year_row <- trend_tbl %>% filter(term == "year")
  
  if (nrow(year_row) == 0) {
    return(list(slope = NA_real_, pval = NA_real_))
  }
  
  list(slope = year_row$estimate[[1]], pval = year_row$p.value[[1]])
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

#----------------------------------
# Trend scoring
#----------------------------------

score_trend <- function(slope, pval) {
  # Handle NA safely
  if (is.na(slope) || is.na(pval)) return(0)
  
  # Must be statistically significant to count
  if (!isTRUE(pval < 0.05)) return(0)
  
  # Positive slope = Habitat expanding = Less risk averse (-1)
  # Negative slope = Habitat shrinking = More risk averse (+1)
  if (slope > 0) return(-1)
  if (slope < 0) return(1)
  
  return(0)
}

#----------------------------------
# Species-year risk score
#----------------------------------

score_species_year <- function(df_species, terminal_year) {
  
  df_sub <- df_species %>%
    filter(year <= terminal_year) %>%
    arrange(year)
  
  end_row <- df_sub %>% filter(year == terminal_year)
  
  if (nrow(end_row) == 0) {
    return(tibble(year = terminal_year, risk_score = NA_real_))
  }
  
  #----------------------------------
  # End state scores
  #----------------------------------
  end_suit <- score_end_state_suitable(end_row$pct_suitable)
  
  #----------------------------------
  # Long-term trend
  #----------------------------------
  long_suit_trend <- calc_trend(df_sub, "pct_suitable")
  long_suit_vals  <- get_slope_pval(long_suit_trend)
  long_suit_score <- score_trend(long_suit_vals$slope, long_suit_vals$pval)
  
  #----------------------------------
  # Short-term trend (last 5 years)
  #----------------------------------
  df_short <- df_sub %>% filter(year >= terminal_year - 4)
  short_suit_trend <- calc_trend(df_short, "pct_suitable")
  short_suit_vals  <- get_slope_pval(short_suit_trend)
  short_suit_score <- score_trend(short_suit_vals$slope, short_suit_vals$pval)
  
  #----------------------------------
  # Total score (bounded)
  #----------------------------------
  total <- sum(end_suit, long_suit_score, short_suit_score, na.rm = TRUE)
  
  # Bound between -4 and +4 for the Risk Policy Framework
  total <- max(min(total, 4), -4)
  
  tibble(
    year = terminal_year,
    risk_score = total
  )
}