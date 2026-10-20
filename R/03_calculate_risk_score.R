# R/03_calculate_risk_scores.R
#
# Purpose: Convert the fall thermal suitability indicator S into Risk Policy
#          scores (-4 to +4) against a fixed climatological baseline
#          (manuscript Section 2.4).
#
# Logic:   1. Baseline mean and SD of S over params$baseline_years (1982-2011),
#             per species.
#          2. z = (S - baseline mean) / baseline SD for every year.
#             If the SD is 0 (or missing), z and the score are NA, not 0.
#             An optional SD floor is set by params$min_baseline_sd.
#             Years with an incomplete fall (fewer than params$season_days
#             daily layers) are not scored.
#          3. z mapped to -4..+4 with z_to_risk_score() (Table 2). Lower
#             suitability -> positive (more precautionary) score.
#
# Output:
#   RDS : data/scoring/risk_scores_hindcast_fall.rds
#   RDS : data/scoring/risk_scores_terminal_fall.rds
#   CSV : data/scoring/baseline_diagnostics.csv
#   PNG : images/scoring/score_distribution_by_period.png
#   PNG : images/indicators/<species>_fall_suitability.png
#   PNG : images/indicators/ALL_SPECIES_fall_suitability.png
#
# Dependencies: tidyverse, here

library(tidyverse)
library(here)

source(here::here("R/utils_config.R"))
source(here::here("R/utils_pipeline.R"))

dir_scoring    <- ensure_dir(paths$scoring_dir)
dir_img_score  <- ensure_dir(paths$images_dir, "scoring")
dir_img_ind    <- ensure_dir(paths$images_dir, "indicators")

baseline_start <- min(params$baseline_years)
baseline_end   <- max(params$baseline_years)
baseline_label <- paste0(baseline_start, "–", baseline_end)


# -------------------------------------------------------------------
# 1. Load indicator
# -------------------------------------------------------------------

if (!file.exists(paths$indicator)) {
  stop("Indicator not found. Run 02_get_perc_suitable_thermal_habitat.R first.")
}

indicator <- readRDS(paths$indicator) |>
  filter(!is.na(perc_suitable))

# A fall with missing days (e.g., the current year before December) would
# bias S, so it is not scored.
incomplete <- indicator |> filter(n_days < params$season_days) |> distinct(year, n_days)
if (nrow(incomplete) > 0) {
  message("Not scored (incomplete fall): ",
          paste0(incomplete$year, " (", incomplete$n_days, " days)", collapse = ", "))
  indicator <- indicator |> filter(n_days >= params$season_days)
}


# -------------------------------------------------------------------
# 2. Baseline statistics and z-scores
# -------------------------------------------------------------------

message("Scoring against fixed ", baseline_label, " baseline...")

scores <- indicator |>
  arrange(species, year) |>
  group_by(species) |>
  mutate(
    in_baseline   = year %in% params$baseline_years,
    n_baseline    = sum(in_baseline),
    baseline_mean = mean(perc_suitable[in_baseline]),
    baseline_sd   = sd(perc_suitable[in_baseline]),
    sd_used       = pmax(baseline_sd, params$min_baseline_sd),
    z_score       = if_else(is.na(sd_used) | sd_used == 0, NA_real_,
                            (perc_suitable - baseline_mean) / sd_used),
    risk_score    = z_to_risk_score(z_score),
    period        = case_when(
      year < baseline_start ~ "Pre-baseline",
      in_baseline           ~ "Baseline",
      TRUE                  ~ "Post-baseline"
    ),
    period = factor(period, levels = c("Pre-baseline", "Baseline", "Post-baseline"))
  ) |>
  ungroup()


# -------------------------------------------------------------------
# 3. Baseline diagnostics
# -------------------------------------------------------------------

diagnostics <- scores |>
  distinct(species, n_baseline, baseline_mean, baseline_sd, sd_used) |>
  mutate(
    incomplete_baseline = n_baseline < length(params$baseline_years),
    low_sd              = is.na(baseline_sd) | baseline_sd < params$low_sd_flag,
    near_bound          = baseline_mean > 95 | baseline_mean < 5
  ) |>
  arrange(baseline_sd)

write_csv(diagnostics, file.path(dir_scoring, "baseline_diagnostics.csv"))

flagged <- diagnostics |> filter(incomplete_baseline | low_sd | near_bound)
if (nrow(flagged) > 0) {
  message("Species needing a look (see baseline_diagnostics.csv):")
  for (i in seq_len(nrow(flagged))) {
    f <- flagged[i, ]
    message(sprintf("  %-28s mean %5.1f%%  sd %5.2f  n=%d%s%s%s",
                    species_label(f$species), f$baseline_mean, f$baseline_sd, f$n_baseline,
                    if (f$incomplete_baseline) "  [incomplete baseline]" else "",
                    if (f$low_sd) "  [low SD]" else "",
                    if (f$near_bound) "  [near 0/100%]" else ""))
  }
}


# -------------------------------------------------------------------
# 4. Save scores
# -------------------------------------------------------------------

hindcast <- scores |>
  select(species, year, period, perc_suitable, baseline_mean, baseline_sd,
         z_score, risk_score, bt_source)

terminal <- hindcast |>
  group_by(species) |>
  filter(year == max(year)) |>
  ungroup()

saveRDS(hindcast, file.path(dir_scoring, "risk_scores_hindcast_fall.rds"))
saveRDS(terminal, file.path(dir_scoring, "risk_scores_terminal_fall.rds"))
write_csv(hindcast, file.path(dir_scoring, "risk_scores_hindcast_fall.csv"))

message("Scores saved to: ", dir_scoring)


# -------------------------------------------------------------------
# 5. Score distribution by period (Results 3.2)
# -------------------------------------------------------------------
# Baseline years are centred on 0 by construction. The informative
# comparison is how the pre- and post-baseline distributions differ from it.

dist_df <- hindcast |>
  filter(!is.na(risk_score)) |>
  count(period, risk_score) |>
  group_by(period) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

p_dist <- ggplot(dist_df, aes(factor(risk_score, levels = -4:4), prop)) +
  geom_col(fill = "darkcyan", colour = "black", alpha = 0.8) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), vjust = -0.4, size = 3) +
  facet_wrap(~period, ncol = 1) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = paste0("Distribution of fall thermal habitat risk scores (baseline ", baseline_label, ")"),
    x = "Risk score (negative = less risk averse, positive = more risk averse)",
    y = "Share of species-years"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(), plot.title = element_text(face = "bold"))

ggsave(file.path(dir_img_score, "score_distribution_by_period.png"), p_dist,
       width = 8, height = 8, dpi = 300, bg = "white")


# -------------------------------------------------------------------
# 6. Indicator time series with scoring bands
# -------------------------------------------------------------------
# Shaded bands mark the baseline mean +/- 0.5, 1, 1.5 and 2 SD, i.e. the
# score boundaries, so each year's score can be read off the plot.

bands <- diagnostics |>
  filter(!is.na(sd_used), sd_used > 0) |>
  crossing(k = c(2, 1.5, 1, 0.5)) |>
  mutate(ymin = pmax(baseline_mean - k * sd_used, 0),
         ymax = pmin(baseline_mean + k * sd_used, 100))

plot_suitability <- function(df, band_df) {
  ggplot(df, aes(year, perc_suitable)) +
    annotate("rect", xmin = baseline_start - 0.5, xmax = baseline_end + 0.5,
             ymin = -Inf, ymax = Inf, fill = "grey95") +
    geom_rect(data = band_df, inherit.aes = FALSE,
              aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
              fill = "steelblue", alpha = 0.08) +
    geom_hline(data = band_df |> distinct(species, baseline_mean),
               aes(yintercept = baseline_mean), linetype = "dashed", colour = "grey30") +
    geom_line(linewidth = 0.5) +
    geom_point(aes(colour = factor(risk_score, levels = -4:4)), size = 1.2) +
    scale_colour_manual(
      values = setNames(colorRampPalette(c("#27ae60", "grey60", "#c0392b"))(9), -4:4),
      name = "Risk score", drop = FALSE, na.value = "black"
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(x = "Year", y = "Suitable fall thermal habitat (%)")
}

walk(unique(hindcast$species), function(sp) {
  p <- plot_suitability(filter(hindcast, species == sp), filter(bands, species == sp)) +
    labs(
      title = paste0(species_label(sp), " — Fall thermal habitat suitability"),
      subtitle = paste0("Grey column: ", baseline_label, " baseline. Dashed: baseline mean. ",
                        "Bands: ±0.5, 1, 1.5, 2 SD.")
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  ggsave(file.path(dir_img_ind, paste0(safe_name(sp), "_fall_suitability.png")),
         p, width = 9, height = 5, dpi = 300, bg = "white")
})

p_all <- plot_suitability(hindcast, bands) +
  facet_wrap(~species, ncol = 5, labeller = as_labeller(species_label)) +
  labs(title = paste0("Fall thermal habitat suitability (baseline ", baseline_label, ")")) +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 7), legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

n_rows <- ceiling(n_distinct(hindcast$species) / 5)
ggsave(file.path(dir_img_ind, "ALL_SPECIES_fall_suitability.png"), p_all,
       width = 15, height = max(5, 2.3 * n_rows + 1.5), dpi = 300, bg = "white")


# -------------------------------------------------------------------
# 7. Console summary
# -------------------------------------------------------------------

message("\nShare of species-years by score and period:")
print(dist_df |> select(-n) |>
        pivot_wider(names_from = period, values_from = prop, values_fill = 0) |>
        arrange(risk_score))

message("\nTerminal-year scores (", max(terminal$year), "):")
print(table(terminal$risk_score, useNA = "ifany"))