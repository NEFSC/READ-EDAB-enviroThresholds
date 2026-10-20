# R/00_get_thermal_niches.R
#
# Purpose: Define each species' thermal niche from fall survey observations
#          (manuscript Section 2.1.2).
#
# Logic:   1. Positive FALL tows (NEFSC + MassDMF) with an in situ bottom
#             temperature, one row per tow x species, pooled across years.
#          2. Niche = 10th-90th percentile of bottom temperature at those tows.
#             No literature overrides: every niche comes from the survey.
#          3. Bootstrap: how stable are the percentiles at small sample sizes?
#             Used to choose params$min_positive_tows.
#          4. Species below params$min_positive_tows are excluded
#             (e.g., Atlantic salmon).
#          5. Literature comparison table (adult, Northeast US, field studies)
#             for Figure 1 / supplementary table. Does NOT change the niches.
#
# Output:
#   RDS : data-raw/thermal_niche.rds               (used by the pipeline)
#   CSV : data/thermal_niche/thermal_niche_table.csv   (Table 1)
#   CSV : data/thermal_niche/niche_bootstrap.csv
#   CSV : data/thermal_niche/survey_vs_literature.csv
#   PNG : images/thermal_niche/niche_bootstrap.png
#   PNG : images/thermal_niche/niche_histograms.png    (Figure 1 draft)
#
# Dependencies: tidyverse, here

library(tidyverse)
library(here)

source(here::here("R/utils_config.R"))
source(here::here("R/utils_pipeline.R"))

set.seed(27)

dir_data   <- ensure_dir(paths$niche_dir)
dir_images <- ensure_dir(paths$images_dir, "thermal_niche")


# -------------------------------------------------------------------
# 1. Fall positive tows with bottom temperature
# -------------------------------------------------------------------

message("Loading fall survey data...")

tows <- load_fall_positive_tows(paths, params) |>
  filter(!is.na(BOTTEMP))

message("  ", nrow(tows), " positive tow-species records with bottom temperature (",
        n_distinct(tows$COMNAME), " species).")


# -------------------------------------------------------------------
# 2. Survey-derived niches (10th-90th percentile)
# -------------------------------------------------------------------

niche <- tows |>
  group_by(COMNAME, SCINAME, manager) |>
  summarise(
    n_tows         = n(),
    n_tows_nefsc   = sum(SURVEY == "NEFSC"),
    n_tows_massdmf = sum(SURVEY == "MassDMF"),
    n_years        = n_distinct(YEAR),
    tmin           = unname(quantile(BOTTEMP, params$niche_probs[1])),
    tmax           = unname(quantile(BOTTEMP, params$niche_probs[2])),
    .groups = "drop"
  )


# -------------------------------------------------------------------
# 3. Bootstrap: percentile stability vs. sample size
# -------------------------------------------------------------------
# For species with many tows, repeatedly draw n tows (with replacement)
# and recompute the percentiles. The width of the central 90% of those
# estimates shows how precisely a niche can be estimated from n tows.

message("Bootstrapping percentile stability...")

ref_species <- niche |>
  filter(n_tows >= params$niche_boot_ref_min_n) |>
  pull(COMNAME)

boot <- tows |>
  filter(COMNAME %in% ref_species) |>
  group_by(COMNAME) |>
  group_modify(function(d, key) {
    map_dfr(params$niche_boot_n, function(n) {
      est <- replicate(params$niche_boot_reps, {
        x <- sample(d$BOTTEMP, n, replace = TRUE)
        quantile(x, params$niche_probs, names = FALSE)
      })
      tibble(
        n            = n,
        width_tmin   = diff(quantile(est[1, ], c(0.05, 0.95), names = FALSE)),
        width_tmax   = diff(quantile(est[2, ], c(0.05, 0.95), names = FALSE))
      )
    })
  }) |>
  ungroup()

boot_summary <- boot |>
  pivot_longer(c(width_tmin, width_tmax), names_to = "bound", values_to = "width") |>
  group_by(n, bound) |>
  summarise(median_width = median(width), q90_width = quantile(width, 0.9), .groups = "drop")

write_csv(boot, file.path(dir_data, "niche_bootstrap.csv"))

at_min <- boot_summary |> filter(n == params$min_positive_tows)

message("  Reference species: ", length(ref_species))
message(sprintf("  Median 90%% interval width at n = %d: %.2f deg C (10th pct), %.2f deg C (90th pct)",
                params$min_positive_tows,
                at_min$median_width[at_min$bound == "width_tmin"],
                at_min$median_width[at_min$bound == "width_tmax"]))

p_boot <- ggplot(boot_summary, aes(n, median_width, colour = bound)) +
  geom_vline(xintercept = params$min_positive_tows, linetype = "dotted", colour = "grey40") +
  geom_line(linewidth = 0.8) +
  geom_point() +
  scale_colour_manual(values = c(width_tmin = "#3182bd", width_tmax = "#e6550d"),
                      labels = c(width_tmin = "10th percentile", width_tmax = "90th percentile"),
                      name = NULL) +
  labs(
    title = "Precision of survey-derived niche bounds vs. number of positive tows",
    subtitle = paste0("Median across ", length(ref_species),
                      " reference species; dotted = minimum used"),
    x = "Positive fall tows sampled", y = "Width of 90% bootstrap interval (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(dir_images, "niche_bootstrap.png"), p_boot, width = 8, height = 5, dpi = 300, bg = "white")


# -------------------------------------------------------------------
# 4. Apply minimum-observation rule
# -------------------------------------------------------------------

niche <- niche |>
  mutate(
    included = n_tows >= params$min_positive_tows,
    source   = "Survey (fall, 10th-90th percentile)"
  ) |>
  arrange(COMNAME)

excluded <- niche |> filter(!included)
if (nrow(excluded) > 0) {
  message("Excluded (< ", params$min_positive_tows, " positive tows): ",
          paste0(species_label(excluded$COMNAME), " (", excluded$n_tows, ")", collapse = ", "))
}

saveRDS(niche, paths$niche)
write_csv(niche, file.path(dir_data, "thermal_niche_table.csv"))
message("Thermal niches saved to: ", paths$niche)


# -------------------------------------------------------------------
# 5. Literature comparison (descriptive only)
# -------------------------------------------------------------------

if (file.exists(paths$literature)) {
  
  lit <- read_csv(paths$literature, show_col_types = FALSE) |>
    mutate(
      COMNAME = str_to_upper(common.name),
      across(c(opt.temp.min, opt.temp.max, stress.temp.min, stress.temp.max,
               lethal.temp.min, lethal.temp.max),
             ~ suppressWarnings(as.numeric(.x)))
    ) |>
    filter(
      age.group == "Adult",
      str_detect(location, "(?i)Northeast US"),
      application == "Wild",                 # field studies only, no lab work
      COMNAME %in% niche$COMNAME
    )
  
  lit_long <- bind_rows(
    lit |> transmute(COMNAME, title, effect.type, limit = "Optimal", lit_tmin = opt.temp.min,    lit_tmax = opt.temp.max),
    lit |> transmute(COMNAME, title, effect.type, limit = "Stress",  lit_tmin = stress.temp.min, lit_tmax = stress.temp.max),
    lit |> transmute(COMNAME, title, effect.type, limit = "Lethal",  lit_tmin = lethal.temp.min, lit_tmax = lethal.temp.max)
  ) |>
    filter(!is.na(lit_tmin) | !is.na(lit_tmax))
  
  comparison <- niche |>
    select(COMNAME, n_tows, survey_tmin = tmin, survey_tmax = tmax) |>
    left_join(lit_long, by = "COMNAME") |>
    mutate(
      diff_tmin = lit_tmin - survey_tmin,
      diff_tmax = lit_tmax - survey_tmax
    )
  
  write_csv(comparison, file.path(dir_data, "survey_vs_literature.csv"))
  
  n_with_lit <- n_distinct(lit_long$COMNAME)
  message("Literature comparison: ", n_with_lit, " of ", nrow(niche),
          " species have adult, Northeast US, field-study values.")
} else {
  message("Literature file not found; skipping comparison.")
  lit_long <- tibble(COMNAME = character(), limit = character(),
                     lit_tmin = numeric(), lit_tmax = numeric())
}


# -------------------------------------------------------------------
# 6. Figure 1 draft: temperature histograms with niche bounds
# -------------------------------------------------------------------

plot_species <- niche |> filter(included) |> pull(COMNAME)

hist_df  <- tows |> filter(COMNAME %in% plot_species) |> mutate(label = species_label(COMNAME))
niche_df <- niche |> filter(included) |> mutate(label = species_label(COMNAME))
lit_df   <- lit_long |>
  filter(COMNAME %in% plot_species) |>
  mutate(label = species_label(COMNAME)) |>
  pivot_longer(c(lit_tmin, lit_tmax), values_to = "temp") |>
  filter(!is.na(temp))

p_hist <- ggplot(hist_df, aes(BOTTEMP)) +
  geom_histogram(binwidth = 0.5, fill = "grey70", colour = NA) +
  geom_vline(data = niche_df, aes(xintercept = tmin), linewidth = 0.6) +
  geom_vline(data = niche_df, aes(xintercept = tmax), linewidth = 0.6) +
  geom_vline(data = lit_df, aes(xintercept = temp, colour = limit),
             linetype = "dashed", linewidth = 0.4) +
  scale_colour_manual(values = c(Optimal = "#1b9e77", Stress = "#d95f02", Lethal = "#7570b3"),
                      name = "Literature limit", drop = FALSE) +
  facet_wrap(~label, scales = "free_y", ncol = 5) +
  labs(
    title = "Fall bottom temperature at positive tows",
    subtitle = "Solid lines: survey niche (10th-90th percentile). Dashed: literature values (adult, Northeast US, field studies).",
    x = "Bottom temperature (°C)", y = "Positive tows"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold", size = 8), legend.position = "bottom")

n_rows <- ceiling(length(plot_species) / 5)
ggsave(file.path(dir_images, "niche_histograms.png"), p_hist,
       width = 14, height = max(4, 2.2 * n_rows + 1.5), dpi = 300, bg = "white")

message("Script complete.")