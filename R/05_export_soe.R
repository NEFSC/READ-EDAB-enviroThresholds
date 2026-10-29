# R/05_export_soe.R
#
# Purpose: Export the fall thermal habitat suitability indicator for the
#          State of the Ecosystem (SOE) report, in ecodata's long format
#          (issue #25). Plotting is done in ecodata.
#
# Logic:   1. Species: adequate footprint (same set scored for the Risk Policy),
#             federally managed only. Report assignment by manager:
#             New England = NEFMC + JOINT, Mid-Atlantic = MAFMC + JOINT.
#          2. S is the whole-footprint indicator from 02 (identical to the
#             Risk Policy input). Incomplete falls are dropped.
#          3. Anomaly = S - species' mean S over params$baseline_years.
#          4. Regional mean = unweighted mean of species anomalies in each
#             report, by year.
#          5. Niches and footprints are recalculated with each annual update,
#             so the full historical series can change between SOE years.
#
# Output (data/soe/):
#   CSV/RDS : thermal_habitat_soe.csv / .rds    (ecodata long format)
#   TXT     : thermal_habitat_soe_run_info.txt  (species lists, years, run date)
#   PNG     : draft_heatmap_<scaled|raw>_<EPU>.png, draft_spaghetti_<EPU>.png (drafts)
#
# Dependencies: tidyverse, here

library(tidyverse)
library(here)

source(here::here("R/utils_config.R"))
source(here::here("R/utils_pipeline.R"))

dir_soe <- ensure_dir(paths$soe_dir)


# -------------------------------------------------------------------
# 1. Inputs and species selection
# -------------------------------------------------------------------

if (!file.exists(paths$indicator)) {
  stop("Indicator not found. Run 02_get_perc_suitable_thermal_habitat.R first.")
}

indicator  <- readRDS(paths$indicator)
footprints <- readRDS(paths$habitat)
niche      <- readRDS(paths$niche)

adequate <- names(keep(footprints, ~ isTRUE(.x$adequate_footprint)))

# One row per species x report (JOINT species appear in both reports)
report_species <- imap_dfr(params$soe_reports, function(managers, epu) {
  niche |>
    filter(included, COMNAME %in% adequate, manager %in% managers) |>
    transmute(species = COMNAME, manager, EPU = epu)
})

message("Species per report:")
report_species |> count(EPU) |> pwalk(~ message("  ", .x, ": ", .y))

not_federal <- setdiff(intersect(adequate, unique(indicator$species)), report_species$species)
if (length(not_federal) > 0) {
  message("Scored but not federally managed (not exported): ",
          paste(species_label(not_federal), collapse = ", "))
}


# -------------------------------------------------------------------
# 2. Complete falls only
# -------------------------------------------------------------------

incomplete <- indicator |> filter(n_days < params$season_days) |> distinct(year, n_days)
if (nrow(incomplete) > 0) {
  message("Dropped (incomplete fall): ",
          paste0(incomplete$year, " (", incomplete$n_days, " days)", collapse = ", "))
}

ind <- indicator |>
  filter(n_days >= params$season_days, !is.na(perc_suitable), species %in% report_species$species)


# -------------------------------------------------------------------
# 3. Anomalies relative to the baseline mean
# -------------------------------------------------------------------

ind <- ind |>
  group_by(species) |>
  mutate(
    baseline_mean = mean(perc_suitable[year %in% params$baseline_years]),
    anomaly       = perc_suitable - baseline_mean
  ) |>
  ungroup()

species_long <- report_species |>
  inner_join(ind, by = "species", relationship = "many-to-many") |>
  transmute(
    Time    = year,
    species = species_label(species),
    EPU,
    `Percent suitable` = perc_suitable,
    Anomaly            = anomaly
  ) |>
  pivot_longer(c(`Percent suitable`, Anomaly), names_to = "metric", values_to = "Value") |>
  mutate(
    Var   = paste(species, metric, sep = params$soe_var_sep),
    Units = if_else(metric == "Anomaly", "percentage points", "percent")
  )


# -------------------------------------------------------------------
# 4. Regional mean of anomalies
# -------------------------------------------------------------------

regional_mean <- species_long |>
  filter(metric == "Anomaly") |>
  group_by(Time, EPU) |>
  summarise(Value = mean(Value), n_species = n(), .groups = "drop")

# Every year should include every species in the report
n_check <- regional_mean |> distinct(EPU, n_species)
if (any(duplicated(n_check$EPU))) {
  warning("Number of species in the regional mean varies by year; check the indicator file.")
}

regional_long <- regional_mean |>
  transmute(
    Time, EPU, Value,
    Var   = paste(params$soe_mean_label, "Anomaly", sep = params$soe_var_sep),
    Units = "percentage points"
  )


# -------------------------------------------------------------------
# 5. Combine and save (ecodata format: Time, Var, Value, EPU, Units)
# -------------------------------------------------------------------

soe <- bind_rows(
  species_long |> select(Time, Var, Value, EPU, Units),
  regional_long
) |>
  arrange(EPU, Var, Time)

saveRDS(soe, file.path(dir_soe, "thermal_habitat_soe.rds"))
write_csv(soe, file.path(dir_soe, "thermal_habitat_soe.csv"))

message("Exported ", nrow(soe), " rows: ", min(soe$Time), "-", max(soe$Time),
        " | ", paste(unique(soe$EPU), collapse = ", "))


# -------------------------------------------------------------------
# 6. Run info (for the tech-doc and to track annual changes)
# -------------------------------------------------------------------

run_info <- c(
  paste("Run date:", Sys.Date()),
  paste("Years:", min(soe$Time), "-", max(soe$Time)),
  paste("Baseline for anomalies:", min(params$baseline_years), "-", max(params$baseline_years)),
  paste("Niche seasons:", if (is.null(params$niche_seasons)) "all" else paste(params$niche_seasons, collapse = ", ")),
  paste("Minimum positive tows:", params$min_positive_tows),
  paste("Minimum % of tows in footprint:", params$min_pct_tows_in_footprint),
  if (nrow(incomplete) > 0) paste("Dropped incomplete falls:", paste(incomplete$year, collapse = ", ")),
  "",
  imap_chr(split(report_species, report_species$EPU), function(d, epu) {
    paste0(epu, " (", nrow(d), " species): ",
           paste(species_label(sort(d$species)), collapse = ", "))
  })
)
writeLines(run_info, file.path(dir_soe, "thermal_habitat_soe_run_info.txt"))

# -------------------------------------------------------------------
# 7. Draft SOE figures (styled after ecodata::plot_condition)
# -------------------------------------------------------------------
# Drafts for review before the ecodata plotting function is written.
#   Heatmap  : species x year, built from percent suitable habitat.
#              "scaled" = each species scaled over its own time series, then
#              binned into quintiles across all species (as plot_condition).
#              "raw"    = continuous percent suitable.
#   Spaghetti: species anomalies (grey) + mean of managed species (bold).
# Colour palettes are viridis / rocket-mako (colour-blind safe), as in
# plot_condition. ecodata themes and GLS trend lines are used if ecodata is
# installed; otherwise ggplot2 equivalents are used.

has_ecodata <- requireNamespace("ecodata", quietly = TRUE)
report_name <- c(NE = "NewEngland", MA = "MidAtlantic")
report_long <- c(NE = "New England", MA = "the Mid-Atlantic")
n_classes   <- 5
quint_labels <- c("Low Suitability", "Below Average", "Neutral",
                  "Above Average", "High Suitability")

soe_theme <- function() {
  if (has_ecodata) list(ecodata::theme_ts(), ecodata::theme_title()) else list()
}

shade_layer <- function(epu, years) {
  # Most recent 10 years shaded, as in other SOE time series
  setup <- if (has_ecodata) {
    tryCatch(ecodata::plot_setup(shadedRegion = NULL, report = report_name[[epu]]),
             error = function(e) NULL)
  }
  x_min  <- if (!is.null(setup$x.shade.min)) setup$x.shade.min else max(years) - 9
  x_max  <- if (!is.null(setup$x.shade.max)) setup$x.shade.max else max(years)
  fill   <- if (!is.null(setup$shade.fill))  setup$shade.fill  else "lightgrey"
  alpha  <- if (!is.null(setup$shade.alpha)) setup$shade.alpha else 0.5
  ggplot2::annotate("rect", xmin = x_min, xmax = x_max, ymin = -Inf, ymax = Inf,
                    fill = fill, alpha = alpha)
}

plot_heatmap <- function(epu, plottype = c("scaled", "raw")) {
  
  plottype <- match.arg(plottype)
  
  fix <- species_long |>
    filter(EPU == epu, metric == "Percent suitable") |>
    group_by(species) |>
    mutate(scaleVal = if (plottype == "scaled") as.numeric(scale(Value)) else Value) |>
    ungroup()
  
  xs <- quantile(fix$scaleVal, seq(0, 1, length.out = n_classes + 1), na.rm = TRUE)
  
  fix <- fix |>
    mutate(category = cut(scaleVal, breaks = xs, labels = quint_labels, include.lowest = TRUE))
  
  # Order species by mean percent suitable (scaled means are all ~0)
  sort_names <- fix |>
    group_by(species) |>
    summarise(total = mean(Value, na.rm = TRUE), .groups = "drop") |>
    arrange(total) |>
    pull(species)
  fix$species <- factor(fix$species, levels = sort_names)
  
  p <- ggplot(fix, aes(x = Time, y = fct_rev(species)))
  
  if (plottype == "scaled") {
    p <- p +
      aes(fill = category) +
      scale_fill_manual(values = viridisLite::viridis(n_classes), drop = FALSE) +
      labs(fill = "Quantiles of\nSuitable Habitat")
  } else {
    p <- p +
      aes(fill = scaleVal) +
      scale_fill_gradientn(
        colors = c(viridisLite::rocket(4)[2:4], viridisLite::mako(4)[4:2]),
        values = scales::rescale(xs, from = range(xs), to = c(0, 1))
      ) +
      labs(fill = "Suitable\nHabitat (%)")
  }
  
  p +
    geom_tile() +
    coord_equal() +
    theme_bw() +
    scale_x_continuous(breaks = round(seq(min(fix$Time), max(fix$Time), by = 5))) +
    theme(
      legend.position  = "right",
      legend.box       = "vertical",
      legend.title     = element_text(size = 8),
      legend.text      = element_text(size = 6),
      axis.title       = element_blank(),
      axis.text.x      = element_text(size = 6),
      axis.text.y      = element_text(size = 6),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    ggtitle(paste0("Fall thermal habitat suitability for species managed in ", report_long[[epu]])) +
    soe_theme()
}

plot_spaghetti <- function(epu, add_gls = TRUE) {
  
  anom      <- species_long |> filter(EPU == epu, metric == "Anomaly")
  mean_line <- regional_long |> filter(EPU == epu)
  
  p <- ggplot(anom, aes(x = Time, y = Value)) +
    shade_layer(epu, anom$Time) +
    geom_hline(yintercept = 0, colour = "grey40", linetype = "dashed") +
    geom_line(aes(group = species), colour = "grey70", linewidth = 0.3) +
    geom_line(data = mean_line, colour = "black", linewidth = 1.1)
  
  if (add_gls && has_ecodata) {
    p <- p + ecodata::geom_gls(data = mean_line, aes(x = Time, y = Value))
  }
  
  p +
    labs(
      y = "Anomaly (percentage points)",
      title = paste0("Fall thermal habitat anomaly, species managed in ", report_long[[epu]])
    ) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          panel.grid.minor = element_blank()) +
    soe_theme()
}

save_plot <- function(p, file, width, height) {
  ggsave(file.path(dir_soe, file), p, width = width, height = height, dpi = 300, bg = "white")
}

walk(unique(soe$EPU), function(epu) {
  
  n_sp <- n_distinct(species_long$species[species_long$EPU == epu])
  
  for (pt in c("scaled", "raw")) {
    save_plot(plot_heatmap(epu, pt), paste0("draft_heatmap_", pt, "_", epu, ".png"),
              width = 9, height = max(3, 0.2 * n_sp + 1.5))
  }
  
  # Fall back to no trend line if the GLS layer fails
  p_spag <- plot_spaghetti(epu, add_gls = TRUE)
  ok <- tryCatch({ save_plot(p_spag, paste0("draft_spaghetti_", epu, ".png"), 8, 4.5); TRUE },
                 error = function(e) { message("  GLS trend skipped (", conditionMessage(e), ")"); FALSE })
  if (!ok) save_plot(plot_spaghetti(epu, add_gls = FALSE), paste0("draft_spaghetti_", epu, ".png"), 8, 4.5)
})

message("Draft figures saved to: ", dir_soe,
        if (!has_ecodata) " (ecodata not installed: SOE themes and GLS trends not applied)")
message("Script complete.")