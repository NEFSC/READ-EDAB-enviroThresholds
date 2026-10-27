# R/04_plot_risk_score_heatmap.R
#
# Purpose: All-species heatmap of fall thermal habitat risk scores
#          (manuscript Figure 4). The baseline years are outlined.
#          Colour-blind-safe blue-orange palette (risk_palette in utils_pipeline.R).
#
# Output:
#   PNG : images/scoring/risk_score_heatmap_fall.png
#
# Dependencies: tidyverse, here

library(tidyverse)
library(here)

source(here::here("R/utils_config.R"))
source(here::here("R/utils_pipeline.R"))

dir_images <- ensure_dir(paths$images_dir, "scoring")

hindcast_file <- file.path(paths$scoring_dir, "risk_scores_hindcast_fall.rds")
if (!file.exists(hindcast_file)) {
  stop("Scores not found. Run 03_calculate_risk_scores.R first.")
}

scores <- readRDS(hindcast_file) |>
  mutate(
    species_label = fct_rev(factor(species_label(species))),
    score_factor  = factor(risk_score, levels = -4:4),
    score_label   = ifelse(is.na(risk_score), "", risk_score),
    # White text on the darkest tiles, black elsewhere
    text_colour   = ifelse(!is.na(risk_score) & abs(risk_score) >= 3, "white", "black")
  )

baseline_start <- min(params$baseline_years)
baseline_end   <- max(params$baseline_years)
n_species      <- n_distinct(scores$species)
year_range     <- range(scores$year)

message("Drawing heatmap for ", n_species, " species...")

p <- ggplot(scores, aes(year, species_label)) +
  geom_tile(aes(fill = score_factor), colour = "grey80", linewidth = 0.2) +
  geom_text(aes(label = score_label, colour = text_colour), size = 2.6) +
  scale_colour_identity() +
  annotate("rect",
           xmin = baseline_start - 0.5, xmax = baseline_end + 0.5,
           ymin = 0.5, ymax = n_species + 0.5,
           fill = NA, colour = "black", linewidth = 0.6) +
  scale_fill_manual(
    values = risk_palette, drop = FALSE, na.value = "grey90",
    name = "Risk score",
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_x_continuous(position = "top", breaks = seq(1960, 2030, by = 5),
                     limits = year_range + c(-0.5, 0.5), expand = c(0, 0)) +
  labs(
    title = "Fall thermal habitat risk scores",
    subtitle = paste0(
      "Blue (negative) = less risk averse | White (0) = neutral | Orange (positive) = more risk averse. ",
      "Outlined: ", baseline_start, "\u2013", baseline_end, " baseline."
    ),
    caption = "Scores are z-scores of the percentage of suitable fall thermal habitat relative to the fixed baseline, binned at 0.5 SD.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x.top   = element_text(size = 9),
    axis.text.y       = element_text(size = 9, face = "bold"),
    panel.grid        = element_blank(),
    plot.title        = element_text(face = "bold", size = 14),
    plot.subtitle     = element_text(colour = "grey40", size = 10, margin = margin(b = 5)),
    plot.caption      = element_text(colour = "grey50", size = 9, hjust = 0, margin = margin(t = 10)),
    legend.title      = element_text(size = 10, face = "bold"),
    legend.key.height = unit(0.6, "cm")
  )

out_file <- file.path(dir_images, "risk_score_heatmap_fall.png")
ggsave(out_file, p, width = 16, height = max(5, n_species * 0.25 + 2), dpi = 300, bg = "white")

message("Saved: ", out_file)