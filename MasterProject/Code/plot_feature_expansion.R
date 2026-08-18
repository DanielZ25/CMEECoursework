#!/usr/bin/env Rscript

# Recreate the cumulative feature-expansion figure from saved summary data.
# This script does not call the embedding API or rerun clustering.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(tidyr)
})

project_root <- normalizePath(getwd(), mustWork = TRUE)
input_path <- file.path(project_root, "Results", "featexp_rarefaction.csv")

if (!file.exists(input_path)) {
  stop("Missing saved feature-expansion summary: ", input_path)
}

feature_expansion <- read_csv(input_path, show_col_types = FALSE)

plot_df <- feature_expansion %>%
  select(k_fields, last_field_added, n_clusters, noise_rate, avg_cluster_size) %>%
  pivot_longer(
    c(n_clusters, noise_rate, avg_cluster_size),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(
      recode(
        metric,
        n_clusters = "Number of non-noise clusters",
        noise_rate = "Noise rate",
        avg_cluster_size = "Average cluster size"
      ),
      levels = c(
        "Average cluster size",
        "Number of non-noise clusters",
        "Noise rate"
      )
    ),
    value_label = ifelse(
      metric == "Noise rate",
      sprintf("%.3f", value),
      sprintf("%.1f", value)
    )
  )

plot <- ggplot(plot_df, aes(k_fields, value)) +
  geom_line(linewidth = 0.8, colour = "steelblue") +
  geom_point(size = 1.8, colour = "steelblue") +
  geom_text(
    aes(label = value_label),
    vjust = -0.9,
    size = 2.5,
    colour = "grey25"
  ) +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  scale_x_continuous(
    breaks = feature_expansion$k_fields,
    labels = paste0(
      "V",
      feature_expansion$k_fields,
      "\n+",
      feature_expansion$last_field_added
    ),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.15))) +
  labs(
    x = "Cumulative input fields",
    y = NULL,
    title = "Cumulative Feature Expansion (VecTraits, 461 concepts)"
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(size = 6))

ggsave(
  file.path(project_root, "Results", "featexp_rarefaction_curve.png"),
  plot,
  width = 9,
  height = 9,
  dpi = 300,
  bg = "white"
)
ggsave(
  file.path(project_root, "Results", "featexp_rarefaction_curve.pdf"),
  plot,
  width = 9,
  height = 9,
  bg = "white"
)

cat("Recreated cumulative feature-expansion figure from saved results.\n")
