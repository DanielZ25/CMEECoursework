#!/usr/bin/env Rscript

# Biological reference-category heat map for the VecTraits
# semantic-clustering project.
#
# This script does NOT regenerate embeddings or rerun HDBSCAN. It reads the
# existing 461-row V1-V8 label matrix and calculates, for each manually curated
# trait category, both dominant-cluster share and dominant-cluster purity.
#
# Run from the project root:
#   Rscript Code/multi_trait_heatmap.R
#
# Optional extension:
# Add Results/additional_trait_ground_truth.csv with these required columns:
#   trait_category, OriginalTraitName, OriginalTraitDef
# An optional `include` column may contain TRUE/yes/1/include.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(tidyr)
  library(patchwork)
})

project_root <- normalizePath(getwd(), mustWork = TRUE)
results_dir <- file.path(project_root, "Results")

label_matrix_path <- file.path(results_dir, "featexp_label_matrix.csv")
fecundity_path <- file.path(results_dir, "fecundity_tracking.csv")
body_size_path <- file.path(results_dir, "body_size_tracking.csv")
additional_ground_truth_path <- file.path(
  results_dir,
  "additional_trait_ground_truth.csv"
)

required_files <- c(label_matrix_path, fecundity_path, body_size_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Required input file(s) not found:\n",
    paste0("  - ", missing_files, collapse = "\n"),
    "\nRun this script from the project root."
  )
}

versions <- paste0("V", 1:8)
version_fields <- c(
  V1 = "name",
  V2 = "+ definition",
  V3 = "+ unit",
  V4 = "+ order",
  V5 = "+ family",
  V6 = "+ stage",
  V7 = "+ class",
  V8 = "+ phylum"
)
version_labels <- paste0(versions, "\n", unname(version_fields[versions]))

read_all_character <- function(path) {
  read_csv(
    path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )
}

label_matrix <- read_all_character(label_matrix_path) %>%
  select(OriginalTraitName, OriginalTraitDef, all_of(versions))

if (nrow(label_matrix) != 461) {
  stop(
    "Expected 461 rows in featexp_label_matrix.csv, found ",
    nrow(label_matrix),
    "."
  )
}

if (anyDuplicated(
  label_matrix[c("OriginalTraitName", "OriginalTraitDef")]
) > 0) {
  stop(
    "featexp_label_matrix.csv contains duplicate ",
    "(OriginalTraitName, OriginalTraitDef) pairs."
  )
}

fecundity_ground_truth <- read_all_character(fecundity_path) %>%
  transmute(
    trait_category = "Fecundity",
    OriginalTraitName,
    OriginalTraitDef,
    ground_truth_source = "fecundity_tracking.csv"
  )

body_size_ground_truth <- read_all_character(body_size_path) %>%
  mutate(
    trait_category = case_when(
      measurement_subtype == "body_mass_or_weight" ~ "Body mass / weight",
      measurement_subtype == "linear_morphometric" ~ "Linear morphometrics",
      TRUE ~ NA_character_
    )
  ) %>%
  transmute(
    trait_category,
    OriginalTraitName,
    OriginalTraitDef,
    ground_truth_source = "body_size_tracking.csv"
  )

if (anyNA(body_size_ground_truth$trait_category)) {
  stop("Unrecognised measurement_subtype in body_size_tracking.csv.")
}

ground_truth <- bind_rows(
  fecundity_ground_truth,
  body_size_ground_truth
)

provisional_categories <- character()

if (file.exists(additional_ground_truth_path)) {
  additional <- read_all_character(additional_ground_truth_path)
  required_columns <- c(
    "trait_category",
    "OriginalTraitName",
    "OriginalTraitDef"
  )
  missing_columns <- setdiff(required_columns, names(additional))
  if (length(missing_columns) > 0) {
    stop(
      "additional_trait_ground_truth.csv is missing required column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if ("include" %in% names(additional)) {
    additional <- additional %>%
      filter(
        str_to_lower(str_trim(include)) %in%
          c("true", "yes", "1", "include", "included")
      )
  }

  additional <- additional %>%
    transmute(
      trait_category = str_trim(trait_category),
      OriginalTraitName,
      OriginalTraitDef,
      ground_truth_source = "additional_trait_ground_truth.csv"
    )

  provisional_categories <- sort(unique(additional$trait_category))
  ground_truth <- bind_rows(ground_truth, additional)
}

ground_truth <- ground_truth %>%
  filter(
    !is.na(trait_category),
    trait_category != "",
    !is.na(OriginalTraitName),
    !is.na(OriginalTraitDef)
  ) %>%
  distinct(
    trait_category,
    OriginalTraitName,
    OriginalTraitDef,
    .keep_all = TRUE
  )

missing_members <- ground_truth %>%
  anti_join(
    label_matrix,
    by = c("OriginalTraitName", "OriginalTraitDef")
  )

if (nrow(missing_members) > 0) {
  write_csv(
    missing_members,
    file.path(results_dir, "multi_trait_missing_ground_truth_rows.csv")
  )
  stop(
    nrow(missing_members),
    " ground-truth row(s) were not found in the 461-row label matrix. ",
    "See Results/multi_trait_missing_ground_truth_rows.csv."
  )
}

category_sizes <- ground_truth %>%
  count(trait_category, name = "n_concepts")

small_categories <- category_sizes %>%
  filter(n_concepts < 5)
if (nrow(small_categories) > 0) {
  warning(
    "Trait categories with fewer than five concepts are below the ",
    "HDBSCAN min_cluster_size used in this project: ",
    paste0(
      small_categories$trait_category,
      " (n=",
      small_categories$n_concepts,
      ")",
      collapse = ", "
    )
  )
}

membership <- ground_truth %>%
  left_join(
    label_matrix,
    by = c("OriginalTraitName", "OriginalTraitDef")
  )

membership_long <- membership %>%
  pivot_longer(
    cols = all_of(versions),
    names_to = "version",
    values_to = "cluster_id"
  )

matrix_long <- label_matrix %>%
  pivot_longer(
    cols = all_of(versions),
    names_to = "version",
    values_to = "cluster_id"
  )

cluster_sizes <- matrix_long %>%
  filter(cluster_id != "-1") %>%
  count(version, cluster_id, name = "cluster_total_size")

base_metrics <- membership_long %>%
  group_by(trait_category, version) %>%
  summarise(
    n_concepts = n(),
    n_noise = sum(cluster_id == "-1"),
    n_non_noise = sum(cluster_id != "-1"),
    n_clusters_occupied = n_distinct(cluster_id[cluster_id != "-1"]),
    .groups = "drop"
  )

target_cluster_counts <- membership_long %>%
  filter(cluster_id != "-1") %>%
  count(
    trait_category,
    version,
    cluster_id,
    name = "target_n_in_cluster"
  ) %>%
  left_join(
    cluster_sizes,
    by = c("version", "cluster_id")
  ) %>%
  mutate(
    candidate_purity = target_n_in_cluster / cluster_total_size
  )

dominant_metrics <- target_cluster_counts %>%
  group_by(trait_category, version) %>%
  filter(target_n_in_cluster == max(target_n_in_cluster)) %>%
  arrange(as.integer(cluster_id)) %>%
  summarise(
    dominant_n = first(target_n_in_cluster),
    dominant_cluster_ids = paste(
      sort(as.integer(cluster_id)),
      collapse = ";"
    ),
    representative_dominant_cluster = first(cluster_id),
    dominant_cluster_tie = n() > 1,
    dominant_cluster_purity = mean(candidate_purity),
    dominant_cluster_purity_min = min(candidate_purity),
    dominant_cluster_purity_max = max(candidate_purity),
    .groups = "drop"
  )

metrics <- base_metrics %>%
  left_join(
    dominant_metrics,
    by = c("trait_category", "version")
  ) %>%
  mutate(
    dominant_n = coalesce(dominant_n, 0L),
    dominant_share = dominant_n / n_concepts,
    non_noise_share = n_non_noise / n_concepts
  ) %>%
  mutate(
    version_number = as.integer(str_remove(version, "^V")),
    field_added = unname(version_fields[version])
  ) %>%
  arrange(trait_category, version_number)

best_versions <- metrics %>%
  group_by(trait_category) %>%
  summarise(
    n_concepts = first(n_concepts),
    maximum_dominant_share = max(dominant_share),
    best_versions = paste(
      version[dominant_share == maximum_dominant_share],
      collapse = ";"
    ),
    .groups = "drop"
  )

version_summary <- metrics %>%
  group_by(trait_category) %>%
  mutate(
    is_trait_best = dominant_share == max(dominant_share)
  ) %>%
  ungroup() %>%
  group_by(version, version_number, field_added) %>%
  summarise(
    n_trait_categories = n(),
    mean_dominant_share = mean(dominant_share),
    median_dominant_share = median(dominant_share),
    mean_dominant_cluster_purity = mean(
      dominant_cluster_purity,
      na.rm = TRUE
    ),
    mean_non_noise_share = mean(non_noise_share),
    n_categories_at_best = sum(is_trait_best),
    .groups = "drop"
  ) %>%
  arrange(version_number)

scores_output_path <- file.path(
  results_dir,
  "multi_trait_heatmap_scores.csv"
)
best_output_path <- file.path(
  results_dir,
  "multi_trait_best_versions.csv"
)
version_summary_output_path <- file.path(
  results_dir,
  "multi_trait_version_summary.csv"
)
membership_output_path <- file.path(
  results_dir,
  "multi_trait_ground_truth_used.csv"
)

write_csv(metrics, scores_output_path, na = "")
write_csv(best_versions, best_output_path, na = "")
write_csv(version_summary, version_summary_output_path, na = "")
write_csv(
  ground_truth %>%
    arrange(trait_category, OriginalTraitName, OriginalTraitDef),
  membership_output_path,
  na = ""
)

preferred_category_order <- c(
  "Fecundity",
  "Body mass / weight",
  "Linear morphometrics",
  "Development time",
  "Development rate",
  "Longevity / lifespan",
  "Survival proportion",
  "Pre-oviposition interval",
  "Egg-hatching success",
  "Egg incubation period"
)
additional_categories <- setdiff(
  sort(unique(metrics$trait_category)),
  preferred_category_order
)
category_order <- c(
  preferred_category_order[
    preferred_category_order %in% unique(metrics$trait_category)
  ],
  additional_categories
)

plot_data <- metrics %>%
  left_join(category_sizes, by = "trait_category", suffix = c("", "_check")) %>%
  mutate(
    version_label = factor(
      version,
      levels = versions,
      labels = version_labels
    ),
    trait_label = paste0(
      trait_category,
      " (n=",
      n_concepts,
      ")"
    ),
    trait_label = factor(
      trait_label,
      levels = rev(
        paste0(
          category_order,
          " (n=",
          category_sizes$n_concepts[
            match(category_order, category_sizes$trait_category)
          ],
          ")"
        )
      )
    )
  ) %>%
  select(
    trait_category,
    trait_label,
    version,
    version_label,
    dominant_share,
    dominant_cluster_purity
  ) %>%
  pivot_longer(
    cols = c(dominant_share, dominant_cluster_purity),
    names_to = "score_type",
    values_to = "score"
  ) %>%
  mutate(
    score_type = factor(
      score_type,
      levels = c("dominant_share", "dominant_cluster_purity"),
      labels = c(
        "A. Dominant-cluster share",
        "B. Dominant-cluster purity"
      )
    ),
    score_label = if_else(
      is.na(score),
      "NA",
      sprintf("%d%%", round(100 * score))
    ),
    label_colour = if_else(
      is.na(score) | score <= 0.25 | score >= 0.78,
      "white",
      "black"
    )
  )

caption_text <- paste(
  "Same HDBSCAN settings in every version;",
  "cluster IDs are version-specific.",
  "\nAll categories were author-defined; independent expert review is pending."
)

heatmap_plot <- ggplot(
  plot_data,
  aes(x = version_label, y = trait_label, fill = score)
) +
  geom_tile(colour = "white", linewidth = 1.1) +
  geom_text(
    aes(label = score_label, colour = label_colour),
    size = 4.4,
    fontface = "bold"
  ) +
  scale_colour_identity() +
  scale_fill_gradientn(
    colours = c("#b2182b", "#ef8a62", "#f7f7a8", "#91cf60", "#1a9850"),
    values = c(0, 0.25, 0.5, 0.75, 1),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = paste0(seq(0, 100, by = 25), "%"),
    name = "Cell value"
  ) +
  facet_grid(score_type ~ ., switch = "y") +
  labs(
    title = "Biological reference-category evaluation across metadata versions",
    subtitle = "Share measures concentration; purity measures the specificity of the dominant cluster",
    x = NULL,
    y = NULL,
    caption = caption_text
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      colour = "#222222",
      face = "bold",
      lineheight = 0.95,
      margin = margin(t = 7)
    ),
    axis.text.y = element_text(
      colour = "#222222",
      face = "bold",
      margin = margin(r = 8)
    ),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(
      angle = 0,
      face = "bold",
      colour = "#222222",
      margin = margin(r = 8)
    ),
    plot.title = element_text(
      face = "bold",
      size = 16,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      colour = "#444444",
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      colour = "#555555",
      hjust = 0,
      margin = margin(t = 10)
    ),
    legend.position = "top",
    legend.justification = "left",
    legend.key.width = grid::unit(2.3, "cm"),
    legend.key.height = grid::unit(0.45, "cm"),
    plot.margin = margin(14, 18, 12, 14)
  )

n_categories <- n_distinct(plot_data$trait_category)
plot_height <- max(8.5, 3.5 + 0.82 * n_categories)
png_output_path <- file.path(
  results_dir,
  "multi_trait_heatmap.png"
)
pdf_output_path <- file.path(
  results_dir,
  "multi_trait_heatmap.pdf"
)

ggsave(
  png_output_path,
  heatmap_plot,
  width = 12,
  height = plot_height,
  units = "in",
  dpi = 300,
  bg = "white"
)
ggsave(
  pdf_output_path,
  heatmap_plot,
  width = 12,
  height = plot_height,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

cat("\nHeat-map analysis completed.\n")
cat("Ground-truth categories:", n_categories, "\n")
print(category_sizes %>% arrange(desc(n_concepts)))
cat("\nBest version(s) by trait:\n")
print(best_versions)
cat("\nCreated:\n")
cat("  ", scores_output_path, "\n", sep = "")
cat("  ", best_output_path, "\n", sep = "")
cat("  ", version_summary_output_path, "\n", sep = "")
cat("  ", membership_output_path, "\n", sep = "")
cat("  ", png_output_path, "\n", sep = "")
cat("  ", pdf_output_path, "\n", sep = "")
