#!/usr/bin/env Rscript

# Build seven additional, auditable trait ground truths from the canonical
# 461-row feature-expansion label matrix.
#
# The rules below use exact, manually reviewed trait-name families. Definitions
# were inspected before the rules were fixed. Name-matched rows with inconsistent
# definitions are deliberately retained, following the same principle used for
# the fecundity ground truth: curation errors are part of the database problem
# that the clustering is intended to reveal.
#
# This script does NOT regenerate embeddings or rerun HDBSCAN.
#
# Run from the project root:
#   Rscript Code/build_additional_ground_truth.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

project_root <- normalizePath(getwd(), mustWork = TRUE)
results_dir <- file.path(project_root, "Results")
label_matrix_path <- file.path(results_dir, "featexp_label_matrix.csv")
source_data_path <- file.path(project_root, "Data", "VecTraits.csv")

required_files <- c(label_matrix_path, source_data_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Required input file(s) not found:\n",
    paste0("  - ", missing_files, collapse = "\n"),
    "\nRun this script from the project root."
  )
}

label_matrix <- read_csv(
  label_matrix_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  progress = FALSE
) %>%
  select(OriginalTraitName, OriginalTraitDef) %>%
  mutate(
    trait_name_normalised = str_to_lower(str_trim(OriginalTraitName))
  )

unit_lookup <- read_csv(
  source_data_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  progress = FALSE
) %>%
  select(
    OriginalTraitName,
    OriginalTraitDef,
    OriginalTraitUnit
  ) %>%
  mutate(
    OriginalTraitUnit = str_trim(OriginalTraitUnit)
  ) %>%
  group_by(OriginalTraitName, OriginalTraitDef) %>%
  summarise(
    OriginalTraitUnits = {
      units <- sort(
        unique(
          OriginalTraitUnit[
            !is.na(OriginalTraitUnit) &
              OriginalTraitUnit != ""
          ]
        )
      )
      paste(units, collapse = "; ")
    },
    n_distinct_units = n_distinct(
      OriginalTraitUnit[
        !is.na(OriginalTraitUnit) &
          OriginalTraitUnit != ""
      ]
    ),
    .groups = "drop"
  )

if (nrow(label_matrix) != 461) {
  stop(
    "Expected 461 rows in featexp_label_matrix.csv, found ",
    nrow(label_matrix),
    "."
  )
}

if (
  anyDuplicated(
    label_matrix[c("OriginalTraitName", "OriginalTraitDef")]
  ) > 0
) {
  stop(
    "featexp_label_matrix.csv contains duplicate ",
    "(OriginalTraitName, OriginalTraitDef) pairs."
  )
}

trait_name_rules <- list(
  "Development time" = c(
    "development time",
    "developmental time",
    "development duration"
  ),
  "Development rate" = c(
    "development rate",
    "development rate (egg)",
    "development rate (larva)",
    "development rate (nymph)"
  ),
  "Longevity / lifespan" = c(
    "longevity",
    "adult longevity",
    "aphid longevity",
    "lifespan",
    "juvenile life span",
    "longevity (alate)",
    "longevity (apterous)",
    "survival time"
  ),
  "Survival proportion" = c(
    "survival",
    "survival rate"
  ),
  "Pre-oviposition interval" = c(
    "preoviposition period",
    "pre-oviposition period",
    "pre-reproductive period",
    "age at first reproduction"
  ),
  "Egg-hatching success" = c(
    "hatchability",
    "hatch rate",
    "hatching rate",
    "hatch ratio",
    "larval hatch rate",
    "egg hatch",
    "egg viability"
  ),
  "Egg incubation period" = c(
    "incubation period"
  )
)

expected_counts <- c(
  "Development time" = 25L,
  "Development rate" = 13L,
  "Longevity / lifespan" = 34L,
  "Survival proportion" = 24L,
  "Pre-oviposition interval" = 21L,
  "Egg-hatching success" = 13L,
  "Egg incubation period" = 9L
)

ground_truth_parts <- lapply(
  names(trait_name_rules),
  function(category_name) {
    accepted_names <- trait_name_rules[[category_name]]

    label_matrix %>%
      filter(trait_name_normalised %in% accepted_names) %>%
      transmute(
        trait_category = category_name,
        OriginalTraitName,
        OriginalTraitDef,
        include = TRUE,
        inclusion_basis = if_else(
          trait_name_normalised == accepted_names[[1]],
          "exact target name",
          "explicit name variant or semantic synonym"
        ),
        audit_status = "provisional; expert review pending",
        audit_note = paste(
          "Name family and definitions manually inspected;",
          "name-matched curation inconsistencies retained."
        )
      )
  }
)

additional_ground_truth <- bind_rows(ground_truth_parts) %>%
  distinct(
    trait_category,
    OriginalTraitName,
    OriginalTraitDef,
    .keep_all = TRUE
  ) %>%
  arrange(
    factor(
      trait_category,
      levels = names(trait_name_rules)
    ),
    OriginalTraitName,
    OriginalTraitDef
  ) %>%
  left_join(
    unit_lookup,
    by = c("OriginalTraitName", "OriginalTraitDef")
  ) %>%
  relocate(
    OriginalTraitUnits,
    n_distinct_units,
    .after = OriginalTraitDef
  )

overlap_check <- additional_ground_truth %>%
  count(
    OriginalTraitName,
    OriginalTraitDef,
    name = "n_categories"
  ) %>%
  filter(n_categories > 1)

if (nrow(overlap_check) > 0) {
  stop(
    nrow(overlap_check),
    " concept(s) were assigned to more than one additional category."
  )
}

summary_table <- additional_ground_truth %>%
  count(trait_category, name = "n_concepts") %>%
  mutate(
    expected_n = unname(expected_counts[trait_category]),
    count_matches_expectation = n_concepts == expected_n
  ) %>%
  arrange(
    factor(
      trait_category,
      levels = names(trait_name_rules)
    )
  )

if (
  any(is.na(summary_table$expected_n)) ||
  nrow(summary_table) != length(expected_counts) ||
  !all(summary_table$count_matches_expectation)
) {
  print(summary_table)
  stop(
    "Ground-truth counts have changed from the manually audited values. ",
    "Review the label matrix and update the rules before continuing."
  )
}

ground_truth_output_path <- file.path(
  results_dir,
  "additional_trait_ground_truth.csv"
)
summary_output_path <- file.path(
  results_dir,
  "additional_trait_ground_truth_summary.csv"
)

write_csv(
  additional_ground_truth,
  ground_truth_output_path,
  na = ""
)
write_csv(
  summary_table,
  summary_output_path,
  na = ""
)

cat("\nAdditional ground truths created and validated.\n")
print(summary_table)
cat("\nTotal additional concepts:", nrow(additional_ground_truth), "\n")
cat("No concept appears in more than one new category.\n")
cat("\nCreated:\n")
cat("  ", ground_truth_output_path, "\n", sep = "")
cat("  ", summary_output_path, "\n", sep = "")
