#!/usr/bin/env Rscript

# Re-evaluate field-order permutations with manually curated reference
# categories rather than silhouette as the ranking criterion.
#
# Default run (no API calls):
#   Rscript Code/groundtruth_permutation_analysis.R
#
# Also evaluate the 3! orders of name/definition/unit. Five new embeddings may
# be required; OPENAI_API_KEY must be set in the environment:
#   Rscript Code/groundtruth_permutation_analysis.R --include-v3-orders

suppressPackageStartupMessages({
  library(digest)
  library(dplyr)
  library(gtools)
  library(readr)
  library(reticulate)
  library(stringr)
  library(tidyr)
})

project_root <- normalizePath(getwd(), mustWork = TRUE)
results_dir <- file.path(project_root, "Results")
include_v3_orders <- "--include-v3-orders" %in% commandArgs(trailingOnly = TRUE)

required_files <- c(
  file.path(results_dir, "ordering_720_full.csv"),
  file.path(results_dir, "multi_trait_ground_truth_used.csv"),
  file.path(results_dir, "multi_trait_version_summary.csv"),
  file.path(results_dir, "multi_trait_heatmap_scores.csv"),
  file.path(results_dir, "featexp_label_matrix.csv"),
  file.path(project_root, "Data", "VecTraits.csv")
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required file(s):\n", paste(missing_files, collapse = "\n"))
}

use_condaenv("pymc-env", required = TRUE)
hdbscan_pkg <- import("hdbscan")

read_char_csv <- function(path) {
  read_csv(
    path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )
}

raw <- read_char_csv(file.path(project_root, "Data", "VecTraits.csv"))

six_columns <- c(
  "OriginalTraitName", "OriginalTraitDef", "OriginalTraitUnit",
  "Interactor1Order", "Interactor1Family", "Interactor1Stage"
)

base <- raw %>%
  mutate(across(all_of(six_columns), ~ str_trim(as.character(.)))) %>%
  distinct(OriginalTraitName, OriginalTraitDef, .keep_all = TRUE) %>%
  select(all_of(six_columns)) %>%
  mutate(row_id = row_number())

if (nrow(base) != 461) {
  stop("Expected 461 unique name-definition pairs; found ", nrow(base), ".")
}
if (anyDuplicated(base[c("OriginalTraitName", "OriginalTraitDef")]) > 0) {
  stop("The 461-row base contains duplicate name-definition pairs.")
}

label_matrix <- read_char_csv(
  file.path(results_dir, "featexp_label_matrix.csv")
)
if (nrow(label_matrix) != 461) {
  stop("featexp_label_matrix.csv does not contain 461 rows.")
}
if (!identical(base$OriginalTraitName, label_matrix$OriginalTraitName) ||
    !identical(base$OriginalTraitDef, label_matrix$OriginalTraitDef)) {
  stop("Row order does not match featexp_label_matrix.csv.")
}

reference <- read_char_csv(
  file.path(results_dir, "multi_trait_ground_truth_used.csv")
) %>%
  select(trait_category, OriginalTraitName, OriginalTraitDef) %>%
  distinct()

if (n_distinct(reference$trait_category) != 10) {
  stop(
    "Expected 10 reference categories; found ",
    n_distinct(reference$trait_category),
    "."
  )
}
if (anyDuplicated(
  reference[c("trait_category", "OriginalTraitName", "OriginalTraitDef")]
) > 0) {
  stop("Duplicate concept within a reference category.")
}

reference_indexed <- reference %>%
  left_join(
    base %>% select(row_id, OriginalTraitName, OriginalTraitDef),
    by = c("OriginalTraitName", "OriginalTraitDef")
  )
if (anyNA(reference_indexed$row_id)) {
  stop("At least one reference concept is absent from the 461-row base.")
}

category_indices <- split(
  reference_indexed$row_id,
  reference_indexed$trait_category
)
category_indices <- lapply(category_indices, as.integer)

run_hdbscan <- function(emb) {
  if (is.null(dim(emb)) || nrow(emb) != 461 || ncol(emb) != 3072) {
    stop("Embedding matrix must be 461 x 3072.")
  }
  clusterer <- hdbscan_pkg$HDBSCAN(
    min_cluster_size = 5L,
    min_samples = 3L,
    metric = "euclidean"
  )
  as.integer(clusterer$fit_predict(emb))
}

harmonic_score <- function(share, purity) {
  if (is.na(purity) || share + purity == 0) return(NA_real_)
  2 * share * purity / (share + purity)
}

score_labels <- function(labels, analysis_id, order_string, perm_id = NA_integer_) {
  if (length(labels) != 461) stop("Labels must have length 461.")

  non_noise_sizes <- table(labels[labels != -1L])
  rows <- lapply(names(category_indices), function(category) {
    idx <- category_indices[[category]]
    category_labels <- labels[idx]
    valid <- category_labels[category_labels != -1L]
    n_concepts <- length(idx)
    n_noise <- sum(category_labels == -1L)

    if (length(valid) == 0) {
      return(tibble(
        analysis_id = analysis_id,
        perm_id = perm_id,
        order_string = order_string,
        trait_category = category,
        n_concepts = n_concepts,
        n_noise = n_noise,
        n_clusters_occupied = 0L,
        dominant_n = 0L,
        dominant_cluster_ids = NA_character_,
        dominant_cluster_tie = FALSE,
        dominant_share = 0,
        non_noise_share = 0,
        dominant_purity_min = NA_real_,
        dominant_purity_mean = NA_real_,
        dominant_purity_max = NA_real_,
        balanced_score = NA_real_
      ))
    }

    target_counts <- table(valid)
    dominant_n <- max(as.integer(target_counts))
    tied_ids <- names(target_counts)[as.integer(target_counts) == dominant_n]
    tied_total_sizes <- as.integer(non_noise_sizes[tied_ids])
    tied_purities <- dominant_n / tied_total_sizes
    dominant_share <- dominant_n / n_concepts
    # Average purity across co-dominant clusters so the result does not depend
    # on an arbitrary cluster ID or only the most favourable tied cluster.
    purity_for_balance <- mean(tied_purities)

    tibble(
      analysis_id = analysis_id,
      perm_id = perm_id,
      order_string = order_string,
      trait_category = category,
      n_concepts = n_concepts,
      n_noise = n_noise,
      n_clusters_occupied = n_distinct(valid),
      dominant_n = dominant_n,
      dominant_cluster_ids = paste(sort(as.integer(tied_ids)), collapse = ";"),
      dominant_cluster_tie = length(tied_ids) > 1,
      dominant_share = dominant_share,
      non_noise_share = length(valid) / n_concepts,
      dominant_purity_min = min(tied_purities),
      dominant_purity_mean = mean(tied_purities),
      dominant_purity_max = max(tied_purities),
      balanced_score = harmonic_score(dominant_share, purity_for_balance)
    )
  })

  bind_rows(rows)
}

summarise_order <- function(category_scores, labels) {
  non_noise <- labels[labels != -1L]
  cluster_sizes <- as.integer(table(non_noise))

  category_scores %>%
    summarise(
      n_categories = n(),
      mean_dominant_share = mean(dominant_share),
      median_dominant_share = median(dominant_share),
      min_dominant_share = min(dominant_share),
      # A category with no non-noise member has no usable dominant cluster.
      # Count its purity/balanced score as zero instead of dropping the
      # category from the macro-average.
      mean_dominant_purity = mean(coalesce(dominant_purity_mean, 0)),
      median_dominant_purity = median(coalesce(dominant_purity_mean, 0)),
      mean_balanced_score = mean(coalesce(balanced_score, 0)),
      mean_non_noise_share = mean(non_noise_share),
      total_noise_rate = mean(labels == -1L),
      n_clusters = n_distinct(non_noise),
      max_cluster_share = if (length(cluster_sizes) > 0) {
        max(cluster_sizes) / length(labels)
      } else {
        0
      },
      n_dominant_cluster_ties = sum(dominant_cluster_tie)
    )
}

# -------------------------------------------------------------------------
# A. Existing 720 permutations of the fixed six-field input
# -------------------------------------------------------------------------

six_blocks <- list(
  name = paste0("Trait: ", base$OriginalTraitName),
  def = paste0("Definition: ", base$OriginalTraitDef),
  unit = paste0("Unit: ", base$OriginalTraitUnit),
  order = paste0("Organism order: ", base$Interactor1Order),
  family = paste0("Organism family: ", base$Interactor1Family),
  stage = paste0(
    "Life stage: ",
    ifelse(
      is.na(base$Interactor1Stage) | base$Interactor1Stage == "",
      "not specified",
      base$Interactor1Stage
    )
  )
)

assemble_six_text <- function(order_vec) {
  paste0(do.call(paste, c(six_blocks[order_vec], sep = ". ")), ".")
}

ordering <- read_csv(
  file.path(results_dir, "ordering_720_full.csv"),
  show_col_types = FALSE,
  progress = FALSE
) %>%
  arrange(perm_id)

if (nrow(ordering) != 720 || n_distinct(ordering$perm_id) != 720) {
  stop("ordering_720_full.csv must contain 720 unique permutations.")
}

labels_file_720 <- file.path(results_dir, "ordering_720_groundtruth_labels.rds")
if (file.exists(labels_file_720)) {
  saved_labels <- readRDS(labels_file_720)
  labels_720 <- saved_labels$labels
  if (!all(dim(labels_720) == c(461, 720)) ||
      !identical(colnames(labels_720), paste0("P", ordering$perm_id))) {
    stop("Existing six-field label cache is not aligned with the 720 permutations.")
  }
  reuse_labels_720 <- TRUE
} else {
  labels_720 <- matrix(
    NA_integer_,
    nrow = 461,
    ncol = 720,
    dimnames = list(NULL, paste0("P", ordering$perm_id))
  )
  reuse_labels_720 <- FALSE
}
category_results_720 <- vector("list", 720)
summary_results_720 <- vector("list", 720)

cat("Scoring 720 cached six-field permutations",
    if (reuse_labels_720) " from saved labels...\n" else "...\n", sep = "")
for (i in seq_len(nrow(ordering))) {
  order_vec <- str_split(ordering$order_string[i], ">", simplify = TRUE)
  order_vec <- as.character(order_vec[1, ])
  if (!setequal(order_vec, names(six_blocks)) || length(order_vec) != 6) {
    stop("Invalid order_string at row ", i, ".")
  }

  if (reuse_labels_720) {
    labels <- labels_720[, i]
  } else {
    texts <- assemble_six_text(order_vec)
    cache_key <- digest(texts, algo = "md5")
    cache_file <- file.path(
      results_dir,
      "ordering_cache",
      paste0(cache_key, ".rds")
    )
    if (!file.exists(cache_file)) {
      stop("Missing six-field embedding cache: ", cache_file)
    }

    emb <- readRDS(cache_file)
    labels <- run_hdbscan(emb)
    labels_720[, i] <- labels
  }

  category_scores <- score_labels(
    labels,
    analysis_id = "six_field_720",
    order_string = ordering$order_string[i],
    perm_id = ordering$perm_id[i]
  )
  category_results_720[[i]] <- category_scores
  summary_results_720[[i]] <- summarise_order(category_scores, labels) %>%
    mutate(
      perm_id = ordering$perm_id[i],
      order_string = ordering$order_string[i]
    )

  if (i %% 50 == 0 || i == 720) {
    cat("  ", i, "/720\n", sep = "")
  }
}

category_scores_720 <- bind_rows(category_results_720)
summary_720 <- bind_rows(summary_results_720) %>%
  left_join(
    ordering %>%
      select(
        perm_id, anchor, silhouette, ARI_vs_original,
        n_clusters_original = n_clusters,
        noise_rate_original = noise_rate,
        max_cluster_share_original = max_cluster_share,
        degenerate_original = degenerate
      ),
    by = "perm_id"
  ) %>%
  mutate(
    groundtruth_rank = min_rank(desc(mean_dominant_share)),
    silhouette_rank = min_rank(desc(silhouette)),
    degenerate_recomputed = max_cluster_share > 0.5
  ) %>%
  mutate(
    screened_groundtruth_rank = if_else(
      degenerate_recomputed,
      NA_integer_,
      min_rank(if_else(degenerate_recomputed, NA_real_,
                       desc(mean_dominant_share)))
    )
  ) %>%
  arrange(degenerate_recomputed, screened_groundtruth_rank,
          desc(mean_dominant_purity), total_noise_rate)

if (max(abs(summary_720$total_noise_rate - summary_720$noise_rate_original)) > 1e-4) {
  stop("Recomputed six-field noise rates do not match ordering_720_full.csv.")
}

field_position <- summary_720 %>%
  filter(!degenerate_recomputed) %>%
  select(perm_id, order_string, mean_dominant_share, mean_dominant_purity,
         mean_balanced_score, total_noise_rate) %>%
  separate_wider_delim(
    order_string,
    delim = ">",
    names = paste0("position_", 1:6)
  ) %>%
  pivot_longer(
    starts_with("position_"),
    names_to = "position",
    names_prefix = "position_",
    values_to = "field"
  ) %>%
  mutate(position = as.integer(position)) %>%
  group_by(field, position) %>%
  summarise(
    n_permutations = n(),
    mean_of_mean_dominant_share = mean(mean_dominant_share),
    sd_of_mean_dominant_share = sd(mean_dominant_share),
    mean_of_mean_purity = mean(mean_dominant_purity),
    mean_of_balanced_score = mean(mean_balanced_score),
    mean_noise_rate = mean(total_noise_rate),
    .groups = "drop"
  ) %>%
  arrange(field, position)

saveRDS(
  list(
    base_keys = base %>% select(row_id, OriginalTraitName, OriginalTraitDef),
    ordering = ordering %>% select(perm_id, order_string),
    labels = labels_720
  ),
  file.path(results_dir, "ordering_720_groundtruth_labels.rds")
)
write_csv(
  category_scores_720,
  file.path(results_dir, "ordering_720_groundtruth_category_scores.csv")
)
write_csv(
  summary_720,
  file.path(results_dir, "ordering_720_groundtruth_summary.csv")
)
write_csv(
  summary_720 %>% filter(!degenerate_recomputed) %>% head(20),
  file.path(results_dir, "ordering_720_groundtruth_top20.csv")
)
write_csv(
  field_position,
  file.path(results_dir, "ordering_720_field_position_effects.csv")
)

v3_reference <- read_csv(
  file.path(results_dir, "multi_trait_version_summary.csv"),
  show_col_types = FALSE
) %>%
  filter(version == "V3")
if (nrow(v3_reference) != 1) stop("Could not identify V3 reference score.")

v3_category_reference <- read_csv(
  file.path(results_dir, "multi_trait_heatmap_scores.csv"),
  show_col_types = FALSE
) %>%
  filter(version == "V3") %>%
  mutate(
    purity_for_balance = coalesce(dominant_cluster_purity, 0),
    balanced_score = if_else(
      dominant_share + purity_for_balance == 0,
      0,
      2 * dominant_share * purity_for_balance /
        (dominant_share + purity_for_balance)
    )
  )
if (nrow(v3_category_reference) != length(category_indices)) {
  stop("V3 heat-map scores do not contain all reference categories.")
}
v3_mean_balanced <- mean(v3_category_reference$balanced_score)

spearman_sil_share <- suppressWarnings(
  cor(summary_720$silhouette, summary_720$mean_dominant_share,
      method = "spearman", use = "complete.obs")
)
pearson_sil_share <- suppressWarnings(
  cor(summary_720$silhouette, summary_720$mean_dominant_share,
      method = "pearson", use = "complete.obs")
)

top_groundtruth_raw <- summary_720 %>%
  arrange(groundtruth_rank, desc(mean_dominant_purity), total_noise_rate) %>%
  slice(1)
top_groundtruth <- summary_720 %>%
  filter(!degenerate_recomputed) %>%
  arrange(screened_groundtruth_rank, desc(mean_dominant_purity),
          total_noise_rate) %>%
  slice(1)
top_silhouette <- summary_720 %>% arrange(silhouette_rank) %>% slice(1)
original_order <- summary_720 %>% filter(anchor == "original")
top_balanced <- summary_720 %>%
  filter(!degenerate_recomputed) %>%
  arrange(desc(mean_balanced_score), desc(mean_dominant_share),
          total_noise_rate) %>%
  slice(1)

diagnostic_lines <- c(
  "GROUND-TRUTH RE-RANKING OF 720 SIX-FIELD PERMUTATIONS",
  paste0("Rows: ", nrow(base), "; reference categories: ",
         length(category_indices), "; reference concepts: ", nrow(reference)),
  paste0("Spearman correlation: silhouette vs mean dominant share = ",
         round(spearman_sil_share, 4)),
  paste0("Pearson correlation: silhouette vs mean dominant share = ",
         round(pearson_sil_share, 4)),
  paste0("V3 fixed-order mean dominant share = ",
         round(v3_reference$mean_dominant_share, 4)),
  paste0("V3 fixed-order mean dominant purity = ",
         round(v3_reference$mean_dominant_cluster_purity, 4)),
  paste0("V3 fixed-order mean share-purity harmonic score = ",
         round(v3_mean_balanced, 4)),
  paste0("Six-field permutations meeting/exceeding V3 (unscreened) = ",
         sum(summary_720$mean_dominant_share >= v3_reference$mean_dominant_share),
         "/720"),
  paste0("Degenerate permutations (largest cluster >50% of all rows) = ",
         sum(summary_720$degenerate_recomputed), "/720"),
  paste0("Non-degenerate permutations meeting/exceeding V3 share = ",
         sum(!summary_720$degenerate_recomputed &
               summary_720$mean_dominant_share >=
               v3_reference$mean_dominant_share),
         "/", sum(!summary_720$degenerate_recomputed)),
  "",
  "Unscreened top proportion result (invalid as a semantic optimum):",
  paste0("  ", top_groundtruth_raw$order_string),
  paste0("  mean dominant share = ",
         round(top_groundtruth_raw$mean_dominant_share, 4)),
  paste0("  mean dominant purity = ",
         round(top_groundtruth_raw$mean_dominant_purity, 4)),
  paste0("  largest cluster share = ",
         round(top_groundtruth_raw$max_cluster_share, 4)),
  "",
  "Top non-degenerate ground-truth-ranked six-field order:",
  paste0("  ", top_groundtruth$order_string),
  paste0("  mean dominant share = ",
         round(top_groundtruth$mean_dominant_share, 4)),
  paste0("  mean dominant purity = ",
         round(top_groundtruth$mean_dominant_purity, 4)),
  paste0("  largest cluster share = ",
         round(top_groundtruth$max_cluster_share, 4)),
  paste0("  silhouette = ", round(top_groundtruth$silhouette, 4),
         " (silhouette rank ", top_groundtruth$silhouette_rank, ")"),
  "",
  "Top non-degenerate six-field order by mean share-purity harmonic score:",
  paste0("  ", top_balanced$order_string),
  paste0("  mean dominant share = ",
         round(top_balanced$mean_dominant_share, 4)),
  paste0("  mean dominant purity = ",
         round(top_balanced$mean_dominant_purity, 4)),
  paste0("  mean harmonic score = ",
         round(top_balanced$mean_balanced_score, 4)),
  "",
  "Top silhouette-ranked six-field order:",
  paste0("  ", top_silhouette$order_string),
  paste0("  silhouette = ", round(top_silhouette$silhouette, 4)),
  paste0("  mean dominant share = ",
         round(top_silhouette$mean_dominant_share, 4),
         " (ground-truth rank ", top_silhouette$groundtruth_rank, ")"),
  "",
  "Original six-field order:",
  paste0("  ", original_order$order_string),
  paste0("  mean dominant share = ",
         round(original_order$mean_dominant_share, 4),
         " (screened rank ", original_order$screened_groundtruth_rank,
         "/", sum(!summary_720$degenerate_recomputed), ")"),
  paste0("  silhouette = ", round(original_order$silhouette, 4),
         " (silhouette rank ", original_order$silhouette_rank, ")")
)
writeLines(
  diagnostic_lines,
  file.path(results_dir, "ordering_720_groundtruth_diagnostics.txt")
)
cat(paste(diagnostic_lines, collapse = "\n"), "\n")

# -------------------------------------------------------------------------
# B. Optional 3! permutations of the V3 semantic field set
# -------------------------------------------------------------------------

if (include_v3_orders) {
  v3_cache_dir <- file.path(results_dir, "v3_order_cache")
  dir.create(v3_cache_dir, showWarnings = FALSE)

  v3_blocks <- list(
    name = paste0(
      "Trait: ",
      ifelse(
        is.na(base$OriginalTraitName) | base$OriginalTraitName == "",
        "not specified",
        base$OriginalTraitName
      )
    ),
    def = paste0(
      "Definition: ",
      ifelse(
        is.na(base$OriginalTraitDef) | base$OriginalTraitDef == "",
        "not specified",
        base$OriginalTraitDef
      )
    ),
    unit = paste0(
      "Unit: ",
      ifelse(
        is.na(base$OriginalTraitUnit) | base$OriginalTraitUnit == "",
        "not specified",
        base$OriginalTraitUnit
      )
    )
  )

  assemble_v3_text <- function(order_vec) {
    paste0(do.call(paste, c(v3_blocks[order_vec], sep = ". ")), ".")
  }

  original_v3_texts <- assemble_v3_text(c("name", "def", "unit"))
  original_v3_key <- digest(list("V3", original_v3_texts), algo = "md5")
  original_v3_file <- file.path(
    results_dir,
    "featexp_cache",
    paste0(original_v3_key, ".rds")
  )
  if (!file.exists(original_v3_file)) {
    stop("Exact original V3 embedding cache is missing.")
  }

  api_client <- NULL
  get_api_client <- function() {
    if (!is.null(api_client)) return(api_client)
    api_key <- Sys.getenv("OPENAI_API_KEY")
    if (!nzchar(api_key)) {
      stop(
        "OPENAI_API_KEY is not set. Six-field outputs were completed, ",
        "but uncached V3 orders require API access."
      )
    }
    openai_pkg <- import("openai")
    api_client <<- openai_pkg$OpenAI(api_key = api_key)
    api_client
  }

  embed_v3_cached <- function(texts, order_string) {
    if (order_string == "name>def>unit") {
      return(readRDS(original_v3_file))
    }
    key <- digest(list("V3_order", order_string, texts), algo = "md5")
    cache_file <- file.path(v3_cache_dir, paste0(key, ".rds"))
    if (file.exists(cache_file)) {
      emb <- readRDS(cache_file)
      if (!is.null(dim(emb)) && all(dim(emb) == c(461, 3072))) return(emb)
      stop("Invalid V3-order cache: ", cache_file)
    }

    client <- get_api_client()
    batches <- vector("list", ceiling(length(texts) / 100))
    batch_number <- 0L
    for (start in seq(1, length(texts), by = 100)) {
      batch_number <- batch_number + 1L
      stop_at <- min(start + 99, length(texts))
      response <- client$embeddings$create(
        input = as.list(texts[start:stop_at]),
        model = "text-embedding-3-large"
      )
      batches[[batch_number]] <- do.call(
        rbind,
        lapply(response$data, function(item) as.numeric(item$embedding))
      )
      cat("    API batch ", batch_number, "/", length(batches), "\n", sep = "")
    }
    emb <- do.call(rbind, batches)
    if (!all(dim(emb) == c(461, 3072))) {
      stop("Unexpected V3-order embedding dimensions.")
    }
    saveRDS(emb, cache_file)
    emb
  }

  v3_perms <- permutations(3, 3, names(v3_blocks))
  category_results_v3 <- vector("list", nrow(v3_perms))
  summary_results_v3 <- vector("list", nrow(v3_perms))
  labels_v3 <- matrix(
    NA_integer_,
    nrow = 461,
    ncol = nrow(v3_perms)
  )

  cat("\nScoring six V3 field orders...\n")
  for (i in seq_len(nrow(v3_perms))) {
    order_vec <- as.character(v3_perms[i, ])
    order_string <- paste(order_vec, collapse = ">")
    cat("  ", order_string, "\n", sep = "")
    texts <- assemble_v3_text(order_vec)
    emb <- embed_v3_cached(texts, order_string)
    labels <- run_hdbscan(emb)
    labels_v3[, i] <- labels

    category_scores <- score_labels(
      labels,
      analysis_id = "v3_three_field_6",
      order_string = order_string,
      perm_id = i
    )
    category_results_v3[[i]] <- category_scores
    summary_results_v3[[i]] <- summarise_order(category_scores, labels) %>%
      mutate(perm_id = i, order_string = order_string)
  }

  colnames(labels_v3) <- apply(v3_perms, 1, paste, collapse = ">")
  category_scores_v3 <- bind_rows(category_results_v3)
  summary_v3 <- bind_rows(summary_results_v3) %>%
    mutate(groundtruth_rank = min_rank(desc(mean_dominant_share))) %>%
    arrange(groundtruth_rank, desc(mean_dominant_purity), total_noise_rate)

  original_v3 <- summary_v3 %>% filter(order_string == "name>def>unit")
  if (nrow(original_v3) != 1 ||
      abs(original_v3$mean_dominant_share -
          v3_reference$mean_dominant_share) > 1e-12) {
    stop("Original V3 score does not reproduce multi_trait_version_summary.csv.")
  }

  saveRDS(
    list(
      base_keys = base %>% select(row_id, OriginalTraitName, OriginalTraitDef),
      labels = labels_v3
    ),
    file.path(results_dir, "v3_order_groundtruth_labels.rds")
  )
  write_csv(
    category_scores_v3,
    file.path(results_dir, "v3_order_groundtruth_category_scores.csv")
  )
  write_csv(
    summary_v3,
    file.path(results_dir, "v3_order_groundtruth_summary.csv")
  )

  v3_lines <- c(
    "GROUND-TRUTH RANKING OF THE SIX NAME/DEFINITION/UNIT ORDERS",
    paste0("Best order: ", summary_v3$order_string[1]),
    paste0("Best mean dominant share: ",
           round(summary_v3$mean_dominant_share[1], 4)),
    paste0("Original order rank: ", original_v3$groundtruth_rank, "/6"),
    paste0("Original mean dominant share: ",
           round(original_v3$mean_dominant_share, 4)),
    paste0("Range across six orders: ",
           round(min(summary_v3$mean_dominant_share), 4), " to ",
           round(max(summary_v3$mean_dominant_share), 4))
  )
  writeLines(
    v3_lines,
    file.path(results_dir, "v3_order_groundtruth_diagnostics.txt")
  )
  cat("\n", paste(v3_lines, collapse = "\n"), "\n", sep = "")
}

cat("\nAnalysis complete.\n")
