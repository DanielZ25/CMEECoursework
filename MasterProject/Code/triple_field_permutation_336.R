#!/usr/bin/env Rscript

# Exhaustive evaluation of all ordered three-field inputs drawn from V1-V8.
#
# Eight candidate fields, taken three at a time and retaining text order:
#   P(8, 3) = 336 ordered inputs
#
# Each input is embedded once, clustered with the project's fixed HDBSCAN
# parameters, and evaluated against all 10 curated reference categories.
# The script checkpoints labels after every completed input, so rerunning it
# resumes rather than paying for completed API work again.
#
# RStudio usage:
#   1. Open the Master Project folder as the working directory.
#   2. Set OPENAI_API_KEY in the R session/environment (never paste it here).
#   3. source("Code/triple_field_permutation_336.R")
#
# Command-line validation without API calls:
#   Rscript Code/triple_field_permutation_336.R --validate-only

suppressPackageStartupMessages({
  library(digest)
  library(dplyr)
  library(gtools)
  library(readr)
  library(reticulate)
  library(stringr)
  library(tidyr)
})

# -----------------------------------------------------------------------------
# 0. Configuration
# -----------------------------------------------------------------------------

MODEL <- "text-embedding-3-large"
EXPECTED_DIMENSIONS <- 3072L
N_EXPECTED_ROWS <- 461L
N_EXPECTED_CATEGORIES <- 10L
API_BATCH_SIZE <- 100L
MAX_API_ATTEMPTS <- 6L
DEGENERATE_CLUSTER_SHARE <- 0.50
CHECKPOINT_VERSION <- "triple336_v1"

args <- commandArgs(trailingOnly = TRUE)
validate_only <- "--validate-only" %in% args

project_root <- normalizePath(getwd(), mustWork = TRUE)
results_dir <- file.path(project_root, "Results")
output_dir <- file.path(results_dir, "triple_permutation_336")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- c(
  file.path(project_root, "Data", "VecTraits.csv"),
  file.path(results_dir, "featexp_label_matrix.csv"),
  file.path(results_dir, "multi_trait_ground_truth_used.csv"),
  file.path(results_dir, "multi_trait_version_summary.csv")
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Missing required file(s). Open the Master Project folder as the R ",
    "working directory before running this script:\n",
    paste(missing_files, collapse = "\n")
  )
}

# Use the same Python environment and clustering implementation as the project.
use_condaenv("pymc-env", required = TRUE)
hdbscan_pkg <- import("hdbscan")
sk_metrics <- import("sklearn.metrics")

read_char_csv <- function(path) {
  read_csv(
    path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )
}

# -----------------------------------------------------------------------------
# 1. Reconstruct and validate the canonical 461-row analysis base
# -----------------------------------------------------------------------------

field_schedule <- tribble(
  ~key,      ~col,                  ~block_label,
  "name",    "OriginalTraitName",   "Trait",
  "def",     "OriginalTraitDef",    "Definition",
  "unit",    "OriginalTraitUnit",   "Unit",
  "order",   "Interactor1Order",    "Organism order",
  "family",  "Interactor1Family",   "Organism family",
  "stage",   "Interactor1Stage",    "Life stage",
  "class",   "Interactor1Class",    "Organism class",
  "phylum",  "Interactor1Phylum",   "Organism phylum"
)
field_keys <- field_schedule$key
field_columns <- field_schedule$col

raw <- read_char_csv(file.path(project_root, "Data", "VecTraits.csv"))
base <- raw %>%
  mutate(across(all_of(field_columns), ~ str_trim(as.character(.)))) %>%
  distinct(OriginalTraitName, OriginalTraitDef, .keep_all = TRUE) %>%
  select(all_of(field_columns)) %>%
  mutate(row_id = row_number())

if (nrow(base) != N_EXPECTED_ROWS) {
  stop(
    "Expected ", N_EXPECTED_ROWS,
    " distinct name-definition rows; found ", nrow(base), "."
  )
}
if (anyDuplicated(base[c("OriginalTraitName", "OriginalTraitDef")]) > 0) {
  stop("The canonical base contains duplicate name-definition pairs.")
}

label_matrix <- read_char_csv(
  file.path(results_dir, "featexp_label_matrix.csv")
)
if (nrow(label_matrix) != N_EXPECTED_ROWS ||
    !identical(base$OriginalTraitName, label_matrix$OriginalTraitName) ||
    !identical(base$OriginalTraitDef, label_matrix$OriginalTraitDef)) {
  stop("The canonical 461 rows are not aligned with featexp_label_matrix.csv.")
}

# Build one labelled text block per field, matching input_EXPANSION.R exactly.
make_block <- function(column_name, block_label) {
  values <- base[[column_name]]
  values <- ifelse(
    is.na(values) | values == "",
    "not specified",
    values
  )
  paste0(block_label, ": ", values)
}

blocks <- Map(
  make_block,
  field_schedule$col,
  field_schedule$block_label
)
names(blocks) <- field_keys

assemble_text <- function(order_vector) {
  if (length(order_vector) != 3L ||
      !all(order_vector %in% field_keys) ||
      anyDuplicated(order_vector) > 0) {
    stop("Each ordered input must contain three different V1-V8 fields.")
  }
  paste0(
    do.call(paste, c(blocks[order_vector], sep = ". ")),
    "."
  )
}

# -----------------------------------------------------------------------------
# 2. Load the 10 reference categories and resolve them to row indices
# -----------------------------------------------------------------------------

reference <- read_char_csv(
  file.path(results_dir, "multi_trait_ground_truth_used.csv")
) %>%
  select(trait_category, OriginalTraitName, OriginalTraitDef) %>%
  distinct()

if (n_distinct(reference$trait_category) != N_EXPECTED_CATEGORIES) {
  stop(
    "Expected ", N_EXPECTED_CATEGORIES,
    " reference categories; found ",
    n_distinct(reference$trait_category), "."
  )
}

reference_indexed <- reference %>%
  left_join(
    base %>% select(row_id, OriginalTraitName, OriginalTraitDef),
    by = c("OriginalTraitName", "OriginalTraitDef")
  )
if (anyNA(reference_indexed$row_id)) {
  stop("At least one reference concept is absent from the canonical 461 rows.")
}

category_indices <- split(
  as.integer(reference_indexed$row_id),
  reference_indexed$trait_category
)

# -----------------------------------------------------------------------------
# 3. Generate all P(8,3) = 336 ordered inputs
# -----------------------------------------------------------------------------

canonical_combination <- function(order_vector) {
  paste(field_keys[field_keys %in% order_vector], collapse = "+")
}

permutation_matrix <- permutations(
  n = length(field_keys),
  r = 3,
  v = field_keys,
  repeats.allowed = FALSE
)

order_table <- as_tibble(permutation_matrix, .name_repair = "minimal")
names(order_table) <- c("field_1", "field_2", "field_3")
order_table <- order_table %>%
  rowwise() %>%
  mutate(
    order_string = paste(c_across(field_1:field_3), collapse = ">"),
    combination_key = canonical_combination(c_across(field_1:field_3)),
    priority = case_when(
      order_string == "name>def>unit" ~ 0L,
      combination_key == "name+def+unit" ~ 1L,
      TRUE ~ 2L
    )
  ) %>%
  ungroup() %>%
  arrange(priority, combination_key, order_string) %>%
  mutate(order_id = row_number()) %>%
  select(order_id, field_1, field_2, field_3,
         order_string, combination_key)

if (nrow(order_table) != 336L ||
    n_distinct(order_table$order_string) != 336L ||
    n_distinct(order_table$combination_key) != 56L) {
  stop("The ordered-three-field design is not 336 orders across 56 field sets.")
}
write_csv(order_table, file.path(output_dir, "triple336_design.csv"))

# -----------------------------------------------------------------------------
# 4. Embedding, clustering, and scoring helpers
# -----------------------------------------------------------------------------

run_hdbscan <- function(embedding_matrix) {
  if (is.null(dim(embedding_matrix)) ||
      !all(dim(embedding_matrix) == c(N_EXPECTED_ROWS, EXPECTED_DIMENSIONS))) {
    stop(
      "Embedding matrix must be ", N_EXPECTED_ROWS,
      " x ", EXPECTED_DIMENSIONS, "."
    )
  }
  clusterer <- hdbscan_pkg$HDBSCAN(
    min_cluster_size = 5L,
    min_samples = 3L,
    metric = "euclidean"
  )
  as.integer(clusterer$fit_predict(embedding_matrix))
}

harmonic_score <- function(share, purity) {
  if (is.na(purity) || share + purity == 0) return(0)
  2 * share * purity / (share + purity)
}

score_one_order <- function(labels, order_id, order_string, combination_key) {
  if (length(labels) != N_EXPECTED_ROWS) {
    stop("Cluster labels must have length ", N_EXPECTED_ROWS, ".")
  }

  non_noise_sizes <- table(labels[labels != -1L])

  bind_rows(lapply(names(category_indices), function(category) {
    idx <- category_indices[[category]]
    category_labels <- labels[idx]
    valid <- category_labels[category_labels != -1L]
    n_concepts <- length(idx)
    n_noise <- sum(category_labels == -1L)

    if (length(valid) == 0L) {
      return(tibble(
        order_id = order_id,
        order_string = order_string,
        combination_key = combination_key,
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
        balanced_score = 0
      ))
    }

    target_counts <- table(valid)
    dominant_n <- max(as.integer(target_counts))
    tied_ids <- names(target_counts)[as.integer(target_counts) == dominant_n]
    tied_total_sizes <- as.integer(non_noise_sizes[tied_ids])
    tied_purities <- dominant_n / tied_total_sizes
    dominant_share <- dominant_n / n_concepts

    tibble(
      order_id = order_id,
      order_string = order_string,
      combination_key = combination_key,
      trait_category = category,
      n_concepts = n_concepts,
      n_noise = n_noise,
      n_clusters_occupied = n_distinct(valid),
      dominant_n = dominant_n,
      dominant_cluster_ids = paste(sort(as.integer(tied_ids)), collapse = ";"),
      dominant_cluster_tie = length(tied_ids) > 1L,
      dominant_share = dominant_share,
      non_noise_share = length(valid) / n_concepts,
      dominant_purity_min = min(tied_purities),
      dominant_purity_mean = mean(tied_purities),
      dominant_purity_max = max(tied_purities),
      # When several clusters contain the same maximum number of target
      # concepts, use their mean purity rather than choosing a cluster by its
      # arbitrary numeric ID or taking only the most favourable tied result.
      balanced_score = harmonic_score(dominant_share, mean(tied_purities))
    )
  }))
}

summarise_one_order <- function(category_scores, labels) {
  non_noise <- labels[labels != -1L]
  cluster_sizes <- as.integer(table(non_noise))

  category_scores %>%
    summarise(
      n_categories = n(),
      mean_dominant_share = mean(dominant_share),
      median_dominant_share = median(dominant_share),
      min_dominant_share = min(dominant_share),
      mean_dominant_purity = mean(coalesce(dominant_purity_mean, 0)),
      median_dominant_purity = median(coalesce(dominant_purity_mean, 0)),
      mean_balanced_score = mean(coalesce(balanced_score, 0)),
      mean_non_noise_share = mean(non_noise_share),
      total_noise_rate = mean(labels == -1L),
      n_clusters = n_distinct(non_noise),
      max_cluster_share = if (length(cluster_sizes) > 0L) {
        max(cluster_sizes) / length(labels)
      } else {
        0
      },
      n_dominant_cluster_ties = sum(dominant_cluster_tie)
    )
}

# The original V3 cache is reused and also acts as an alignment check.
v3_texts <- assemble_text(c("name", "def", "unit"))
v3_cache_key <- digest(list("V3", v3_texts), algo = "md5")
v3_cache_file <- file.path(
  results_dir,
  "featexp_cache",
  paste0(v3_cache_key, ".rds")
)
if (!file.exists(v3_cache_file)) {
  stop("The exact 461-row V3 embedding cache is missing: ", v3_cache_file)
}

validate_v3_cache <- function(verbose = TRUE) {
  v3_embeddings <- readRDS(v3_cache_file)
  v3_labels <- run_hdbscan(v3_embeddings)
  expected_v3 <- as.integer(label_matrix$V3)
  v3_ari <- as.numeric(sk_metrics$adjusted_rand_score(expected_v3, v3_labels))

  v3_scores <- score_one_order(
    v3_labels,
    order_id = 1L,
    order_string = "name>def>unit",
    combination_key = "name+def+unit"
  )
  v3_summary <- summarise_one_order(v3_scores, v3_labels)
  expected_summary <- read_csv(
    file.path(results_dir, "multi_trait_version_summary.csv"),
    show_col_types = FALSE
  ) %>%
    filter(version == "V3")

  if (nrow(expected_summary) != 1L || abs(v3_ari - 1) > 1e-12) {
    stop("The exact V3 cache does not reproduce the current V3 labels.")
  }
  if (abs(v3_summary$mean_dominant_share -
          expected_summary$mean_dominant_share) > 1e-12) {
    stop("The V3 dominant-share calculation does not reproduce the heat map.")
  }

  if (verbose) {
    cat(
      "Validation passed:\n",
      "  rows = ", nrow(base), "\n",
      "  ordered triples = ", nrow(order_table), "\n",
      "  reference categories = ", length(category_indices), "\n",
      "  V3 ARI versus saved labels = ", round(v3_ari, 6), "\n",
      "  V3 mean dominant share = ",
      round(v3_summary$mean_dominant_share, 6), "\n",
      sep = ""
    )
  }

  invisible(list(labels = v3_labels, scores = v3_scores,
                 summary = v3_summary))
}

v3_validation <- validate_v3_cache(verbose = TRUE)

if (validate_only) {
  cat("Validation-only run complete. No API calls were made.\n")
  quit(save = "no", status = 0)
}

# -----------------------------------------------------------------------------
# 5. Resumable API and label checkpoint
# -----------------------------------------------------------------------------

state_file <- file.path(output_dir, "triple336_checkpoint.rds")
progress_file <- file.path(output_dir, "triple336_progress.csv")

new_state <- function() {
  list(
    checkpoint_version = CHECKPOINT_VERSION,
    model = MODEL,
    field_keys = field_keys,
    order_strings = order_table$order_string,
    labels = matrix(
      NA_integer_,
      nrow = N_EXPECTED_ROWS,
      ncol = nrow(order_table),
      dimnames = list(NULL, paste0("O", order_table$order_id))
    ),
    completed = rep(FALSE, nrow(order_table)),
    api_prompt_tokens = rep(NA_real_, nrow(order_table)),
    completed_at = rep(NA_character_, nrow(order_table))
  )
}

if (file.exists(state_file)) {
  state <- readRDS(state_file)
  state_is_valid <- identical(state$checkpoint_version, CHECKPOINT_VERSION) &&
    identical(state$model, MODEL) &&
    identical(state$field_keys, field_keys) &&
    identical(state$order_strings, order_table$order_string) &&
    all(dim(state$labels) == c(N_EXPECTED_ROWS, nrow(order_table))) &&
    length(state$completed) == nrow(order_table)
  if (!state_is_valid) {
    stop(
      "The existing checkpoint does not match this experiment. Move or rename ",
      state_file, " before starting a new run."
    )
  }
  cat("Loaded checkpoint: ", sum(state$completed), "/336 completed.\n", sep = "")
} else {
  state <- new_state()
}

write_progress <- function(state) {
  write_csv(
    order_table %>%
      mutate(
        completed = state$completed,
        api_prompt_tokens = state$api_prompt_tokens,
        completed_at = state$completed_at
      ),
    progress_file
  )
}

save_checkpoint <- function(state) {
  temporary_file <- tempfile(
    pattern = "triple336_checkpoint_",
    tmpdir = output_dir,
    fileext = ".rds"
  )
  saveRDS(state, temporary_file)
  if (!file.rename(temporary_file, state_file)) {
    unlink(temporary_file)
    stop("Could not update checkpoint file: ", state_file)
  }
  write_progress(state)
}

# Seed the exact V3 result without an API call if it is not already complete.
v3_order_id <- order_table$order_id[
  order_table$order_string == "name>def>unit"
]
if (length(v3_order_id) != 1L) stop("Could not identify the original V3 order.")
if (!state$completed[v3_order_id]) {
  state$labels[, v3_order_id] <- v3_validation$labels
  state$completed[v3_order_id] <- TRUE
  state$api_prompt_tokens[v3_order_id] <- 0
  state$completed_at[v3_order_id] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  save_checkpoint(state)
  cat("Stored exact cached V3 result (no API call).\n")
}

pending_ids <- which(!state$completed)
if (length(pending_ids) > 0L && !nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  stop(
    "OPENAI_API_KEY is not set. Set it securely in the R session/environment, ",
    "then rerun the script. The saved V3 checkpoint will be reused."
  )
}

openai_pkg <- NULL
api_client <- NULL
get_api_client <- function() {
  if (!is.null(api_client)) return(api_client)
  openai_pkg <<- import("openai")
  api_client <<- openai_pkg$OpenAI(
    api_key = Sys.getenv("OPENAI_API_KEY")
  )
  api_client
}

embed_texts <- function(texts, order_string) {
  client <- get_api_client()
  starts <- seq(1L, length(texts), by = API_BATCH_SIZE)
  matrices <- vector("list", length(starts))
  total_prompt_tokens <- 0

  for (batch_index in seq_along(starts)) {
    start <- starts[batch_index]
    stop_at <- min(start + API_BATCH_SIZE - 1L, length(texts))
    batch_texts <- texts[start:stop_at]
    response <- NULL

    for (attempt in seq_len(MAX_API_ATTEMPTS)) {
      response <- tryCatch(
        client$embeddings$create(
          input = as.list(batch_texts),
          model = MODEL
        ),
        error = function(error) error
      )

      if (!inherits(response, "error")) break
      if (attempt == MAX_API_ATTEMPTS) {
        stop(
          "Embedding API failed after ", MAX_API_ATTEMPTS,
          " attempts for ", order_string, " batch ", batch_index,
          ": ", conditionMessage(response)
        )
      }

      wait_seconds <- min(60, 2^(attempt - 1)) + runif(1, 0, 1)
      cat(
        "    API retry ", attempt, "/", MAX_API_ATTEMPTS,
        " after ", round(wait_seconds, 1), " seconds.\n",
        sep = ""
      )
      Sys.sleep(wait_seconds)
    }

    items <- response$data
    item_indices <- vapply(
      items,
      function(item) as.integer(item$index),
      integer(1)
    )
    items <- items[order(item_indices)]
    matrices[[batch_index]] <- do.call(
      rbind,
      lapply(items, function(item) as.numeric(item$embedding))
    )

    if (!is.null(response$usage) &&
        !is.null(response$usage$prompt_tokens)) {
      total_prompt_tokens <- total_prompt_tokens +
        as.numeric(response$usage$prompt_tokens)
    }

    cat(
      "    API batch ", batch_index, "/", length(starts), " complete.\n",
      sep = ""
    )
  }

  embedding_matrix <- do.call(rbind, matrices)
  if (!all(dim(embedding_matrix) ==
           c(N_EXPECTED_ROWS, EXPECTED_DIMENSIONS))) {
    stop("Unexpected embedding dimensions for ", order_string, ".")
  }

  list(
    embeddings = embedding_matrix,
    prompt_tokens = total_prompt_tokens
  )
}

cat(
  "Starting/resuming 336 ordered-three-field experiment.\n",
  "Completed: ", sum(state$completed),
  "; remaining: ", sum(!state$completed), "\n",
  sep = ""
)

for (i in which(!state$completed)) {
  row <- order_table[i, ]
  order_vector <- c(row$field_1, row$field_2, row$field_3)
  cat(
    "\n[", i, "/", nrow(order_table), "] ", row$order_string, "\n",
    sep = ""
  )

  texts <- assemble_text(order_vector)
  embedded <- embed_texts(texts, row$order_string)
  labels <- run_hdbscan(embedded$embeddings)

  state$labels[, i] <- labels
  state$completed[i] <- TRUE
  state$api_prompt_tokens[i] <- embedded$prompt_tokens
  state$completed_at[i] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  save_checkpoint(state)

  cat(
    "  saved; clusters = ", n_distinct(labels[labels != -1L]),
    "; noise = ", sum(labels == -1L),
    "; completed = ", sum(state$completed), "/336\n",
    sep = ""
  )

  rm(embedded, labels, texts)
  if (i %% 10L == 0L) gc(verbose = FALSE)
}

if (!all(state$completed)) {
  stop("The run stopped before all 336 inputs were completed.")
}

# -----------------------------------------------------------------------------
# 6. Calculate all 10-category metrics and final rankings
# -----------------------------------------------------------------------------

category_results <- vector("list", nrow(order_table))
order_results <- vector("list", nrow(order_table))

for (i in seq_len(nrow(order_table))) {
  labels <- state$labels[, i]
  category_scores <- score_one_order(
    labels = labels,
    order_id = order_table$order_id[i],
    order_string = order_table$order_string[i],
    combination_key = order_table$combination_key[i]
  )
  category_results[[i]] <- category_scores
  order_results[[i]] <- summarise_one_order(category_scores, labels) %>%
    mutate(
      order_id = order_table$order_id[i],
      order_string = order_table$order_string[i],
      combination_key = order_table$combination_key[i],
      api_prompt_tokens = state$api_prompt_tokens[i]
    )
}

category_scores <- bind_rows(category_results)
order_summary <- bind_rows(order_results) %>%
  mutate(
    degenerate = max_cluster_share > DEGENERATE_CLUSTER_SHARE,
    dominant_share_rank_all = min_rank(desc(mean_dominant_share)),
    dominant_share_rank_screened = if_else(
      degenerate,
      NA_integer_,
      min_rank(if_else(degenerate, NA_real_, desc(mean_dominant_share)))
    ),
    balanced_rank_screened = if_else(
      degenerate,
      NA_integer_,
      min_rank(if_else(degenerate, NA_real_, desc(mean_balanced_score)))
    )
  ) %>%
  arrange(
    degenerate,
    dominant_share_rank_screened,
    desc(mean_dominant_purity),
    total_noise_rate
  )

fieldset_summary <- order_summary %>%
  group_by(combination_key) %>%
  summarise(
    n_orders = n(),
    n_degenerate_orders = sum(degenerate),
    mean_dominant_share_across_orders = mean(mean_dominant_share),
    sd_dominant_share_across_orders = sd(mean_dominant_share),
    min_dominant_share_across_orders = min(mean_dominant_share),
    max_dominant_share_across_orders = max(mean_dominant_share),
    mean_dominant_purity_across_orders = mean(mean_dominant_purity),
    min_dominant_purity_across_orders = min(mean_dominant_purity),
    mean_balanced_score_across_orders = mean(mean_balanced_score),
    min_balanced_score_across_orders = min(mean_balanced_score),
    mean_noise_rate_across_orders = mean(total_noise_rate),
    best_order_by_share = order_string[which.max(mean_dominant_share)][1],
    best_order_by_balanced_score =
      order_string[which.max(mean_balanced_score)][1],
    .groups = "drop"
  ) %>%
  mutate(
    eligible = n_degenerate_orders == 0L,
    fieldset_share_rank = if_else(
      eligible,
      min_rank(if_else(
        eligible,
        desc(mean_dominant_share_across_orders),
        NA_real_
      )),
      NA_integer_
    ),
    fieldset_balanced_rank = if_else(
      eligible,
      min_rank(if_else(
        eligible,
        desc(mean_balanced_score_across_orders),
        NA_real_
      )),
      NA_integer_
    )
  ) %>%
  arrange(
    !eligible,
    fieldset_balanced_rank,
    fieldset_share_rank
  )

if (any(fieldset_summary$n_orders != 6L) || nrow(fieldset_summary) != 56L) {
  stop("Each of the 56 three-field sets must contain exactly six orders.")
}

semantic_trio_orders <- order_summary %>%
  filter(combination_key == "name+def+unit") %>%
  arrange(desc(mean_balanced_score), desc(mean_dominant_share))

# Recheck the original V3 row against the pre-existing heat-map summary.
original_v3_result <- order_summary %>%
  filter(order_string == "name>def>unit")
expected_v3_summary <- read_csv(
  file.path(results_dir, "multi_trait_version_summary.csv"),
  show_col_types = FALSE
) %>%
  filter(version == "V3")
if (nrow(original_v3_result) != 1L ||
    abs(original_v3_result$mean_dominant_share -
        expected_v3_summary$mean_dominant_share) > 1e-12) {
  stop("Final original-V3 score does not reproduce the existing heat map.")
}

write_csv(
  category_scores,
  file.path(output_dir, "triple336_category_scores.csv")
)
write_csv(
  order_summary,
  file.path(output_dir, "triple336_order_summary.csv")
)
write_csv(
  fieldset_summary,
  file.path(output_dir, "triple336_fieldset_summary.csv")
)
write_csv(
  semantic_trio_orders,
  file.path(output_dir, "triple336_name_def_unit_orders.csv")
)
write_csv(
  order_summary %>% filter(!degenerate) %>% slice_head(n = 20),
  file.path(output_dir, "triple336_top20_orders_by_share.csv")
)
saveRDS(
  list(
    base_keys = base %>%
      select(row_id, OriginalTraitName, OriginalTraitDef),
    design = order_table,
    labels = state$labels
  ),
  file.path(output_dir, "triple336_final_labels.rds")
)

best_order_share <- order_summary %>%
  filter(!degenerate) %>%
  arrange(desc(mean_dominant_share), desc(mean_dominant_purity)) %>%
  slice(1)
best_order_balanced <- order_summary %>%
  filter(!degenerate) %>%
  arrange(desc(mean_balanced_score), desc(mean_dominant_share)) %>%
  slice(1)
best_fieldset <- fieldset_summary %>%
  filter(eligible) %>%
  arrange(fieldset_balanced_rank, fieldset_share_rank) %>%
  slice(1)
semantic_fieldset <- fieldset_summary %>%
  filter(combination_key == "name+def+unit")

diagnostic_lines <- c(
  "ORDERED THREE-FIELD GROUND-TRUTH EXPERIMENT (V1-V8)",
  paste0("Rows: ", nrow(base)),
  paste0("Reference categories: ", length(category_indices)),
  paste0("Ordered inputs: ", nrow(order_table)),
  paste0("Unique three-field sets: ", nrow(fieldset_summary)),
  paste0("Degenerate ordered inputs: ", sum(order_summary$degenerate)),
  paste0("Recorded API prompt tokens: ",
         sum(state$api_prompt_tokens, na.rm = TRUE)),
  "",
  "Best non-degenerate individual order by mean dominant share:",
  paste0("  ", best_order_share$order_string),
  paste0("  mean dominant share = ",
         round(best_order_share$mean_dominant_share, 4)),
  paste0("  mean dominant purity = ",
         round(best_order_share$mean_dominant_purity, 4)),
  paste0("  mean balanced score = ",
         round(best_order_share$mean_balanced_score, 4)),
  "",
  "Best non-degenerate individual order by mean balanced score:",
  paste0("  ", best_order_balanced$order_string),
  paste0("  mean dominant share = ",
         round(best_order_balanced$mean_dominant_share, 4)),
  paste0("  mean dominant purity = ",
         round(best_order_balanced$mean_dominant_purity, 4)),
  paste0("  mean balanced score = ",
         round(best_order_balanced$mean_balanced_score, 4)),
  "",
  "Best eligible three-field set across all six internal orders:",
  paste0("  ", best_fieldset$combination_key),
  paste0("  mean dominant share across orders = ",
         round(best_fieldset$mean_dominant_share_across_orders, 4)),
  paste0("  mean dominant purity across orders = ",
         round(best_fieldset$mean_dominant_purity_across_orders, 4)),
  paste0("  mean balanced score across orders = ",
         round(best_fieldset$mean_balanced_score_across_orders, 4)),
  "",
  "Name + definition + unit across its six orders:",
  paste0("  field-set share rank = ", semantic_fieldset$fieldset_share_rank,
         "/", sum(fieldset_summary$eligible)),
  paste0("  field-set balanced rank = ",
         semantic_fieldset$fieldset_balanced_rank,
         "/", sum(fieldset_summary$eligible)),
  paste0("  mean dominant share across orders = ",
         round(semantic_fieldset$mean_dominant_share_across_orders, 4)),
  paste0("  range of dominant share = ",
         round(semantic_fieldset$min_dominant_share_across_orders, 4),
         " to ",
         round(semantic_fieldset$max_dominant_share_across_orders, 4)),
  paste0("  mean dominant purity across orders = ",
         round(semantic_fieldset$mean_dominant_purity_across_orders, 4)),
  paste0("  mean balanced score across orders = ",
         round(semantic_fieldset$mean_balanced_score_across_orders, 4))
)

writeLines(
  diagnostic_lines,
  file.path(output_dir, "triple336_diagnostics.txt")
)

cat("\n", paste(diagnostic_lines, collapse = "\n"), "\n", sep = "")
cat("\nAnalysis complete. Outputs: ", output_dir, "\n", sep = "")
