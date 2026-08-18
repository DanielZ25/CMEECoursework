# ============================================================
# Experiment 1 — Feature Expansion Rarefaction (VecTraits)
# ============================================================
# Route X: ALL 20 versions re-embedded fresh, unified dedup
# distinct(OriginalTraitName, OriginalTraitDef) => 461 rows.
# This keeps every version on the SAME rows/row-order, so the
# rarefaction curve is comparable, and matches the 461-row basis
# used in the ordering experiment.
#
# Concatenation = Route A: name-led, each new field APPENDED in the
# order it is added (same style as the original V1-V6 templates).
# Empty values filled with "not specified".
#
# Prereqs in session (run analysis.R / analysis_more_inputs.R first):
#   rows, get_embeddings, hdbscan_pkg, OPENAI_API_KEY, pymc-env w/ sklearn
# ------------------------------------------------------------

library(tidyverse)
library(reticulate)
library(digest)

setwd("/Users/danielzhu/Documents/Master Project")
dir.create("Results/featexp_cache", showWarnings = FALSE)

# ── 1. Field schedule: incremental add order (the rarefaction X-axis) ──
# Each entry: short label -> the column it pulls from + the sentence-block label.
# Order here defines V1..V20 (V_k uses the first k fields).
field_schedule <- tribble(
  ~key,        ~col,                  ~block_label,
  "name",      "OriginalTraitName",   "Trait",
  "def",       "OriginalTraitDef",    "Definition",
  "unit",      "OriginalTraitUnit",   "Unit",
  "order",     "Interactor1Order",    "Organism order",
  "family",    "Interactor1Family",   "Organism family",
  "stage",     "Interactor1Stage",    "Life stage",
  "class",     "Interactor1Class",    "Organism class",
  "phylum",    "Interactor1Phylum",   "Organism phylum",
  "kingdom",   "Interactor1Kingdom",  "Organism kingdom",
  "genus",     "Interactor1Genus",    "Organism genus",
  "species",   "Interactor1Species",  "Organism species",
  "habitat",   "Habitat",             "Habitat",
  "labfield",  "LabField",            "Lab or field",
  "loctype",   "LocationType",        "Location type",
  "location",  "Location",            "Location",
  "temp",      "Interactor1Temp",     "Temperature",
  "tempunit",  "Interactor1TempUnit", "Temperature unit",
  "lat",       "Latitude",            "Latitude",
  "long",      "Longitude",           "Longitude",
  "coordtype", "CoordinateType",      "Coordinate type"
)
n_versions <- nrow(field_schedule)   # 20
all_cols    <- field_schedule$col

# ── 2. Build the 461-row base table (unified dedup) ──────────────
base_tbl <- rows %>%
  mutate(across(all_of(all_cols), ~ str_trim(as.character(.)))) %>%
  distinct(OriginalTraitName, OriginalTraitDef, .keep_all = TRUE) %>%
  select(all_of(all_cols))

cat("Base rows:", nrow(base_tbl), "\n")
N_ROWS <- nrow(base_tbl)

# Per-field text block, with "not specified" fallback for empty/NA
make_block <- function(col, label) {
  vals <- base_tbl[[col]]
  vals <- ifelse(is.na(vals) | vals == "", "not specified", vals)
  paste0(label, ": ", vals)
}
blocks <- Map(make_block, field_schedule$col, field_schedule$block_label)
names(blocks) <- field_schedule$key

# Assemble V_k text = first k blocks, joined "block1. block2. ... ."
assemble_version <- function(k) {
  parts <- blocks[seq_len(k)]
  paste0(do.call(paste, c(parts, sep = ". ")), ".")
}

# ── 3. Hardened cached embedding (fail-fast, never cache NULL) ──
embed_cached <- function(texts, tag) {
  key        <- digest(list(tag, texts), algo = "md5")
  cache_file <- sprintf("Results/featexp_cache/%s.rds", key)
  if (file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    if (!is.null(cached) && nrow(cached) == length(texts)) return(cached)
    unlink(cache_file)
  }
  batch_size <- 100
  acc <- list()
  for (i in seq(1, length(texts), by = batch_size)) {
    batch     <- texts[i:min(i + batch_size - 1, length(texts))]
    emb_batch <- get_embeddings(batch)
    if (is.null(emb_batch)) stop(sprintf("Embedding failed for %s (API NULL). Nothing cached.", tag))
    acc[[length(acc) + 1]] <- emb_batch
  }
  emb <- do.call(rbind, acc)
  stopifnot(!is.null(emb), nrow(emb) == length(texts))
  saveRDS(emb, cache_file)
  emb
}

run_hdbscan <- function(emb) {
  cl <- hdbscan_pkg$HDBSCAN(min_cluster_size = 5L, min_samples = 3L, metric = "euclidean")
  as.vector(cl$fit_predict(emb))
}

# ── 4. Loop over V1..V20: embed -> cluster -> 3 metrics ──────────
summary_rows <- list()
labels_all   <- list()

for (k in seq_len(n_versions)) {
  vname  <- sprintf("V%d", k)
  fields <- paste(field_schedule$key[seq_len(k)], collapse = "+")
  cat(sprintf("\n=== %s (%s) ===\n", vname, fields))
  
  texts  <- assemble_version(k)
  emb    <- embed_cached(texts, tag = vname)
  labels <- run_hdbscan(emb)
  labels_all[[vname]] <- labels
  
  non_noise        <- labels[labels != -1]
  n_clusters       <- length(unique(non_noise))
  n_noise          <- sum(labels == -1)
  tab              <- as.data.frame(table(non_noise))   # avoids 'list' dplyr bug
  sizes            <- if (nrow(tab) > 0) tab$Freq else 0
  avg_cluster_size <- if (n_clusters > 0) mean(sizes) else 0
  
  cat(sprintf("  clusters=%d  noise=%d (%.1f%%)  avg_size=%.1f\n",
              n_clusters, n_noise, 100*n_noise/length(labels), avg_cluster_size))
  
  summary_rows[[vname]] <- data.frame(
    version          = vname,
    k_fields         = k,
    last_field_added = field_schedule$key[k],
    fields           = fields,
    n_clusters       = n_clusters,
    n_noise          = n_noise,
    noise_rate       = round(n_noise / length(labels), 4),
    avg_cluster_size = round(avg_cluster_size, 2),
    stringsAsFactors = FALSE
  )
}

rarefaction <- bind_rows(summary_rows)
cat("\n=== FEATURE EXPANSION RAREFACTION SUMMARY ===\n")
print(rarefaction)
write_csv(rarefaction, "Results/featexp_rarefaction.csv")

# Per-trait label matrix across versions
label_matrix <- base_tbl %>%
  select(OriginalTraitName, OriginalTraitDef) %>%
  bind_cols(as.data.frame(labels_all))
write_csv(label_matrix, "Results/featexp_label_matrix.csv")


# ── 5. Plot: 3-metric rarefaction curve (with value labels) ──────
plot_df <- rarefaction %>%
  select(k_fields, last_field_added, n_clusters, noise_rate, avg_cluster_size) %>%
  pivot_longer(c(n_clusters, noise_rate, avg_cluster_size),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(recode(metric,
                                n_clusters       = "N clusters",
                                noise_rate       = "Noise rate",
                                avg_cluster_size = "Avg cluster size"),
                         levels = c("Avg cluster size", "N clusters", "Noise rate")),
         # label: integers for counts, 3 decimals for noise rate
         lbl = ifelse(metric == "Noise rate",
                      sprintf("%.3f", value),
                      sprintf("%.1f", value)))

p <- ggplot(plot_df, aes(k_fields, value)) +
  geom_line(linewidth = 0.8, colour = "steelblue") +
  geom_point(size = 1.8, colour = "steelblue") +
  geom_text(aes(label = lbl), vjust = -0.9, size = 2.5, colour = "grey25") +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = rarefaction$k_fields,
                     labels = paste0("V", rarefaction$k_fields, "\n+",
                                     rarefaction$last_field_added),
                     expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.15))) +  # headroom for labels
  labs(x = "Cumulative input fields", y = NULL,
       title = "Cumulative Feature Expansion (VecTraits, 461 concepts)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(size = 6))

ggsave("Results/featexp_rarefaction_curve.png", p, width = 9, height = 9, dpi = 150)
ggsave("Results/featexp_rarefaction_curve.pdf", p, width = 9, height = 9)
cat("\nSaved:\n  Results/featexp_rarefaction.csv\n  Results/featexp_label_matrix.csv\n",
    "  Results/featexp_rarefaction_curve.png / .pdf\n")
cat("Done.\n")
