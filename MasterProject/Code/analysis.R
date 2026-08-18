# ============================================================
# VecTraits Heterogeneity Analysis & Semantic Clustering
# ============================================================
Sys.setenv(RETICULATE_PYTHON = "/Users/danielzhu/Library/r-miniconda-arm64/envs/pymc-env/bin/python")
library(reticulate)
library(tidyverse)



setwd("/Users/danielzhu/Documents/Master Project")

# ── 1. read data ───────────────────────────────────────────────
rows <- read_csv("Data/VecTraits.csv")

cat("Total rows:", nrow(rows), "\n")

# ── 2. unique trait names ───────────────────────────────────────────────
unique_names <- rows %>%
  mutate(OriginalTraitName = str_trim(OriginalTraitName)) %>%
  count(OriginalTraitName, sort = TRUE)

cat("Unique OriginalTraitNames:", nrow(unique_names), "\n")

# ── 3. Name + Definition paring ─────────────────────────────────
pairs <- rows %>%
  mutate(across(c(OriginalTraitName, OriginalTraitDef), str_trim)) %>%
  count(OriginalTraitName, OriginalTraitDef, sort = TRUE)

cat("Unique name+definition pairs:", nrow(pairs), "\n")

# ── 4. one name -> multiple definitions ───────────────────────────────────
name_to_defs <- rows %>%
  mutate(across(c(OriginalTraitName, OriginalTraitDef), str_trim)) %>%
  group_by(OriginalTraitName) %>%
  summarise(n_definitions = n_distinct(OriginalTraitDef)) %>%
  ungroup()

multi_def <- name_to_defs %>% filter(n_definitions > 1)
cat("Names with multiple definitions:", nrow(multi_def), "\n")

# ── 5. StandardisedTraitName coverage──────────────────────────
std_filled <- rows %>%
  filter(str_trim(StandardisedTraitName) != "" & !is.na(StandardisedTraitName))

cat("StandardisedTraitName filled:", nrow(std_filled), "/", nrow(rows), "\n")
cat(sprintf("(%.1f%%)\n", 100 * nrow(std_filled) / nrow(rows)))

unique_std <- std_filled %>%
  count(StandardisedTraitName, sort = TRUE)

cat("Unique StandardisedTraitName values:", nrow(unique_std), "\n")
cat("Top 5:\n")
print(head(unique_std, 5))

# ── 6. unit consistency ─────────────────────────────────────────────
name_to_units <- rows %>%
  mutate(across(c(OriginalTraitName, OriginalTraitUnit), str_trim)) %>%
  filter(OriginalTraitUnit != "" & !is.na(OriginalTraitUnit)) %>%
  group_by(OriginalTraitName) %>%
  summarise(n_units = n_distinct(OriginalTraitUnit)) %>%
  ungroup()

multi_unit <- name_to_units %>% filter(n_units > 1)
cat("Names with multiple units:", nrow(multi_unit), "\n")

# ── 7. Top 20 most frequent trait names ──────────────────────────────
cat("\nTop 20 most frequent trait names:\n")
top20 <- unique_names %>%
  head(20) %>%
  left_join(name_to_defs, by = "OriginalTraitName") %>%
  left_join(name_to_units, by = "OriginalTraitName") %>%
  replace_na(list(n_definitions = 1, n_units = 0))

print(top20)

# ── 8. Taxa analysis ──────────────────────────────────────────────
name_to_taxa <- rows %>%
  mutate(across(c(OriginalTraitName, Interactor1Genus), str_trim)) %>%
  filter(Interactor1Genus != "" & !is.na(Interactor1Genus)) %>%
  group_by(OriginalTraitName) %>%
  summarise(n_genera = n_distinct(Interactor1Genus)) %>%
  ungroup()

# ── 9. captilisation inconsistency ───────────────────────────────────────
capitalisation_issues <- unique_names %>%
  mutate(name_lower = str_to_lower(OriginalTraitName)) %>%
  group_by(name_lower) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(name_lower)

cat("\nCapitalisation inconsistencies:\n")
print(capitalisation_issues)

# ── 10. generate heterogeneity analysis CSV ──────────────────────────────────────────
output_table <- unique_names %>%
  rename(total_rows = n) %>%
  left_join(name_to_defs, by = "OriginalTraitName") %>%
  left_join(name_to_units, by = "OriginalTraitName") %>%
  left_join(name_to_taxa, by = "OriginalTraitName") %>%
  replace_na(list(n_definitions = 1, n_units = 0, n_genera = 0)) %>%
  mutate(
    taxa_specific = if_else(n_genera <= 3 & total_rows >= 100, "YES", ""),
    capitalisation_issue = if_else(
      str_to_lower(OriginalTraitName) %in%
        (capitalisation_issues %>% pull(name_lower)),
      "YES", ""
    )
  )

dir.create("Results", showWarnings = FALSE)
write_csv(output_table, "Results/heterogeneity_analysis.csv")
cat("Saved to Results/heterogeneity_analysis.csv\n")

# ── 11.  embedding input ───────────────────────────────
cat("\nPreparing text for embedding...\n")

unique_pairs <- rows %>%
  mutate(across(c(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit),
                str_trim)) %>%
  distinct(OriginalTraitName, OriginalTraitDef, .keep_all = TRUE) %>%
  select(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit) %>%
  mutate(
    text = paste0(
      "Trait: ", OriginalTraitName,
      ". Definition: ", OriginalTraitDef,
      ". Unit: ", OriginalTraitUnit, "."
    )
  )

cat("Total unique pairs to embed:", nrow(unique_pairs), "\n")
cat("Example:", unique_pairs$text[1], "\n")

# ── 12. generate Embeddings (via OpenAI API) ──────────────────────
library(httr)
library(jsonlite)

cat("\nGenerating embeddings with text-embedding-3-large...\n")

Sys.setenv(OPENAI_API_KEY = "")

get_embeddings <- function(texts, model = "text-embedding-3-large") {
  body <- list(
    input = as.list(texts),
    model = model
  )
  
  response <- POST(
    url = "https://api.openai.com/v1/embeddings",
    add_headers(
      "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      "Content-Type" = "application/json"
    ),
    body = toJSON(body, auto_unbox = TRUE),
    encode = "raw"
  )
  
  result <- content(response, as = "parsed")
  
  if (!is.null(result$error)) {
    cat("API Error:", result$error$message, "\n")
    return(NULL)
  }
  
  embeddings <- do.call(rbind, lapply(result$data, function(x) unlist(x$embedding)))
  return(embeddings)
}


batch_size <- 100
all_embeddings <- list()

for (i in seq(1, nrow(unique_pairs), by = batch_size)) {
  batch <- unique_pairs$text[i:min(i + batch_size - 1, nrow(unique_pairs))]
  cat(sprintf("Processing batch %d/%d...\n",
      ceiling(i/batch_size),
      ceiling(nrow(unique_pairs)/batch_size)))
  all_embeddings[[length(all_embeddings) + 1]] <- get_embeddings(batch)
}

embeddings <- do.call(rbind, all_embeddings)
cat("Embeddings shape:", dim(embeddings), "\n")


# ── 13. HDBSCAN clustering ──────────────────────────────────────────
cat("\nClustering...\n")

hdbscan_pkg <- import("hdbscan")

clusterer <- hdbscan_pkg$HDBSCAN(
  min_cluster_size = 5L,
  min_samples = 3L,
  metric = "euclidean"
)

cluster_labels <- clusterer$fit_predict(embeddings)

n_clusters <- length(unique(cluster_labels[cluster_labels != -1]))
n_noise <- sum(cluster_labels == -1)

cat("Number of clusters:", n_clusters, "\n")
cat("Noise points (unclustered):", n_noise, "\n")
cat("Clustered points:", length(cluster_labels) - n_noise, "\n")

# ── 14. Save clustering results ──────────────────────────────────────────
results <- unique_pairs %>%
  select(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit, text) %>%
  mutate(
    cluster_id = as.vector(cluster_labels),
    n_members = as.integer(table(as.vector(cluster_labels))[as.character(as.vector(cluster_labels))])
  ) %>%
  arrange(cluster_id)

write_csv(results, "Results/clustering_results_openai.csv")
cat("Saved to Results/clustering_results_openai.csv\n")

sink("Results/clustering_results_openai.txt")
for (cid in sort(unique(results$cluster_id))) {
  if (cid == -1) next
  members <- results %>% filter(cluster_id == cid)
  cat(sprintf("\n--- Cluster %d (%d members) ---\n", cid, nrow(members)))
  for (i in seq_len(nrow(members))) {
    cat(sprintf("  '%s' | %s | %s\n",
                members$OriginalTraitName[i],
                str_trunc(members$OriginalTraitDef[i], 50),
                members$OriginalTraitUnit[i]))
  }
}
noise <- results %>% filter(cluster_id == -1)
cat(sprintf("\n--- NOISE (unclustered, %d items) ---\n", nrow(noise)))
for (i in seq_len(nrow(noise))) {
  cat(sprintf("  '%s' | %s | %s\n",
              noise$OriginalTraitName[i],
              str_trunc(noise$OriginalTraitDef[i], 50),
              noise$OriginalTraitUnit[i]))
}
sink()

cat("Saved to Results/clustering_results_openai.txt\n")
cat("\nAll done!\n")
