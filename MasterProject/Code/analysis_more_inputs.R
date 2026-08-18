# ── Version 1: name only ──────────────────────────────────────────
unique_pairs_v1 <- rows %>%
  mutate(OriginalTraitName = str_trim(OriginalTraitName)) %>%
  distinct(OriginalTraitName, .keep_all = TRUE) %>%
  select(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit) %>%
  mutate(text = paste0("Trait: ", OriginalTraitName, "."))

# ── Version 2: name + definition ─────────────────────────────────
unique_pairs_v2 <- rows %>%
  mutate(across(c(OriginalTraitName, OriginalTraitDef), str_trim)) %>%
  distinct(OriginalTraitName, OriginalTraitDef, .keep_all = TRUE) %>%
  select(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit) %>%
  mutate(text = paste0(
    "Trait: ", OriginalTraitName,
    ". Definition: ", OriginalTraitDef, "."
  ))

# ── Version 3: name + definition + unit (baseline) ───────────────
unique_pairs_v3 <- rows %>%
  mutate(across(c(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit), str_trim)) %>%
  distinct(OriginalTraitName, OriginalTraitDef, .keep_all = TRUE) %>%
  select(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit) %>%
  mutate(text = paste0(
    "Trait: ", OriginalTraitName,
    ". Definition: ", OriginalTraitDef,
    ". Unit: ", OriginalTraitUnit, "."
  ))
# ── Version 4: name + definition + unit + kingdom ────────────────
unique_pairs_v4 <- rows %>%
  mutate(across(c(OriginalTraitName, OriginalTraitDef, 
                  OriginalTraitUnit, Interactor1Order), str_trim)) %>%
  distinct(OriginalTraitName, OriginalTraitDef, .keep_all = TRUE) %>%
  select(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit, Interactor1Order) %>%
  mutate(text = paste0(
    "Trait: ", OriginalTraitName,
    ". Definition: ", OriginalTraitDef,
    ". Unit: ", OriginalTraitUnit,
    ". Organism order: ", Interactor1Order, "."
  ))
# V5: name + def + unit + order + family
unique_pairs_v5 <- rows %>%
  mutate(across(c(OriginalTraitName, OriginalTraitDef, 
                  OriginalTraitUnit, Interactor1Order, Interactor1Family), str_trim)) %>%
  distinct(OriginalTraitName, OriginalTraitDef, .keep_all = TRUE) %>%
  select(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit, 
         Interactor1Order, Interactor1Family) %>%
  mutate(text = paste0(
    "Trait: ", OriginalTraitName,
    ". Definition: ", OriginalTraitDef,
    ". Unit: ", OriginalTraitUnit,
    ". Organism order: ", Interactor1Order,
    ". Organism family: ", Interactor1Family, "."
  ))

# V6: name + def + unit + order + family + stage
unique_pairs_v6 <- rows %>%
  mutate(across(c(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit, 
                  Interactor1Order, Interactor1Family, Interactor1Stage), str_trim)) %>%
  distinct(OriginalTraitName, OriginalTraitDef, .keep_all = TRUE) %>%
  select(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit,
         Interactor1Order, Interactor1Family, Interactor1Stage) %>%
  mutate(text = paste0(
    "Trait: ", OriginalTraitName,
    ". Definition: ", OriginalTraitDef,
    ". Unit: ", OriginalTraitUnit,
    ". Organism order: ", Interactor1Order,
    ". Organism family: ", Interactor1Family,
    ". Life stage: ", ifelse(is.na(Interactor1Stage) | Interactor1Stage == "",
                             "not specified", Interactor1Stage), "."
  ))

cat("V1 (name only):", nrow(unique_pairs_v1), "pairs\n")
cat("V2 (name + def):", nrow(unique_pairs_v2), "pairs\n")
cat("V3 (name + def + unit):", nrow(unique_pairs_v3), "pairs\n")
cat("V4 (name + def + unit + order):", nrow(unique_pairs_v4), "pairs\n")



versions <- list(
  v1 = unique_pairs_v1,
  v2 = unique_pairs_v2,
  v3 = unique_pairs_v3,
  v4 = unique_pairs_v4,
  v5 = unique_pairs_v5,
  v6 = unique_pairs_v6
)

results_summary <- data.frame()

for (version_name in names(versions)) {
  cat(sprintf("\n=== Running %s ===\n", version_name))
  
  # Embedding
  pairs <- versions[[version_name]]
  batch_size <- 100
  all_embeddings <- list()
  
  for (i in seq(1, nrow(pairs), by = batch_size)) {
    batch <- pairs$text[i:min(i + batch_size - 1, nrow(pairs))]
    cat(sprintf("Embedding batch %d/%d...\n",
                ceiling(i/batch_size),
                ceiling(nrow(pairs)/batch_size)))
    all_embeddings[[length(all_embeddings) + 1]] <- get_embeddings(batch)
  }
  
  embeddings <- do.call(rbind, all_embeddings)
  
  # Clustering
  clusterer <- hdbscan_pkg$HDBSCAN(
    min_cluster_size = 5L,
    min_samples = 3L,
    metric = "euclidean"
  )
  
  cluster_labels <- clusterer$fit_predict(embeddings)
  
  n_clusters <- length(unique(cluster_labels[cluster_labels != -1]))
  n_noise <- sum(cluster_labels == -1)
  
  cat(sprintf("Clusters: %d | Noise: %d | Clustered: %d\n",
              n_clusters, n_noise, length(cluster_labels) - n_noise))
  

  result <- pairs %>%
    mutate(
      version = version_name,
      cluster_id = as.vector(cluster_labels)
    )
  
  write_csv(result, sprintf("Results/clustering_%s.csv", version_name))
  
  results_summary <- rbind(results_summary, data.frame(
    version = version_name,
    n_pairs = nrow(pairs),
    n_clusters = n_clusters,
    n_noise = n_noise,
    n_clustered = length(cluster_labels) - n_noise
  ))
}

cat("\n=== SUMMARY ===\n")
print(results_summary)
write_csv(results_summary, "Results/feature_comparison.csv")
