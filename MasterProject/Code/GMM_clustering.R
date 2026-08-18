# ============================================================
# GMM Clustering — VecTraits V3
# ============================================================
library(tidyverse)
library(mclust)
library(httr)
library(jsonlite)

setwd("/Users/danielzhu/Documents/Master Project")
rows <- read_csv("Data/VecTraits.csv")


unique_pairs_v3 <- rows %>%
  mutate(across(c(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit), str_trim)) %>%
  distinct(OriginalTraitName, OriginalTraitDef, .keep_all = TRUE) %>%
  select(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit) %>%
  mutate(text = paste0(
    "Trait: ", OriginalTraitName,
    ". Definition: ", OriginalTraitDef,
    ". Unit: ", OriginalTraitUnit, "."
  ))


embedding_path <- "Results/embeddings_v3.rds"

if (file.exists(embedding_path)) {
  cat("Loading saved embeddings...\n")
  embeddings_v3 <- readRDS(embedding_path)
} else {
  cat("Generating embeddings...\n")
  
  Sys.setenv(OPENAI_API_KEY = "")
  
  get_embeddings <- function(texts, model = "text-embedding-3-large") {
    body <- list(input = as.list(texts), model = model)
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
    if (!is.null(result$error)) stop(result$error$message)
    do.call(rbind, lapply(result$data, function(x) unlist(x$embedding)))
  }
  
  batch_size <- 100
  all_embeddings <- list()
  for (i in seq(1, nrow(unique_pairs_v3), by = batch_size)) {
    batch <- unique_pairs_v3$text[i:min(i + batch_size - 1, nrow(unique_pairs_v3))]
    cat(sprintf("Batch %d/%d...\n", ceiling(i/batch_size), ceiling(nrow(unique_pairs_v3)/batch_size)))
    all_embeddings[[length(all_embeddings) + 1]] <- get_embeddings(batch)
  }
  
  embeddings_v3 <- do.call(rbind, all_embeddings)
  saveRDS(embeddings_v3, embedding_path)
  cat("Embeddings saved to", embedding_path, "\n")
}


cat("Running PCA...\n")
pca <- prcomp(embeddings_v3, center = TRUE, scale. = FALSE)


var_explained <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
n_components <- which(var_explained >= 0.90)[1]
cat(sprintf("PCA: %d components explain 90%% variance\n", n_components))

embeddings_pca <- pca$x[, 1:n_components]


cat("Running GMM (this may take a few minutes)...\n")

gmm <- Mclust(embeddings_pca, G = 2:40, verbose = TRUE)

cat(sprintf("Optimal k: %d clusters\n", gmm$G))
cat(sprintf("Best model: %s\n", gmm$modelName))
cat(sprintf("BIC: %.2f\n", gmm$bic))


results_gmm <- unique_pairs_v3 %>%
  mutate(
    cluster_id = as.integer(gmm$classification),
    uncertainty = as.numeric(gmm$uncertainty)
  ) %>%
  arrange(cluster_id)

write_csv(results_gmm, "Results/clustering_gmm_v3.csv")


cluster_sizes <- as.data.frame(table(results_gmm$cluster_id))
colnames(cluster_sizes) <- c("cluster_id", "n")
cluster_sizes <- cluster_sizes[order(-cluster_sizes$n), ]
cat("\nCluster size distribution:\n")
print(cluster_sizes)
cat(sprintf("\nAvg cluster size: %.1f\n", mean(cluster_sizes$n)))


sink("Results/clustering_gmm_v3.txt")
for (cid in sort(unique(results_gmm$cluster_id))) {
  members <- results_gmm %>% filter(cluster_id == cid)
  cat(sprintf("\n--- Cluster %d (%d members) ---\n", cid, nrow(members)))
  for (i in seq_len(nrow(members))) {
    cat(sprintf("  '%s' | %s | %s\n",
                members$OriginalTraitName[i],
                str_trunc(members$OriginalTraitDef[i], 50),
                members$OriginalTraitUnit[i]))
  }
}
sink()

cat("\nDone! Results saved to Results/clustering_gmm_v3.csv and .txt\n")
