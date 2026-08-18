library(reticulate)
use_condaenv("pymc-env", required = TRUE)
py_config()
# ============================================================
# BioTraits GlobalDataset — Clustering Pipeline (V1-V6)
# ============================================================
library(tidyverse)
library(reticulate)


# Sys.setenv(RETICULATE_PYTHON = "/path/to/your/python")  

setwd("/Users/danielzhu/Documents/Master Project")


biotraits <- read_csv(
  "Data/GlobalDataset.csv",
  locale = locale(encoding = "ISO-8859-1"),
  show_col_types = FALSE
)


unique_pairs <- biotraits %>%
  mutate(across(c(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit,
                  ConOrder, ConFamily, ConStage),
                ~str_trim(as.character(.)))) %>%
  distinct(OriginalTraitName, OriginalTraitDef, .keep_all = TRUE) %>%
  select(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit,
         ConOrder, ConFamily, ConStage)

cat(sprintf("Unique (name+def) pairs: %d\n", nrow(unique_pairs)))


build_text <- function(df, version) {
  txt <- paste0("Trait: ", df$OriginalTraitName, ".")
  
  if (version >= 2) {
    def <- ifelse(is.na(df$OriginalTraitDef) | df$OriginalTraitDef == "",
                  "", paste0(" Definition: ", df$OriginalTraitDef, "."))
    txt <- paste0(txt, def)
  }
  if (version >= 3) {
    unit <- ifelse(is.na(df$OriginalTraitUnit) | df$OriginalTraitUnit == "",
                   "", paste0(" Unit: ", df$OriginalTraitUnit, "."))
    txt <- paste0(txt, unit)
  }
  if (version >= 4) {
    ord <- ifelse(is.na(df$ConOrder) | df$ConOrder == "",
                  "", paste0(" Organism order: ", df$ConOrder, "."))
    txt <- paste0(txt, ord)
  }
  if (version >= 5) {
    fam <- ifelse(is.na(df$ConFamily) | df$ConFamily == "",
                  "", paste0(" Organism family: ", df$ConFamily, "."))
    txt <- paste0(txt, fam)
  }
  if (version >= 6) {
    stg <- ifelse(is.na(df$ConStage) | df$ConStage == "",
                  "", paste0(" Life stage: ", df$ConStage, "."))
    txt <- paste0(txt, stg)
  }
  txt
}


openai <- import("openai")
hdbscan <- import("hdbscan")
np <- import("numpy")
sklearn_prep <- import("sklearn.preprocessing")

client <- openai$OpenAI(api_key = "")

get_embeddings <- function(texts, model = "text-embedding-3-large", batch_size = 100) {
  all_emb <- list()
  n <- length(texts)
  for (i in seq(1, n, by = batch_size)) {
    idx <- i:min(i + batch_size - 1, n)
    cat(sprintf("  Batch %d/%d...\n", ceiling(i/batch_size), ceiling(n/batch_size)))
    resp <- client$embeddings$create(input = as.list(texts[idx]), model = model)
    batch_emb <- lapply(resp$data, function(x) x$embedding)
    all_emb <- c(all_emb, batch_emb)
  }
  do.call(rbind, lapply(all_emb, unlist))
}


results <- list()

for (v in 1:6) {
  cat(sprintf("\n=== Version %d ===\n", v))
  
  texts <- build_text(unique_pairs, v)
  
  # embedding
  emb <- get_embeddings(texts)
  emb_norm <- sklearn_prep$normalize(emb)
  

  np$save(sprintf("Results/biotraits_embeddings_v%d.npy", v), emb_norm)
  
  # HDBSCAN
  clusterer <- hdbscan$HDBSCAN(
    min_cluster_size = 5L,
    min_samples = 3L,
    metric = "euclidean",
    gen_min_span_tree = TRUE
  )
  labels <- clusterer$fit_predict(emb_norm)
  labels <- as.integer(labels)
  

  n_clusters <- length(unique(labels[labels != -1]))
  n_noise    <- sum(labels == -1)
  noise_rate <- n_noise / length(labels)
  
  sizes <- as.data.frame(table(labels[labels != -1]))
  avg_size <- mean(sizes$Freq)
  
  cat(sprintf("  Clusters: %d, Noise: %d (%.1f%%)\n",
              n_clusters, n_noise, noise_rate * 100))
  
  results[[v]] <- tibble(
    version    = sprintf("v%d", v),
    x          = v,
    n_clusters = n_clusters,
    n_noise    = n_noise,
    noise_rate = noise_rate,
    avg_size   = avg_size
  )
  

  out <- unique_pairs %>%
    mutate(text = texts, version = sprintf("v%d", v), cluster_id = labels)
  write_csv(out, sprintf("Results/biotraits_clustering_v%d.csv", v))
}


summary_df <- bind_rows(results)
write_csv(summary_df, "Results/biotraits_rarefaction.csv")

cat("\n=== Summary ===\n")
print(summary_df)
