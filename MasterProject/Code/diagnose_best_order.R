# ============================================================
# Diagnose the 0.585 "optimal" 20-field ordering
# ============================================================
# Question: is silhouette 0.585 real (semantic clusters) or an
# artifact (clusters formed by repeated geographic/numeric values
# like longitude / coordinate type / temperature unit)?
#
# We re-cluster under the search's best ordering, then for each
# cluster print: the trait names inside, and the spread of the
# geographic/numeric fields. If a cluster has ONE longitude/coord
# value but MANY different traits -> it's grouping by geography,
# not by trait meaning (the artifact we suspect).
#
# Prereqs in session: rows, get_embeddings, hdbscan_pkg, API key.
# ------------------------------------------------------------

library(tidyverse)
library(reticulate)
library(digest)

setwd("/Users/danielzhu/Documents/Master Project")

# The best ordering found by the search (silhouette 0.5853)
best_order <- c("long","genus","family","location","tempunit","order",
                "kingdom","habitat","lat","species","temp","class",
                "loctype","coordtype","stage","labfield","name","def",
                "unit","phylum")

# field key -> column + label (same as order_search.R)
fields20 <- tribble(
  ~key,        ~col,                  ~block_label,
  "name",      "OriginalTraitName",   "Trait",
  "def",       "OriginalTraitDef",    "Definition",
  "unit",      "OriginalTraitUnit",   "Unit",
  "kingdom",   "Interactor1Kingdom",  "Organism kingdom",
  "phylum",    "Interactor1Phylum",   "Organism phylum",
  "class",     "Interactor1Class",    "Organism class",
  "order",     "Interactor1Order",    "Organism order",
  "family",    "Interactor1Family",   "Organism family",
  "genus",     "Interactor1Genus",    "Organism genus",
  "species",   "Interactor1Species",  "Organism species",
  "stage",     "Interactor1Stage",    "Life stage",
  "habitat",   "Habitat",             "Habitat",
  "labfield",  "LabField",            "Lab or field",
  "location",  "Location",            "Location",
  "loctype",   "LocationType",        "Location type",
  "lat",       "Latitude",            "Latitude",
  "long",      "Longitude",           "Longitude",
  "coordtype", "CoordinateType",      "Coordinate type",
  "temp",      "Interactor1Temp",     "Temperature",
  "tempunit",  "Interactor1TempUnit", "Temperature unit"
)

all_cols <- fields20$col
base_tbl <- rows %>%
  mutate(across(all_of(all_cols), ~ str_trim(as.character(.)))) %>%
  distinct(OriginalTraitName, OriginalTraitDef, .keep_all = TRUE) %>%
  select(all_of(all_cols))
cat("Base rows:", nrow(base_tbl), "\n")

make_block <- function(col, label) {
  v <- base_tbl[[col]]; v <- ifelse(is.na(v) | v=="", "not specified", v)
  paste0(label, ": ", v)
}
block_strings <- Map(make_block, fields20$col, fields20$block_label)
names(block_strings) <- fields20$key

assemble_text <- function(keys) {
  paste0(do.call(paste, c(block_strings[keys], sep=". ")), ".")
}

# reuse the search cache so we don't pay again
embed_cached <- function(texts, tag) {
  key <- digest(list(tag, texts), algo="md5")
  cf  <- sprintf("Results/ordersearch_cache/%s.rds", key)
  if (file.exists(cf)) { x<-readRDS(cf); if(!is.null(x)&&nrow(x)==length(texts)) return(x); unlink(cf) }
  acc<-list()
  for (i in seq(1,length(texts),by=100)) {
    b<-texts[i:min(i+99,length(texts))]; e<-get_embeddings(b)
    if (is.null(e)) stop("API NULL"); acc[[length(acc)+1]]<-e
  }
  emb<-do.call(rbind,acc); saveRDS(emb,cf); emb
}

# cluster under the best ordering
txt    <- assemble_text(best_order)
emb    <- embed_cached(txt, tag = paste(best_order, collapse=">"))
cl     <- hdbscan_pkg$HDBSCAN(min_cluster_size=5L, min_samples=3L, metric="euclidean")
labels <- as.vector(cl$fit_predict(emb))

diag_tbl <- base_tbl %>%
  mutate(cluster = labels) %>%
  select(cluster,
         trait = OriginalTraitName,
         genus = Interactor1Genus,
         family = Interactor1Family,
         longitude = Longitude,
         latitude  = Latitude,
         loctype   = LocationType,
         coordtype = CoordinateType,
         temp      = Interactor1Temp)

# ── Per-cluster diagnostic: is it ONE geography but MANY traits? ──
cat("\n=== CLUSTER COMPOSITION (excluding noise) ===\n")
summ <- diag_tbl %>%
  filter(cluster != -1) %>%
  group_by(cluster) %>%
  summarise(
    n              = n(),
    n_distinct_trait = n_distinct(trait),
    n_distinct_long  = n_distinct(longitude),
    n_distinct_lat   = n_distinct(latitude),
    n_distinct_coord = n_distinct(coordtype),
    n_distinct_temp  = n_distinct(temp),
    .groups="drop"
  ) %>%
  arrange(desc(n))
print(summ, n = 40)

cat("\nINTERPRETATION KEY:\n")
cat(" If n_distinct_long / coord / temp are SMALL (~1) but n_distinct_trait is LARGE,\n")
cat(" the cluster is grouping by GEOGRAPHY/NUMERIC value, not by trait meaning.\n")
cat(" If n_distinct_trait is SMALL (traits are semantically consistent), it's genuine.\n")

# ── Print contents of the 5 largest clusters in detail ───────────
top_clusters <- summ$cluster[1:min(5, nrow(summ))]
for (cc in top_clusters) {
  cat(sprintf("\n----- CLUSTER %s (size %d) -----\n", cc, sum(diag_tbl$cluster==cc)))
  sub <- diag_tbl %>% filter(cluster==cc)
  cat("Distinct traits in this cluster:\n")
  print(sort(unique(sub$trait)))
  cat(sprintf("Longitude values: %s\n", paste(unique(sub$longitude), collapse=", ")))
  cat(sprintf("CoordinateType values: %s\n", paste(unique(sub$coordtype), collapse=", ")))
  cat(sprintf("Temp values: %s\n", paste(unique(sub$temp), collapse=", ")))
}

write_csv(diag_tbl, "Results/ordersearch_best_cluster_contents.csv")
write_csv(summ,     "Results/ordersearch_best_cluster_summary.csv")
cat("\nSaved: Results/ordersearch_best_cluster_contents.csv\n")
cat("Saved: Results/ordersearch_best_cluster_summary.csv\n")
