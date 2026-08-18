# ============================================================
# Optimal Ordering Search (20 semantic fields, VecTraits)
# ============================================================
# GOAL (per Samraat): the 20-field permutation space is far too large
# to enumerate (20! ~ 2.4e18). Instead, SEARCH it with simulated
# annealing to "zero in on" the ordering that MAXIMISES silhouette.
#
# CREDIBILITY CHECK: first run the SAME search on the 6 V6 fields,
# where we already have the true optimum from the exhaustive 720-run
# (order>family>name>def>stage>unit, silhouette 0.363). If the search
# recovers (near-)that optimum on the solvable 6-field case, we can
# trust its 20-field result, which we cannot verify by enumeration.
#
# Field set = 20 semantic columns (metadata + Interactor1 full name
# excluded, as previously justified to Samraat). Dedup = distinct
# (name, def) -> 461 rows. Concatenation = name-led blocks joined ". ".
# Empty -> "not specified". Hardened cache (fail-fast, never cache NULL).
#
# Prereqs in session: rows, get_embeddings, hdbscan_pkg, OPENAI_API_KEY,
# pymc-env with scikit-learn.
# ------------------------------------------------------------

library(tidyverse)
library(reticulate)
library(digest)

setwd("/Users/danielzhu/Documents/Master Project")
dir.create("Results/ordersearch_cache", showWarnings = FALSE)

sklearn <- import("sklearn.metrics")

# ── 1. The 20 semantic fields (col -> sentence-block label) ──────
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

# 6-field set (for the credibility check), same labels as V6
fields6_keys <- c("name","def","unit","order","family","stage")

# ── 2. Build the 461-row base table (unified dedup) ──────────────
all_cols <- fields20$col
base_tbl <- rows %>%
  mutate(across(all_of(all_cols), ~ str_trim(as.character(.)))) %>%
  distinct(OriginalTraitName, OriginalTraitDef, .keep_all = TRUE) %>%
  select(all_of(all_cols))
N_ROWS <- nrow(base_tbl)
cat("Base rows:", N_ROWS, "\n")

# Pre-build each field's text block (with "not specified" fallback)
make_block <- function(col, label) {
  v <- base_tbl[[col]]
  v <- ifelse(is.na(v) | v == "", "not specified", v)
  paste0(label, ": ", v)
}
block_strings <- Map(make_block, fields20$col, fields20$block_label)
names(block_strings) <- fields20$key

# Assemble text for a given ordering (vector of field keys)
assemble_text <- function(order_keys) {
  parts <- block_strings[order_keys]
  paste0(do.call(paste, c(parts, sep = ". ")), ".")
}

# ── 3. Hardened cached embedding + HDBSCAN + silhouette ──────────
embed_cached <- function(texts, tag) {
  key <- digest(list(tag, texts), algo = "md5")
  cf  <- sprintf("Results/ordersearch_cache/%s.rds", key)
  if (file.exists(cf)) {
    x <- readRDS(cf)
    if (!is.null(x) && nrow(x) == length(texts)) return(x)
    unlink(cf)
  }
  acc <- list()
  for (i in seq(1, length(texts), by = 100)) {
    b <- texts[i:min(i+99, length(texts))]
    e <- get_embeddings(b)
    if (is.null(e)) stop(sprintf("Embedding failed (%s): API returned NULL.", tag))
    acc[[length(acc)+1]] <- e
  }
  emb <- do.call(rbind, acc)
  stopifnot(!is.null(emb), nrow(emb) == length(texts))
  saveRDS(emb, cf); emb
}

run_hdbscan <- function(emb) {
  cl <- hdbscan_pkg$HDBSCAN(min_cluster_size = 5L, min_samples = 3L, metric = "euclidean")
  as.vector(cl$fit_predict(emb))
}

silhouette_score <- function(order_keys) {
  txt    <- assemble_text(order_keys)
  emb    <- embed_cached(txt, tag = paste(order_keys, collapse = ">"))
  labels <- run_hdbscan(emb)
  keep   <- labels != -1
  lab    <- labels[keep]
  if (length(unique(lab)) < 2) return(list(sil = -1, labels = labels))  # degenerate
  sub <- emb[keep, , drop = FALSE]
  s <- tryCatch(sklearn$silhouette_score(sub, as.integer(lab), metric = "euclidean"),
                error = function(e) -1)
  # degeneracy guard: penalise if one cluster swallows >50% of points
  tab <- as.data.frame(table(lab))
  max_share <- max(tab$Freq) / length(labels)
  if (max_share > 0.5) s <- s - 1   # heavy penalty so search avoids these
  list(sil = s, labels = labels, max_share = max_share)
}

# ── 4. Simulated annealing over orderings ────────────────────────
# Neighbour move = swap two random positions. Accept better always;
# accept worse with probability exp(delta/T); T cools each step.
simulated_anneal <- function(field_keys, max_evals = 400,
                              T0 = 0.05, cooling = 0.99, seed = 1,
                              label = "search") {
  set.seed(seed)
  current <- sample(field_keys)                 # random start
  cur_sil <- silhouette_score(current)$sil
  best <- current; best_sil <- cur_sil
  T <- T0
  trace <- data.frame(eval = 1, current_sil = cur_sil, best_sil = best_sil)
  evals <- 1
  cat(sprintf("[%s] eval %d/%d  start sil=%.4f\n", label, evals, max_evals, cur_sil))

  while (evals < max_evals) {
    cand <- current
    ij <- sample(length(cand), 2)
    cand[ij] <- cand[rev(ij)]                   # swap two positions
    cand_sil <- silhouette_score(cand)$sil
    evals <- evals + 1

    delta <- cand_sil - cur_sil
    if (delta > 0 || runif(1) < exp(delta / T)) {  # accept
      current <- cand; cur_sil <- cand_sil
      if (cur_sil > best_sil) { best <- current; best_sil <- cur_sil }
    }
    T <- T * cooling
    trace <- rbind(trace, data.frame(eval = evals, current_sil = cur_sil, best_sil = best_sil))
    if (evals %% 25 == 0)
      cat(sprintf("[%s] eval %d/%d  cur=%.4f  best=%.4f  T=%.4f\n",
                  label, evals, max_evals, cur_sil, best_sil, T))
  }
  list(best_order = best, best_sil = best_sil, trace = trace)
}

# ── 5. CREDIBILITY CHECK on 6 fields (known optimum = 0.363) ─────
cat("\n=== STEP A: validate search on 6 fields (known true optimum) ===\n")
res6 <- simulated_anneal(fields6_keys, max_evals = 120, seed = 42, label = "6f")
cat(sprintf("\n6-field search best: %s\n  silhouette = %.4f\n",
            paste(res6$best_order, collapse = ">"), res6$best_sil))
cat("Known true optimum (from 720 enumeration): order>family>name>def>stage>unit, sil=0.363\n")
cat("--> If the search best is at/near 0.363, the method is trustworthy.\n")

# ── 6. MAIN SEARCH on 20 fields ──────────────────────────────────
cat("\n=== STEP B: search 20-field space for optimal ordering ===\n")
res20 <- simulated_anneal(fields20$key, max_evals = 400, seed = 7, label = "20f")
cat(sprintf("\n20-field search best ordering:\n  %s\n  silhouette = %.4f\n",
            paste(res20$best_order, collapse = " > "), res20$best_sil))

# ── 7. Save results ──────────────────────────────────────────────
write_csv(res6$trace,  "Results/ordersearch_trace_6field.csv")
write_csv(res20$trace, "Results/ordersearch_trace_20field.csv")
writeLines(c(
  "=== 6-field validation ===",
  paste("search best :", paste(res6$best_order, collapse=">")),
  sprintf("search sil  : %.4f", res6$best_sil),
  "true optimum: order>family>name>def>stage>unit (sil 0.363, from 720 enumeration)",
  "",
  "=== 20-field search ===",
  paste("best ordering:", paste(res20$best_order, collapse=" > ")),
  sprintf("best silhouette: %.4f", res20$best_sil)
), "Results/ordersearch_summary.txt")

# Convergence plot (both searches)
trace_all <- bind_rows(
  res6$trace  %>% mutate(search = "6-field (validation)"),
  res20$trace %>% mutate(search = "20-field (main)")
)
p <- ggplot(trace_all, aes(eval, best_sil)) +
  geom_line(colour = "steelblue", linewidth = 0.8) +
  facet_wrap(~ search, scales = "free", ncol = 1) +
  labs(title = "Simulated annealing: best silhouette found vs evaluations",
       x = "Evaluation number", y = "Best silhouette so far") +
  theme_minimal(base_size = 10)
ggsave("Results/ordersearch_convergence.png", p, width = 8, height = 7, dpi = 150)

cat("\nSaved:\n  Results/ordersearch_summary.txt\n",
    "  Results/ordersearch_trace_6field.csv / _20field.csv\n",
    "  Results/ordersearch_convergence.png\n")
cat("Done.\n")
