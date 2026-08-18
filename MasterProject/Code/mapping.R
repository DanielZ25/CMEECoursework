# ============================================================
# Step 5: Cross-reference Clustering with OBA/PATO Mapping
# ============================================================

library(tidyverse)

setwd("/Users/danielzhu/Documents/Master Project")


clustering <- read_csv("Results/clustering_results_openai.csv")
mapping    <- read_csv("Results/ontology_mapping_results.csv")

cat("Clustering results:", nrow(clustering), "\n")
cat("Ontology mapping results:", nrow(mapping), "\n")


combined <- clustering %>%
  left_join(
    mapping %>% select(trait_name, trait_def, best_match_id, 
                       best_match_label, confidence, reasoning),
    by = c("OriginalTraitName" = "trait_name",
           "OriginalTraitDef"  = "trait_def")
  ) %>%
  mutate(
    is_mapped = !best_match_id %in% c("NO_MATCH", "NO_CANDIDATES", 
                                      "ERROR", "PARSE_ERROR", NA)
  )

cat("Combined rows:", nrow(combined), "\n")


cluster_summary <- combined %>%
  filter(cluster_id != -1) %>%
  group_by(cluster_id) %>%
  summarise(
    n_members      = n(),
    n_mapped       = sum(is_mapped, na.rm = TRUE),
    n_unmapped     = sum(!is_mapped, na.rm = TRUE),
    mapping_rate   = round(100 * n_mapped / n(), 1),

    top_ontology_term = {
      mapped_terms <- best_match_label[is_mapped & !is.na(best_match_label)]
      if (length(mapped_terms) > 0) {
        names(sort(table(mapped_terms), decreasing = TRUE))[1]
      } else {
        "none"
      }
    },
    top_term_count = {
      mapped_terms <- best_match_label[is_mapped & !is.na(best_match_label)]
      if (length(mapped_terms) > 0) {
        max(table(mapped_terms))
      } else {
        0L
      }
    },
    trait_names    = paste(unique(OriginalTraitName), collapse = " | "),
    n_high_conf    = sum(confidence == "high" & is_mapped, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    anchor_quality = case_when(
      mapping_rate >= 75 ~ "well-anchored",
      mapping_rate >= 40 ~ "partially-anchored",
      TRUE               ~ "novel-contribution"
    )
  ) %>%
  arrange(desc(mapping_rate))

cat("\n=== Cluster Mapping Summary ===\n")
print(cluster_summary %>% 
        select(cluster_id, n_members, n_mapped, mapping_rate, 
               top_ontology_term, anchor_quality))


noise_summary <- combined %>%
  filter(cluster_id == -1) %>%
  summarise(
    n_noise    = n(),
    n_mapped   = sum(is_mapped, na.rm = TRUE),
    n_unmapped = sum(!is_mapped, na.rm = TRUE),
    mapping_rate = round(100 * n_mapped / n(), 1)
  )

cat("\n=== Noise Points Mapping ===\n")
print(noise_summary)


cat("\n=== Overall Statistics ===\n")
cat("Well-anchored clusters (>=75% mapped):", 
    sum(cluster_summary$anchor_quality == "well-anchored"), "\n")
cat("Partially-anchored clusters (40-75%):", 
    sum(cluster_summary$anchor_quality == "partially-anchored"), "\n")
cat("Novel contribution clusters (<40%):", 
    sum(cluster_summary$anchor_quality == "novel-contribution"), "\n")


write_csv(combined,        "Results/combined_results.csv")
write_csv(cluster_summary, "Results/cluster_ontology_summary.csv")

cat("\nSaved to Results/\n")
cat("All done!\n")
