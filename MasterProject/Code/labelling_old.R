# ============================================================
# Step 6: LLM Cluster Labeling with OBA/PATO Context
# ============================================================

library(tidyverse)
library(httr)
library(jsonlite)

setwd("/Users/danielzhu/Documents/Master Project")

Sys.setenv(OPENAI_API_KEY = "")


clustering <- read_csv("Results/clustering_results_openai.csv")
mapping    <- read_csv("Results/ontology_mapping_results.csv")
summary    <- read_csv("Results/cluster_ontology_summary.csv")


label_cluster_with_context <- function(cluster_id, members, ontology_context) {
  

  members_text <- paste(
    apply(members, 1, function(row) {
      paste0("- '", row["OriginalTraitName"], "': ", row["OriginalTraitDef"])
    }),
    collapse = "\n"
  )
  

  if (nrow(ontology_context) > 0) {
    onto_text <- paste(
      apply(ontology_context, 1, function(row) {
        paste0("- [", row["confidence"], " confidence] ",
               row["best_match_id"], " '", row["best_match_label"], "'")
      }),
      collapse = "\n"
    )
    onto_section <- paste0(
      "Ontology mappings for members of this cluster:\n", onto_text, "\n\n"
    )
  } else {
    onto_section <- "No ontology mappings found for this cluster.\n\n"
  }
  
  prompt <- paste0(
    "You are an expert in vector biology and ecological trait databases. ",
    "Your task is to assign a standardised label to a cluster of related trait descriptions.\n\n",
    "Cluster ", cluster_id, " members (", nrow(members), " traits):\n",
    members_text, "\n\n",
    onto_section,
    "Instructions:\n",
    "1. Generate a standardised trait label (2-5 words, lowercase) that best represents ALL members\n",
    "2. If ontology mappings are available and appropriate, use them to inform your label\n",
    "3. If the ontology terms are too broad or don't fit, generate a more specific label\n",
    "4. Provide a one-sentence justification\n",
    "5. Note whether your label aligns with, refines, or extends beyond existing ontology terms\n\n",
    "Respond ONLY in JSON:\n",
    '{"standardised_label": "...", ',
    '"ontology_alignment": "aligns/refines/extends", ',
    '"best_ontology_reference": "OBA:XXXXXXX or PATO:XXXXXXX or none", ',
    '"justification": "..."}'
  )
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(
      "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      "Content-Type" = "application/json"
    ),
    body = toJSON(list(
      model = "gpt-4o",
      messages = list(list(role = "user", content = prompt)),
      temperature = 0.1
    ), auto_unbox = TRUE),
    encode = "raw"
  )
  
  result <- content(response, as = "parsed")
  
  if (!is.null(result$error)) {
    cat("API Error:", result$error$message, "\n")
    return(list(
      standardised_label     = "ERROR",
      ontology_alignment     = "none",
      best_ontology_reference = "none",
      justification          = result$error$message
    ))
  }
  
  text <- result$choices[[1]]$message$content
  text <- str_remove_all(text, "```json|```")
  text <- str_trim(text)
  
  tryCatch({
    fromJSON(text)
  }, error = function(e) {
    list(
      standardised_label      = "PARSE_ERROR",
      ontology_alignment      = "none",
      best_ontology_reference = "none",
      justification           = text
    )
  })
}


cluster_ids <- sort(unique(clustering$cluster_id))
cluster_ids <- cluster_ids[cluster_ids != -1]

cat("Labeling", length(cluster_ids), "clusters...\n\n")

labels <- list()

for (cid in cluster_ids) {
  cat(sprintf("Processing Cluster %d...\n", cid))
  

  members <- clustering %>%
    filter(cluster_id == cid) %>%
    select(OriginalTraitName, OriginalTraitDef) %>%
    distinct()
  

  onto_context <- mapping %>%
    filter(trait_name %in% members$OriginalTraitName) %>%
    filter(!best_match_id %in% c("NO_MATCH", "NO_CANDIDATES", "ERROR", "PARSE_ERROR")) %>%
    filter(confidence %in% c("high", "medium")) %>%
    select(trait_name, best_match_id, best_match_label, confidence) %>%
    distinct(best_match_label, .keep_all = TRUE) %>%
    head(5)  
  
  result <- label_cluster_with_context(cid, members, onto_context)
  
  labels[[length(labels) + 1]] <- data.frame(
    cluster_id              = cid,
    n_members               = nrow(members),
    standardised_label      = result$standardised_label,
    ontology_alignment      = result$ontology_alignment,
    best_ontology_reference = result$best_ontology_reference,
    justification           = result$justification,
    stringsAsFactors        = FALSE
  )
  
  cat(sprintf("  -> %s [%s]\n", result$standardised_label, result$ontology_alignment))
  Sys.sleep(0.3)
}


labels_df <- do.call(rbind, labels)

cat("\n=== Labeling Results ===\n")
print(labels_df %>% select(cluster_id, n_members, standardised_label, ontology_alignment))

cat("\nOntology alignment distribution:\n")
print(table(labels_df$ontology_alignment))


final_results <- clustering %>%
  left_join(labels_df %>% select(cluster_id, standardised_label, 
                                 ontology_alignment, best_ontology_reference),
            by = "cluster_id") %>%
  mutate(
    standardised_label = ifelse(cluster_id == -1, "NOISE", standardised_label)
  )


write_csv(labels_df,     "Results/cluster_labels_with_ontology.csv")
write_csv(final_results, "Results/final_controlled_vocabulary.csv")

cat("\nSaved to Results/\n")
cat("All done!\n")
