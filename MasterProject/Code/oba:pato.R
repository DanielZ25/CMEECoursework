# ============================================================
# Step 3: OBA/PATO Matching via GPT-4o
# ============================================================

library(tidyverse)
library(httr)
library(jsonlite)

setwd("/Users/danielzhu/Documents/Master Project")

Sys.setenv(OPENAI_API_KEY = "")


ontology_matches <- read_csv("Results/ontology_matches.csv")
unique_pairs <- read_csv("Results/clustering_results_openai.csv") %>%
  select(OriginalTraitName, OriginalTraitDef, OriginalTraitUnit) %>%
  distinct(OriginalTraitName, OriginalTraitDef, .keep_all = TRUE)

cat("Unique VecTraits pairs:", nrow(unique_pairs), "\n")
cat("Ontology candidates:", nrow(ontology_matches), "\n")


match_to_ontology <- function(trait_name, trait_def, trait_unit, candidates) {
  

  candidates_text <- paste(
    apply(candidates, 1, function(row) {
      def_text <- ifelse(is.na(row["term_definition"]), 
                         "no definition available", 
                         row["term_definition"])
      paste0("- [", row["ontology"], "] ", row["term_id"], 
             ' "', row["term_label"], '": ', def_text)
    }),
    collapse = "\n"
  )
  
  prompt <- paste0(
    "You are an expert in biological trait ontologies. ",
    "Your task is to match a VecTraits database entry to the most appropriate ontology term.\n\n",
    "VecTraits entry:\n",
    "- Trait name: ", trait_name, "\n",
    "- Definition: ", trait_def, "\n",
    "- Unit: ", trait_unit, "\n\n",
    "Candidate ontology terms:\n",
    candidates_text, "\n\n",
    "Instructions:\n",
    "1. Select the BEST matching term from the candidates above\n",
    "2. Only select a match if you are confident it represents the same biological concept\n",
    "3. If none of the candidates are appropriate, respond with 'NO_MATCH'\n\n",
    "Respond ONLY in JSON format:\n",
    '{"best_match_id": "OBA:XXXXXXX or NO_MATCH", ',
    '"best_match_label": "label or NO_MATCH", ',
    '"confidence": "high/medium/low/none", ',
    '"reasoning": "one sentence explanation"}'
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
      best_match_id = "ERROR",
      best_match_label = "ERROR",
      confidence = "none",
      reasoning = result$error$message
    ))
  }
  
  text <- result$choices[[1]]$message$content
  text <- str_remove_all(text, "```json|```")
  text <- str_trim(text)
  
  tryCatch({
    fromJSON(text)
  }, error = function(e) {
    list(
      best_match_id = "PARSE_ERROR",
      best_match_label = "PARSE_ERROR", 
      confidence = "none",
      reasoning = text
    )
  })
}


cat("\nMatching traits to OBA/PATO via GPT-4o...\n")

all_results <- list()

for (i in seq_len(nrow(unique_pairs))) {
  trait_name <- unique_pairs$OriginalTraitName[i]
  trait_def  <- unique_pairs$OriginalTraitDef[i]
  trait_unit <- unique_pairs$OriginalTraitUnit[i]
  
  cat(sprintf("[%d/%d] %s\n", i, nrow(unique_pairs), trait_name))
  

  candidates <- ontology_matches %>%
    filter(query_trait == trait_name)
  
  if (nrow(candidates) == 0) {
    all_results[[length(all_results) + 1]] <- data.frame(
      trait_name     = trait_name,
      trait_def      = trait_def,
      trait_unit     = trait_unit,
      best_match_id  = "NO_CANDIDATES",
      best_match_label = "NO_CANDIDATES",
      confidence     = "none",
      reasoning      = "No OBA/PATO candidates found via API",
      stringsAsFactors = FALSE
    )
    next
  }
  

  match_result <- match_to_ontology(trait_name, trait_def, trait_unit, candidates)
  
  all_results[[length(all_results) + 1]] <- data.frame(
    trait_name       = trait_name,
    trait_def        = trait_def,
    trait_unit       = trait_unit,
    best_match_id    = match_result$best_match_id,
    best_match_label = match_result$best_match_label,
    confidence       = match_result$confidence,
    reasoning        = match_result$reasoning,
    stringsAsFactors = FALSE
  )
  
  Sys.sleep(0.3)
}


results_df <- do.call(rbind, all_results)


mapped   <- results_df %>% filter(!best_match_id %in% c("NO_MATCH", "NO_CANDIDATES", "ERROR", "PARSE_ERROR"))
unmapped <- results_df %>% filter(best_match_id %in% c("NO_MATCH", "NO_CANDIDATES"))

cat("\n=== Results ===\n")
cat("Total traits processed:", nrow(results_df), "\n")
cat("Successfully mapped:", nrow(mapped), "\n")
cat("Unmapped (novel contribution):", nrow(unmapped), "\n")
cat("\nConfidence breakdown:\n")
print(table(results_df$confidence))


write_csv(results_df, "Results/ontology_mapping_results.csv")
write_csv(mapped,     "Results/mapped_traits.csv")
write_csv(unmapped,   "Results/unmapped_traits.csv")

cat("\nSaved to Results/\n")
cat("All done!\n")
