# ============================================================
# OBA / PATO Ontology Mapping
# ============================================================

library(tidyverse)
library(httr)
library(jsonlite)

setwd("/Users/danielzhu/Documents/Master Project")


unique_names <- read_csv("Results/heterogeneity_analysis.csv") %>%
  pull(OriginalTraitName)

cat("Total unique trait names:", length(unique_names), "\n")


query_ontology <- function(trait_name, ontology = "oba", n_results = 5) {
  

  url <- paste0(
    "https://www.ebi.ac.uk/ols4/api/search",
    "?q=", URLencode(trait_name, reserved = TRUE),
    "&ontology=", ontology,
    "&rows=", n_results,
    "&type=class"
  )
  

  response <- tryCatch({
    GET(url, timeout(30))
  }, error = function(e) {
    cat("Request error for", trait_name, ":", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(response)) return(NULL)
  if (status_code(response) != 200) {
    cat("API error for", trait_name, ": status", status_code(response), "\n")
    return(NULL)
  }
  

  result <- content(response, as = "parsed")
  docs <- result$response$docs
  
  if (length(docs) == 0) return(NULL)
  

  matches <- lapply(docs, function(doc) {
    data.frame(
      query_trait = trait_name,
      ontology = toupper(ontology),
      term_id = ifelse(!is.null(doc$obo_id), doc$obo_id, NA),
      term_label = ifelse(!is.null(doc$label), doc$label, NA),
      term_definition = ifelse(
        length(doc$description) > 0, 
        doc$description[[1]], 
        NA
      ),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, matches)
}


cat("\nQuerying OBA...\n")
oba_results <- list()

for (i in seq_along(unique_names)) {
  trait <- unique_names[i]
  cat(sprintf("[%d/%d] %s\n", i, length(unique_names), trait))
  
  result <- query_ontology(trait, ontology = "oba")
  if (!is.null(result)) {
    oba_results[[length(oba_results) + 1]] <- result
  }
  
  Sys.sleep(0.3)  
}

oba_df <- do.call(rbind, oba_results)
cat("OBA results:", nrow(oba_df), "matches found\n")


cat("\nQuerying PATO...\n")
pato_results <- list()

for (i in seq_along(unique_names)) {
  trait <- unique_names[i]
  cat(sprintf("[%d/%d] %s\n", i, length(unique_names), trait))
  
  result <- query_ontology(trait, ontology = "pato")
  if (!is.null(result)) {
    pato_results[[length(pato_results) + 1]] <- result
  }
  
  Sys.sleep(0.3)
}

pato_df <- do.call(rbind, pato_results)
cat("PATO results:", nrow(pato_df), "matches found\n")


all_matches <- rbind(oba_df, pato_df) %>%
  arrange(query_trait, ontology)


dir.create("Results", showWarnings = FALSE)
write_csv(oba_df, "Results/oba_matches.csv")
write_csv(pato_df, "Results/pato_matches.csv")
write_csv(all_matches, "Results/ontology_matches.csv")

cat("\nSaved to Results/ontology_matches.csv\n")


cat("\n=== Summary ===\n")
cat("Traits with OBA matches:", 
    length(unique(oba_df$query_trait)), "/", length(unique_names), "\n")
cat("Traits with PATO matches:", 
    length(unique(pato_df$query_trait)), "/", length(unique_names), "\n")


no_match <- unique_names[!unique_names %in% unique(all_matches$query_trait)]
cat("Traits with NO matches:", length(no_match), "\n")
if (length(no_match) > 0) {
  cat("Examples:", paste(head(no_match, 7), collapse = ", "), "\n")
}

cat("\nAll done!\n")
