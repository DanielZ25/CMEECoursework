# ============================================================
# Cluster Labeling with GPT-4o
# ============================================================

library(tidyverse)
library(httr)
library(jsonlite)

setwd("/Users/danielzhu/Documents/Master Project")

# API key
Sys.setenv(OPENAI_API_KEY = "")


results_v5 <- read_csv("Results/clustering_v5.csv")
cat("Loaded", nrow(results_v5), "rows\n")


get_cluster_label <- function(cluster_id, cluster_members) {
  
  members_text <- paste(
    apply(cluster_members, 1, function(row) {
      paste0("- ", row["OriginalTraitName"], ": ", row["OriginalTraitDef"])
    }),
    collapse = "\n"
  )
  
  prompt <- paste0(
    "You are an expert in ecological trait databases specialising in vector biology. ",
    "Below is a group of trait names and definitions that have been clustered together by semantic similarity.\n\n",
    members_text, "\n\n",
    "Please provide:\n",
    "1. A single standardised trait name (2-4 words, lowercase) that best represents this cluster\n",
    "2. A brief justification (1 sentence)\n\n",
    "Respond ONLY in JSON format with no extra text: ",
    "{\"standardised_name\": \"...\", \"justification\": \"...\"}"
  )
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(
      "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      "Content-Type" = "application/json"
    ),
    body = toJSON(list(
      model = "gpt-4o",
      messages = list(
        list(role = "user", content = prompt)
      ),
      temperature = 0.3
    ), auto_unbox = TRUE),
    encode = "raw"
  )
  
  result <- content(response, as = "parsed")
  
  if (!is.null(result$error)) {
    cat("API Error for cluster", cluster_id, ":", result$error$message, "\n")
    return(list(standardised_name = "ERROR", justification = result$error$message))
  }
  
  text <- result$choices[[1]]$message$content
  

  text <- str_remove_all(text, "```json|```")
  text <- str_trim(text)
  
  tryCatch({
    parsed <- fromJSON(text)
    return(parsed)
  }, error = function(e) {
    cat("JSON parse error for cluster", cluster_id, ":", text, "\n")
    return(list(standardised_name = "PARSE ERROR", justification = text))
  })
}


cluster_ids <- sort(unique(results_v5$cluster_id))
cluster_ids <- cluster_ids[cluster_ids != -1]  

cat("Labeling", length(cluster_ids), "clusters...\n\n")

labels <- data.frame()

for (cid in cluster_ids) {
  cat(sprintf("Processing Cluster %d...\n", cid))
  
  members <- results_v5 %>%
    filter(cluster_id == cid) %>%
    select(OriginalTraitName, OriginalTraitDef) %>%
    distinct()
  
  result <- get_cluster_label(cid, members)
  
  labels <- rbind(labels, data.frame(
    cluster_id = cid,
    n_members = nrow(results_v5 %>% filter(cluster_id == cid)),
    standardised_name = result$standardised_name,
    justification = result$justification,
    stringsAsFactors = FALSE
  ))
  
  cat(sprintf("  -> %s\n", result$standardised_name))
  Sys.sleep(0.5)  
}


write_csv(labels, "Results/cluster_labels.csv")
cat("\nSaved to Results/cluster_labels.csv\n")


results_labeled <- results_v5 %>%
  left_join(labels %>% select(cluster_id, standardised_name), 
            by = "cluster_id") %>%
  mutate(standardised_name = ifelse(cluster_id == -1, "NOISE", standardised_name))

write_csv(results_labeled, "Results/clustering_v5_labeled.csv")
cat("Saved to Results/clustering_v5_labeled.csv\n")

cat("\nAll done!\n")
print(labels %>% select(cluster_id, n_members, standardised_name))
