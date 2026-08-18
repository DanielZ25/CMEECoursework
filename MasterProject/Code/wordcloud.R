library(tidyverse)
library(wordcloud)
library(tm)
library(RColorBrewer)

setwd("/Users/danielzhu/Documents/Master Project")


v3 <- read_csv("Results/clustering_v3.csv", show_col_types = FALSE) %>%
  filter(cluster_id != -1)


custom_stops <- c(
  stopwords("en"),
  "mean", "duration", "life", "stage", "individual", "rate",
  "per", "day", "time", "number", "period", "level", "percent",
  "proportion", "unit", "trait", "definition", "days", "1",
  "secondstressor", "individuallevel", "replicatelevel",
  "const", "temps", "constant", "temperatures", "temperature",
  "across", "function", "completion", "required", "different",
  "first", "second", "several", "total", "based", "average",
  "using", "also", "two", "one", "held"
)


cluster_ids <- sort(unique(v3$cluster_id))

pdf("Results/wordclouds_hdbscan_v3_nameonly.pdf", width = 8, height = 6)

for (cid in cluster_ids) {
  members <- v3 %>% filter(cluster_id == cid)
  

  text_blob <- paste(
    members$OriginalTraitName,
    collapse = " "
  )%>%
    tolower() %>%
    removePunctuation() %>%
    removeNumbers() %>%
    removeWords(custom_stops) %>%
    stripWhitespace()
  

  words <- unlist(strsplit(text_blob, " "))
  words <- words[nchar(words) > 2]
  word_freq <- sort(table(words), decreasing = TRUE)
  
  if (length(word_freq) < 2) next
  

  par(mar = c(0, 0, 2, 0))
  wordcloud(
    words      = names(word_freq),
    freq       = as.numeric(word_freq),
    max.words  = 40,
    min.freq   = 1,
    colors     = brewer.pal(8, "Dark2"),
    random.order = FALSE,
    scale      = c(3, 0.5)
  )
  title(main = sprintf("Cluster %d  (%d traits)", cid, nrow(members)),
        cex.main = 1.2)
}

dev.off()
cat("Saved to Results/wordclouds_hdbscan_v3.pdf\n")
