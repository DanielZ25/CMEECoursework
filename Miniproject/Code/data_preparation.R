# Script 1: Data preparations
library(dplyr)

df_raw <- read.csv("Data/logistic_growth_data.csv", stringsAsFactors = FALSE)

# make one ID for each growth curve
df_raw$str_ID <- paste(
  df_raw$Species,
  df_raw$Temp,
  df_raw$Medium,
  df_raw$Citation,
  df_raw$Rep,
  sep = "_"
)

# turn string IDs into numeric IDs
unique_str_ids <- unique(df_raw$str_ID)
id_map <- setNames(seq_along(unique_str_ids), unique_str_ids)
df_raw$ID <- id_map[df_raw$str_ID]

# remove invalid PopBio values
n_before <- nrow(df_raw)
df_raw <- df_raw[!is.na(df_raw$PopBio) & df_raw$PopBio > 0, ]

# log transform
df_raw$log_PopBio <- log(df_raw$PopBio)
df_raw <- df_raw[is.finite(df_raw$log_PopBio), ]

# sort by curve and time
df_raw <- df_raw[order(df_raw$ID, df_raw$Time), ]

# keep only curves with at least 5 points
curve_counts <- table(df_raw$ID)
valid_ids <- as.integer(names(curve_counts[curve_counts >= 5]))
df_clean <- df_raw[df_raw$ID %in% valid_ids, ]

write.csv(df_clean, "Data/cleaned_growth_data.csv", row.names = FALSE)

cat("Number of curves:", length(valid_ids), "\n")
cat("Final number of rows:", nrow(df_clean), "\n")