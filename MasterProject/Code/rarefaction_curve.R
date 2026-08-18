library(tidyverse)
library(patchwork)


files <- list(
  v1 = "/Users/danielzhu/Documents/Master Project/Results/clustering_v1.csv",
  v2 = "/Users/danielzhu/Documents/Master Project/Results/clustering_v2.csv",
  v3 = "/Users/danielzhu/Documents/Master Project/Results/clustering_v3.csv",
  v4 = "/Users/danielzhu/Documents/Master Project/Results/clustering_v4.csv",
  v5 = "/Users/danielzhu/Documents/Master Project/Results/clustering_v5.csv",
  v6 = "/Users/danielzhu/Documents/Master Project/Results/clustering_v6.csv"
)

version_labels <- c(
  v1 = "Name",
  v2 = "Name + Def",
  v3 = "Name + Def + Unit",
  v4 = "+ Order",
  v5 = "+ Family",
  v6 = "+ Stage"
)

x_labels <- c("Name", "Name\n+Def", "+Unit", "+Order", "+Family", "+Stage")


summary_stats <- map_dfr(seq_along(files), function(i) {
  v <- names(files)[i]
  all_df <- read_csv(files[[v]], show_col_types = FALSE)
  df     <- all_df %>% filter(cluster_id != -1)
  
  n_total    <- nrow(all_df)
  n_noise    <- sum(all_df$cluster_id == -1)
  n_clusters <- n_distinct(df$cluster_id)
  
  cluster_sizes <- df %>% count(cluster_id)
  
  tibble(
    version     = v,
    label       = version_labels[v],
    x           = i,
    n_clusters  = n_clusters,
    n_noise     = n_noise,
    noise_rate  = n_noise / n_total,
    avg_size    = mean(cluster_sizes$n),
    median_size = median(cluster_sizes$n)
  )
})

print(summary_stats)


p1 <- ggplot(summary_stats, aes(x = x, y = n_clusters)) +
  geom_line(colour = "#2E86AB", linewidth = 1) +
  geom_point(colour = "#2E86AB", size = 3) +
  scale_x_continuous(breaks = 1:6, labels = x_labels) +
  labs(title = "Number of clusters", x = NULL, y = "N clusters") +
  theme_minimal(base_size = 12)

p2 <- ggplot(summary_stats, aes(x = x, y = noise_rate)) +
  geom_line(colour = "#E84855", linewidth = 1) +
  geom_point(colour = "#E84855", size = 3) +
  scale_x_continuous(breaks = 1:6, labels = x_labels) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Noise rate (unclustered %)", x = NULL, y = "Noise rate") +
  theme_minimal(base_size = 12)

p3 <- ggplot(summary_stats, aes(x = x, y = avg_size)) +
  geom_line(colour = "#3BB273", linewidth = 1) +
  geom_point(colour = "#3BB273", size = 3) +
  scale_x_continuous(breaks = 1:6, labels = x_labels) +
  labs(title = "Average traits per cluster", x = "Input features", y = "Avg cluster size") +
  theme_minimal(base_size = 12)

p_combined <- p1 / p2 / p3 +
  plot_annotation(
    title = "Rarefaction-style curve: clustering output vs input information",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

ggsave("Results/rarefaction_curve.pdf", p_combined, width = 8, height = 10)
ggsave("Results/rarefaction_curve.png", p_combined, width = 8, height = 10, dpi = 300)

cat("Saved!\n")
print(summary_stats)
