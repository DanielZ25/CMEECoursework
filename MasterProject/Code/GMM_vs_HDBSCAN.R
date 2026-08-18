library(tidyverse)
library(patchwork)


hdbscan <- read_csv("Results/clustering_v3.csv", show_col_types = FALSE)
gmm     <- read_csv("Results/clustering_gmm_v3.csv", show_col_types = FALSE)


hdbscan_noise <- sum(hdbscan$cluster_id == -1)

hdbscan_sizes <- as.data.frame(table(hdbscan$cluster_id)) %>%
  setNames(c("cluster_id", "n")) %>%
  mutate(n = as.integer(n), cluster_id = as.integer(as.character(cluster_id))) %>%
  filter(cluster_id != -1) %>%
  arrange(desc(n)) %>%
  mutate(rank = row_number(), method = "HDBSCAN")

# GMM
gmm_sizes <- as.data.frame(table(gmm$cluster_id)) %>%
  setNames(c("cluster_id", "n")) %>%
  mutate(n = as.integer(n), cluster_id = as.integer(as.character(cluster_id))) %>%
  arrange(desc(n)) %>%
  mutate(rank = row_number(), method = "GMM")

combined <- bind_rows(hdbscan_sizes, gmm_sizes)


p1 <- ggplot(combined, aes(x = rank, y = n, colour = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c("HDBSCAN" = "#2E86AB", "GMM" = "#E84855")) +
  annotate("text", x = 5, y = hdbscan_noise, 
           label = paste0("HDBSCAN noise: ", hdbscan_noise, " pts"),
           colour = "#2E86AB", hjust = 0, size = 3.5) +
  labs(
    title = "Cluster size distribution: HDBSCAN vs GMM",
    subtitle = "V3 feature set (name + definition + unit)",
    x = "Cluster rank (largest to smallest)",
    y = "Number of trait pairs",
    colour = "Method"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")


summary_compare <- tibble(
  Metric             = c("N clusters", "Largest cluster", "Avg cluster size", 
                         "Median cluster size", "Noise points"),
  HDBSCAN            = c(
    n_distinct(hdbscan_sizes$cluster_id),
    max(hdbscan_sizes$n),
    round(mean(hdbscan_sizes$n), 1),
    median(hdbscan_sizes$n),
    hdbscan_noise
  ),
  GMM                = c(
    n_distinct(gmm_sizes$cluster_id),
    max(gmm_sizes$n),
    round(mean(gmm_sizes$n), 1),
    median(gmm_sizes$n),
    0
  )
)

print(summary_compare)


ggsave("Results/hdbscan_vs_gmm.png", p1, width = 8, height = 5, dpi = 300)
ggsave("Results/hdbscan_vs_gmm.pdf", p1, width = 8, height = 5)
write_csv(summary_compare, "Results/hdbscan_vs_gmm_summary.csv")

cat("Done!\n")
print(p1)
