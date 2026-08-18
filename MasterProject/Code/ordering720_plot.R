# ============================================================
# Ordering Experiment (720 permutations) — summary plots
# Panel A: ARI-vs-original distribution  -> field-order sensitivity
# Panel B: silhouette-based ranking      -> geometric ranking only
# Reads Results/ordering_720_full.csv (no re-embedding needed).
# ============================================================

library(tidyverse)
library(patchwork)   # combine the two panels

setwd("/Users/danielzhu/Documents/Master Project")
ord <- read_csv("Results/ordering_720_full.csv", show_col_types = FALSE)

# Anchor points (named orderings) for highlighting
anchors <- ord %>% filter(anchor != "")

# ── Panel A: ARI distribution ───────────────────────────────────
med_ari <- median(ord$ARI_vs_original)

pA <- ggplot(ord, aes(ARI_vs_original)) +
  geom_histogram(binwidth = 0.025, fill = "steelblue", colour = "white", boundary = 0) +
  geom_vline(xintercept = med_ari, linetype = "dashed", colour = "grey30") +
  annotate("text", x = med_ari, y = Inf, vjust = 1.6, hjust = -0.08,
           label = sprintf("median = %.2f", med_ari), size = 3, colour = "grey30") +
  annotate("text", x = 1.0, y = Inf, vjust = 1.6, hjust = 1.05,
           label = "original\n(ARI = 1)", size = 2.7, colour = "firebrick") +
  labs(title = "A. Agreement with the original field order",
       subtitle = "ARI is adjusted for chance; only 1 of 720 orderings was identical to the original",
       x = "Adjusted Rand Index vs original ordering", y = "Number of orderings") +
  theme_minimal(base_size = 10)

# ── Panel B: silhouette ranking ─────────────────────────────────
ranked <- ord %>%
  arrange(desc(silhouette)) %>%
  mutate(rank = row_number())

anchors_ranked <- ranked %>% filter(anchor != "")

pB <- ggplot(ranked, aes(rank, silhouette)) +
  geom_line(colour = "grey70", linewidth = 0.5) +
  geom_point(aes(colour = degenerate), size = 0.7) +
  scale_colour_manual(values = c(`FALSE` = "steelblue", `TRUE` = "firebrick"),
                      labels = c("Non-degenerate", "Degenerate (largest cluster >50%)"),
                      name = NULL) +
  # highlight + label the named anchors
  geom_point(data = anchors_ranked, aes(rank, silhouette),
             colour = "black", size = 2.2, shape = 21, fill = "gold") +
  ggrepel::geom_text_repel(data = anchors_ranked,
                           aes(rank, silhouette, label = paste0(anchor, "\n(rank ", rank, ")")),
                           size = 2.7, box.padding = 0.6, min.segment.length = 0,
                           colour = "grey15", seed = 1) +
  labs(title = "B. Silhouette-based ranking of 720 field orders",
       subtitle = "This ranking describes geometric separation, not biological validity",
       x = "Rank (best to worst silhouette)", y = "Silhouette (non-noise points)") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

# ── Combine & save ──────────────────────────────────────────────
combined <- pA / pB + plot_layout(heights = c(1, 1.3))

ggsave("Results/ordering_720_summary.png", combined, width = 9, height = 10, dpi = 300)
ggsave("Results/ordering_720_summary.pdf", combined, width = 9, height = 10)

cat("Saved:\n  Results/ordering_720_summary.png\n  Results/ordering_720_summary.pdf\n")

# quick console facts to report
cat(sprintf("\nARI: min=%.3f median=%.3f max=%.3f\n",
            min(ord$ARI_vs_original), med_ari, max(ord$ARI_vs_original)))
cat(sprintf("Identical to original (ARI>=0.9999): %d / %d\n",
            sum(ord$ARI_vs_original >= 0.9999), nrow(ord)))
cat(sprintf("Silhouette: min=%.3f max=%.3f\n",
            min(ord$silhouette, na.rm = TRUE), max(ord$silhouette, na.rm = TRUE)))
cat(sprintf("Degenerate orderings (max cluster >50%%): %d / %d\n",
            sum(ord$degenerate), nrow(ord)))
cat("\nNamed anchors by silhouette rank:\n")
print(anchors_ranked %>% select(anchor, rank, silhouette, noise_rate,
                                max_cluster_share, ARI_vs_original))
