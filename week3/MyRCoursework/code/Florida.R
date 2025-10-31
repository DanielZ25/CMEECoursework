rm(list=ls())
load("../data/KeyWestAnnualMeanTemperature.RData")
ls()

class(ats)
head(ats)

plot(ats)

r_obs <- cor(ats$Year, ats$Temp, method = "pearson")


r_perm <- replicate(10000, {
  cor(ats$Year, sample(ats$Temp, replace = FALSE), method = "pearson")
})

p_perm_right <- (sum(r_perm >= r_obs) + 1) / (10000 + 1)

p_perm_two <- (sum(abs(r_perm) >= abs(r_obs)) + 1) / (10000 + 1)

png("../results/perm_hist.png", width = 1200, height = 800, res = 150)
hist(r_perm, breaks = 40,
     main = "Permutation distribution of Pearson r (Temp ~ Year)\nKey West annual mean temperature",
     xlab = "r under permutation (Temp shuffled)",
     ylab = "Frequency")
abline(v = r_obs, lwd = 3)
legend("topleft",
       legend = c(sprintf("Observed r = %.3f", r_obs),
                  sprintf("One-sided p ≈ %.4f", p_perm_right),
                  sprintf("Two-sided p ≈ %.4f", p_perm_two)),
       bty = "n")
dev.off()

summary_lines <- c(
  "Permutation test: Is Florida (Key West) getting warmer?",
  sprintf("Observed Pearson correlation (Year, Temp): r = %.6f", r_obs),
  sprintf("Permutations: 10000"),
  sprintf("One-sided p-value (Pr[r_perm >= r_obs]): %.6f", p_perm_right),
  sprintf("Two-sided p-value (Pr[|r_perm| >= |r_obs|]): %.6f", p_perm_two),
  "Figure saved to ../results/perm_hist.png"
)
writeLines(summary_lines, "../results/florida_permutation_results.txt")

# Also print to console
cat(paste0(summary_lines, collapse = "\n"), "\n")
