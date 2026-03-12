# Script 3: Plotting
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("gridExtra")
install.packages("scales")
install.packages("minpack.lm")

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(scales)
library(minpack.lm)

# load data
df_clean <- read.csv("Data/cleaned_growth_data.csv", stringsAsFactors = FALSE)
results_df <- read.csv("Results/model_results.csv", stringsAsFactors = FALSE)

# model functions
logistic_log <- function(t, N_0, N_max, r_max) {
  N_max - log(1 + (exp(N_max - N_0) - 1) * exp(-r_max * t))
}

gompertz_log <- function(t, N_0, N_max, r_max, t_lag) {
  N_0 + (N_max - N_0) * exp(-exp((r_max * exp(1) * (t_lag - t)) / (N_max - N_0) + 1))
}

# starting values for nonlinear fitting
get_starting_values <- function(sub) {
  N_0 <- min(sub$log_PopBio)
  N_max <- max(sub$log_PopBio)
  
  if (nrow(sub) > 1) {
    slopes <- diff(sub$log_PopBio) / diff(sub$Time)
    r_max <- max(max(slopes, na.rm = TRUE), 1e-4)
  } else {
    r_max <- 0.1
  }
  
  threshold <- N_0 + 0.1 * (N_max - N_0)
  above_idx <- which(sub$log_PopBio >= threshold)
  t_lag <- if (length(above_idx) > 0) sub$Time[above_idx[1]] else sub$Time[1]
  t_lag <- max(t_lag, 0)
  
  list(N_0 = N_0, N_max = N_max, r_max = r_max, t_lag = t_lag)
}

model_names <- c("Quadratic", "Cubic", "Logistic", "Gompertz")
model_colours <- c(
  "Quadratic" = "#E69F00",
  "Cubic" = "#56B4E9",
  "Logistic" = "#009E73",
  "Gompertz" = "#D55E00"
)

all_ids <- sort(unique(df_clean$ID))
n_curves <- length(all_ids)

agree <- mean(results_df$best_AIC == results_df$best_BIC, na.rm = TRUE)

# check model fits for individual curves

pdf("Plots/individual_fits.pdf", width = 10, height = 8)

for (cid in all_ids) {
  sub <- df_clean[df_clean$ID == cid, ]
  sub <- sub[order(sub$Time), ]
  
  t_seq <- seq(min(sub$Time), max(sub$Time), length.out = 300)
  pred_df <- data.frame(Time = t_seq)
  
  # quadratic
  tryCatch({
    fit_q <- lm(log_PopBio ~ Time + I(Time^2), data = sub)
    pred_df$Quadratic <- predict(fit_q, newdata = data.frame(Time = t_seq))
  }, error = function(e) {
    pred_df$Quadratic <- NA
  })
  
  # cubic
  tryCatch({
    fit_c <- lm(log_PopBio ~ Time + I(Time^2) + I(Time^3), data = sub)
    pred_df$Cubic <- predict(fit_c, newdata = data.frame(Time = t_seq))
  }, error = function(e) {
    pred_df$Cubic <- NA
  })
  
  # logistic
  tryCatch({
    starts <- get_starting_values(sub)
    fit_l <- nlsLM(
      log_PopBio ~ logistic_log(Time, N_0, N_max, r_max),
      data = sub,
      start = list(N_0 = starts$N_0, N_max = starts$N_max, r_max = starts$r_max),
      lower = c(N_0 = -Inf, N_max = -Inf, r_max = 1e-6),
      control = nls.lm.control(maxiter = 1000)
    )
    p_l <- coef(fit_l)
    pred_df$Logistic <- logistic_log(t_seq, p_l["N_0"], p_l["N_max"], p_l["r_max"])
  }, error = function(e) {
    pred_df$Logistic <- NA
  })
  
  # gompertz
  tryCatch({
    starts <- get_starting_values(sub)
    fit_g <- nlsLM(
      log_PopBio ~ gompertz_log(Time, N_0, N_max, r_max, t_lag),
      data = sub,
      start = list(
        N_0 = starts$N_0,
        N_max = starts$N_max,
        r_max = starts$r_max,
        t_lag = starts$t_lag
      ),
      lower = c(N_0 = -Inf, N_max = -Inf, r_max = 1e-6, t_lag = 0),
      control = nls.lm.control(maxiter = 1000)
    )
    p_g <- coef(fit_g)
    pred_df$Gompertz <- gompertz_log(t_seq, p_g["N_0"], p_g["N_max"], p_g["r_max"], p_g["t_lag"])
  }, error = function(e) {
    pred_df$Gompertz <- NA
  })
  
  pred_long <- pivot_longer(pred_df, -Time, names_to = "Model", values_to = "log_PopBio")
  pred_long <- pred_long[!is.na(pred_long$log_PopBio), ]
  pred_long$Model <- factor(pred_long$Model, levels = model_names)
  
  sp <- unique(sub$Species)[1]
  temp <- unique(sub$Temp)[1]
  med <- unique(sub$Medium)[1]
  
  res_row <- results_df[results_df$ID == cid, ]
  best <- ifelse(nrow(res_row) > 0, res_row$best_AIC[1], "?")
  
  p <- ggplot(sub, aes(x = Time, y = log_PopBio)) +
    geom_point(size = 2, colour = "black") +
    geom_line(
      data = pred_long,
      aes(colour = Model, linetype = Model),
      linewidth = 0.9
    ) +
    scale_colour_manual(values = model_colours) +
    scale_linetype_manual(values = c(
      "Quadratic" = "dashed",
      "Cubic" = "dotdash",
      "Logistic" = "solid",
      "Gompertz" = "solid"
    )) +
    labs(
      title = paste0("ID ", cid, ": ", sp),
      subtitle = paste0("Temp = ", temp, "°C | Medium: ", med, " | Best model (AIC): ", best),
      x = "Time (hours)",
      y = "log(Population / Biomass)",
      colour = "Model",
      linetype = "Model"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  print(p)
}

dev.off()

# Best-fit model under AIC

aic_counts <- as.data.frame(table(results_df$best_AIC), stringsAsFactors = FALSE)
names(aic_counts) <- c("Model", "Count")
aic_counts$Model <- factor(aic_counts$Model, levels = model_names)
aic_counts$Pct <- 100 * aic_counts$Count / sum(aic_counts$Count)

p_aic <- ggplot(aic_counts, aes(x = Model, y = Count, fill = Model)) +
  geom_bar(stat = "identity", width = 0.65, colour = "white") +
  geom_text(aes(label = paste0(Count, "\n(", round(Pct, 1), "%)")),
            vjust = -0.3, size = 4) +
  scale_fill_manual(values = model_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Best-fitting model by AIC",
    subtitle = paste0("n = ", sum(aic_counts$Count), " curves"),
    x = "Model",
    y = "Number of curves"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

ggsave("Plots/AIC_winner.pdf", p_aic, width = 7, height = 5)

# Best-fit model under BIC

bic_counts <- as.data.frame(table(results_df$best_BIC), stringsAsFactors = FALSE)
names(bic_counts) <- c("Model", "Count")
bic_counts$Model <- factor(bic_counts$Model, levels = model_names)
bic_counts$Pct <- 100 * bic_counts$Count / sum(bic_counts$Count)

p_bic <- ggplot(bic_counts, aes(x = Model, y = Count, fill = Model)) +
  geom_bar(stat = "identity", width = 0.65, colour = "white") +
  geom_text(aes(label = paste0(Count, "\n(", round(Pct, 1), "%)")),
            vjust = -0.3, size = 4) +
  scale_fill_manual(values = model_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Best-fitting model by BIC",
    subtitle = paste0("n = ", sum(bic_counts$Count), " curves"),
    x = "Model",
    y = "Number of curves"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

ggsave("Plots/BIC_winner.pdf", p_bic, width = 7, height = 5)


# check convergence rates for nonlinear models


n_logistic_converged <- sum(results_df$converged_logistic, na.rm = TRUE)
n_gompertz_converged <- sum(results_df$converged_gompertz, na.rm = TRUE)

conv_df <- data.frame(
  Model = c("Logistic", "Gompertz"),
  Converged = c(n_logistic_converged, n_gompertz_converged),
  NotConverged = c(n_curves - n_logistic_converged, n_curves - n_gompertz_converged)
)

conv_long <- pivot_longer(conv_df, -Model, names_to = "Status", values_to = "Count")
conv_long$Status <- factor(
  conv_long$Status,
  levels = c("Converged", "NotConverged"),
  labels = c("Converged", "Did not converge")
)

p_conv <- ggplot(conv_long, aes(x = Model, y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("Converged" = "#2ecc71", "Did not converge" = "#e74c3c")) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "NLLS model convergence rates",
    x = "Model",
    y = "Proportion of curves",
    fill = "Convergence"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Plots/convergence_rates.pdf", p_conv, width = 12, height = 5)


# summary figure

temp_model <- results_df %>%
  filter(!is.na(best_AIC)) %>%
  mutate(Temp = factor(Temp)) %>%
  group_by(Temp, best_AIC) %>%
  summarise(Count = n(), .groups = "drop") %>%
  rename(Model = best_AIC) %>%
  mutate(Model = factor(Model, levels = model_names))

rmax_df <- results_df %>%
  filter(!is.na(rmax_gompertz), converged_gompertz == TRUE) %>%
  mutate(Temp = factor(Temp))


p2 <- ggplot(rmax_df, aes(x = Temp, y = rmax_gompertz)) +
  geom_boxplot(fill = "#D55E00", alpha = 0.7, outlier.size = 0.5, width = 0.5) +
  labs(
    title = "B) r_max by temperature",
    x = "Temperature (°C)",
    y = expression(r[max] ~ "(h"^-1*")")
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 10))

p1 <- ggplot(temp_model, aes(x = Temp, y = Count, fill = Model)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = model_colours) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "A) Best model by temperature",
    x = "Temperature (°C)",
    y = "Proportion",
    fill = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  )

summary_plot <- grid.arrange(p1, p2, ncol = 2)
ggsave("Plots/summary_temperature.pdf", summary_plot, width = 12, height = 5)

# simple summary output
cat("Total curves analysed:", n_curves, "\n")
cat("AIC/BIC agreement:", round(100 * agree, 1), "%\n")

cat("\nBest model by AIC:\n")
print(as.data.frame(table(results_df$best_AIC)))

cat("\nBest model by BIC:\n")
print(as.data.frame(table(results_df$best_BIC)))
