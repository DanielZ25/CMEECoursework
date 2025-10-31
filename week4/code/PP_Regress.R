library(ggplot2)
library(dplyr)
library(broom)

df <- read.csv("../data/EcolArchives-E089-51-D1.csv")

df$Predator.mass <- as.numeric(df$Predator.mass)
df$Prey.mass <- as.numeric(df$Prey.mass)

df_clean <- df[!is.na(df$Predator.mass) & !is.na(df$Prey.mass) & 
                 !is.na(df$Predator.lifestage) & !is.na(df$Type.of.feeding.interaction), ]
df_clean <- df_clean[df_clean$Predator.mass > 0 & df_clean$Prey.mass > 0, ]

df_clean$Group <- paste(df_clean$Predator.lifestage, df_clean$Type.of.feeding.interaction, sep = " - ")

unique_groups <- unique(df_clean$Group)

regression_results <- data.frame(Predator.lifestage = character(), Type.of.feeding.interaction = character(), n_observations = integer(), Slope = numeric(), Intercept = numeric(), R.squared = numeric(), F.statistic = numeric(), p.value = numeric(), stringsAsFactors = FALSE)

for (group in unique_groups) {
  group_data <- df_clean[df_clean$Group == group, ]
  
  if (nrow(group_data) < 3) {
    next
  }
  
  lm_model <- lm(log10(Prey.mass) ~ log10(Predator.mass), data = group_data)
  glance_stats <- glance(lm_model)
  
  slope <- coef(lm_model)[2]
  intercept <- coef(lm_model)[1]
  r_squared <- glance_stats$r.squared
  f_stat <- glance_stats$statistic
  p_val <- glance_stats$p.value
  n_obs <- nrow(group_data)
  
  parts <- strsplit(group, " - ")[[1]]
  lifestage <- parts[1]
  feeding_type <- parts[2]
  
  new_row <- data.frame(Predator.lifestage = lifestage, Type.of.feeding.interaction = feeding_type, n_observations = n_obs, Slope = round(slope, 6), Intercept = round(intercept, 6), R.squared = round(r_squared, 6), F.statistic = round(f_stat, 6), p.value = round(p_val, 6))
  
  regression_results <- rbind(regression_results, new_row)
}

print(regression_results)

output_dir <- "../results"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

csv_path <- file.path(output_dir, "PP_Regress_Results.csv")
write.csv(regression_results, csv_path, row.names = FALSE)

p <- ggplot(df_clean, aes(x = Prey.mass, y = Predator.mass)) +
  geom_point(aes(color = Predator.lifestage), alpha = 0.6, size = 1.5, shape = 3) +
  geom_smooth(aes(color = Predator.lifestage), method = "lm", se = TRUE, 
              alpha = 0.15, formula = y ~ x, size = 0.8, fullrange = TRUE) +
  facet_wrap(~Type.of.feeding.interaction, scales = "fixed", ncol = 1, strip.position = "right") +
  scale_x_log10(labels = scales::scientific) +
  scale_y_log10(labels = scales::scientific) +
  labs(x = "Prey Mass in grams", y = "Predator mass in grams", color = "Predator.lifeStage") +
  theme_minimal() +
  theme(strip.text.y = element_text(size = 10, face = "bold", angle = 0),
        strip.placement = "right",
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.title.y = element_text(angle = 90),
        legend.position = "bottom",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        panel.grid.major = element_line(color = "gray95", size = 0.3),
        panel.grid.minor = element_line(color = "gray99", size = 0.2),
        plot.margin = margin(5, 15, 5, 5))

print(p)

pdf_path <- file.path(output_dir, "PP_Regress.pdf")
pdf(pdf_path, width = 8.5, height = 11)
print(p)
dev.off()

