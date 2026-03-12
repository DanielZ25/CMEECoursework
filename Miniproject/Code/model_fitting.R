# Script 2: Model fitting
install.packages("minpack.lm")
library(minpack.lm)
library(dplyr)

# load cleaned data
df_clean <- read.csv("Data/cleaned_growth_data.csv", stringsAsFactors = FALSE)

# metadata for later plots/results
metadata <- df_clean %>%
  select(ID, Species, Temp, Medium, PopBio_units) %>%
  distinct() %>%
  arrange(ID)

# nonlinear model functions
logistic_log <- function(t, N_0, N_max, r_max) {
  N_max - log(1 + (exp(N_max - N_0) - 1) * exp(-r_max * t))
}

gompertz_log <- function(t, N_0, N_max, r_max, t_lag) {
  N_0 + (N_max - N_0) * exp(
    -exp((r_max * exp(1) * (t_lag - t)) / (N_max - N_0) + 1)
  )
}

# starting values for nonlinear fits
get_starting_values <- function(sub) {
  N_0 <- min(sub$log_PopBio)
  N_max <- max(sub$log_PopBio)
  
  if (nrow(sub) > 1) {
    slopes <- diff(sub$log_PopBio) / diff(sub$Time)
    r_max <- max(slopes, na.rm = TRUE)
    r_max <- max(r_max, 1e-4)
  } else {
    r_max <- 0.1
  }
  
  threshold <- N_0 + 0.1 * (N_max - N_0)
  above_idx <- which(sub$log_PopBio >= threshold)
  
  if (length(above_idx) > 0) {
    t_lag <- sub$Time[above_idx[1]]
  } else {
    t_lag <- sub$Time[1]
  }
  
  t_lag <- max(t_lag, 0)
  
  list(N_0 = N_0, N_max = N_max, r_max = r_max, t_lag = t_lag)
}

all_ids <- sort(unique(df_clean$ID))
n_curves <- length(all_ids)

results_list <- vector("list", n_curves)

n_logistic_converged <- 0
n_gompertz_converged <- 0

for (i in seq_along(all_ids)) {
  cid <- all_ids[i]
  sub <- df_clean[df_clean$ID == cid, ]
  sub <- sub[order(sub$Time), ]
  n <- nrow(sub)
  
  row <- data.frame(
    ID = cid,
    n_points = n,
    AIC_quad = NA_real_,
    BIC_quad = NA_real_,
    AIC_cubic = NA_real_,
    BIC_cubic = NA_real_,
    AIC_logistic = NA_real_,
    BIC_logistic = NA_real_,
    converged_logistic = FALSE,
    N0_logistic = NA_real_,
    Nmax_logistic = NA_real_,
    rmax_logistic = NA_real_,
    AIC_gompertz = NA_real_,
    BIC_gompertz = NA_real_,
    converged_gompertz = FALSE,
    N0_gompertz = NA_real_,
    Nmax_gompertz = NA_real_,
    rmax_gompertz = NA_real_,
    tlag_gompertz = NA_real_,
    stringsAsFactors = FALSE
  )
  
  # quadratic
  tryCatch({
    fit_q <- lm(log_PopBio ~ Time + I(Time^2), data = sub)
    row$AIC_quad <- AIC(fit_q)
    row$BIC_quad <- BIC(fit_q)
  }, error = function(e) NULL)
  
  # cubic
  tryCatch({
    fit_c <- lm(log_PopBio ~ Time + I(Time^2) + I(Time^3), data = sub)
    row$AIC_cubic <- AIC(fit_c)
    row$BIC_cubic <- BIC(fit_c)
  }, error = function(e) NULL)
  
  starts <- get_starting_values(sub)
  
  # logistic
  tryCatch({
    fit_l <- nlsLM(
      log_PopBio ~ logistic_log(Time, N_0, N_max, r_max),
      data = sub,
      start = list(N_0 = starts$N_0, N_max = starts$N_max, r_max = starts$r_max),
      lower = c(N_0 = -Inf, N_max = -Inf, r_max = 1e-6),
      upper = c(N_0 = Inf, N_max = Inf, r_max = Inf),
      control = nls.lm.control(maxiter = 1000, ftol = 1e-10, ptol = 1e-10)
    )
    
    row$AIC_logistic <- AIC(fit_l)
    row$BIC_logistic <- BIC(fit_l)
    row$converged_logistic <- TRUE
    row$N0_logistic <- coef(fit_l)["N_0"]
    row$Nmax_logistic <- coef(fit_l)["N_max"]
    row$rmax_logistic <- coef(fit_l)["r_max"]
    
    n_logistic_converged <- n_logistic_converged + 1
  }, error = function(e) NULL)
  
  # gompertz
  tryCatch({
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
      upper = c(N_0 = Inf, N_max = Inf, r_max = Inf, t_lag = Inf),
      control = nls.lm.control(maxiter = 1000, ftol = 1e-10, ptol = 1e-10)
    )
    
    row$AIC_gompertz <- AIC(fit_g)
    row$BIC_gompertz <- BIC(fit_g)
    row$converged_gompertz <- TRUE
    row$N0_gompertz <- coef(fit_g)["N_0"]
    row$Nmax_gompertz <- coef(fit_g)["N_max"]
    row$rmax_gompertz <- coef(fit_g)["r_max"]
    row$tlag_gompertz <- coef(fit_g)["t_lag"]
    
    n_gompertz_converged <- n_gompertz_converged + 1
  }, error = function(e) NULL)
  
  results_list[[i]] <- row
}

results_df <- do.call(rbind, results_list)

aic_cols <- c("AIC_quad", "AIC_cubic", "AIC_logistic", "AIC_gompertz")
bic_cols <- c("BIC_quad", "BIC_cubic", "BIC_logistic", "BIC_gompertz")
model_names <- c("Quadratic", "Cubic", "Logistic", "Gompertz")

get_best <- function(row_vals, names) {
  if (all(is.na(row_vals))) return(NA_character_)
  names[which.min(row_vals)]
}

results_df$best_AIC <- apply(results_df[, aic_cols], 1, get_best, names = model_names)
results_df$best_BIC <- apply(results_df[, bic_cols], 1, get_best, names = model_names)

results_df <- merge(results_df, metadata, by = "ID", all.x = TRUE)

write.csv(results_df, "Results/model_results.csv", row.names = FALSE)

agree <- mean(results_df$best_AIC == results_df$best_BIC, na.rm = TRUE)

cat("Best model by AIC:\n")
print(table(results_df$best_AIC))

cat("\nBest model by BIC:\n")
print(table(results_df$best_BIC))

cat("\nAIC/BIC agreement:", round(100 * agree, 1), "%\n")
cat("Logistic convergence:", n_logistic_converged, "/", n_curves, "\n")
cat("Gompertz convergence:", n_gompertz_converged, "/", n_curves, "\n")
