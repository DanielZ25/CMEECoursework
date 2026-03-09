# CMEE 2025 HPC exercises R code main pro forma
# You don't HAVE to use this but it will be very helpful.
# If you opt to write everything yourself from scratch please ensure you use
# EXACTLY the same function and parameter names and beware that you may lose
# marks if it doesn't work properly because of not using the pro-forma.

name <- "Daniel Zhu"
preferred_name <- "Daniel Zhu"
email <- "hz1321@imperial.ac.uk"
username <- "hz1321"

# Please remember *not* to clear the work space here, or anywhere in this file.
# If you do, it'll wipe out your username information that you entered just
# above, and when you use this file as a 'toolbox' as intended it'll also wipe
# away everything you're doing outside of the toolbox.  For example, it would
# wipe away any automarking code that may be running and that would be annoying!

source("Code/Demographic.R")
# Section One: Stochastic demographic population model
# Question 0

state_initialise_adult <- function(num_stages,initial_size){
  #first repeat the 0 num_stages times
  state <- rep(0, num_stages)
  #then No.num_stages (the last/final one) is the initial size (all individuals are in the final adult stage)
  state[num_stages] <- initial_size
  
  return(state)
}

state_initialise_spread <- function(num_stages,initial_size){
  #base and remainder
  base <- floor(initial_size/num_stages)
  remainder <- initial_size%%num_stages
  #create vector state with even base
  state <- rep(base,num_stages)
  #distribute the remainder evenly from the start
  if (remainder >0) {
    state[1:remainder] <- state[1:remainder] +1}
  
  return(state)
}

# Question 1
question_1 <- function(){
  source("Code/Demographic.R")
  growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0,
                            0.5, 0.4, 0.0, 0.0,
                            0.0, 0.4, 0.7, 0.0,
                            0.0, 0.0, 0.25, 0.4),
                          nrow=4, ncol=4, byrow=T)
  reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0),
                                nrow=4, ncol=4, byrow=T)
  projection_matrix = reproduction_matrix + growth_matrix
  
 state_adult <- state_initialise_adult(4,100)
 state_spread <- state_initialise_spread(4,100)
 population_adult <- deterministic_simulation(state_adult, projection_matrix, 24)
 population_spread <- deterministic_simulation(state_spread, projection_matrix, 24)
 time <- 0:24
  png(filename="Results/question_1.png", width = 600, height = 400)
  # plot your graph here
  matplot(time, cbind(population_adult,population_spread),
       type = "l",
       lty = 1,
       col  = c("blue","red"),
       xlab = "Time",
       ylab = "Total population size",
       main = "Population size over time")
  legend("topright", 
         legend = c("Initials all in final stages","Initials spread across"),
         col = c("blue","red"),
         lty = 1)
 
  Sys.sleep(0.1)
  dev.off()

#Explain how the initial distribution of the population 
#in different life stages affects the initial and eventual 
#population growth.  
  return("Early population dynamics are affected by the initial distribution. It will not change long-term growth rate. When all individuals start as adults, individuals starts to reproduce immediately, causing rapid initial increase. When spread across life stages, growth is initially slower, because younger individuals must mature before reproducing. However, both populations eventually converge to the same exponential growth rate, which is determined by the dominant eigenvalue of the projection matrix.")
}

# Question 2
question_2 <- function(){
  source("Code/Demographic.R")
  growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0,
                            0.5, 0.4, 0.0, 0.0,
                            0.0, 0.4, 0.7, 0.0,
                            0.0, 0.0, 0.25, 0.4),
                          nrow=4, ncol=4, byrow=T)
  reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0),
                                nrow=4, ncol=4, byrow=T)
  clutch_distribution <- c(0.06,0.08,0.13,0.15,0.16,0.18,0.15,0.06,0.03)
  
  q2_state_adult <- state_initialise_adult(4,100)
  q2_state_spread <- state_initialise_spread(4,100)
  q2_population_adult <- stochastic_simulation(q2_state_adult,growth_matrix,reproduction_matrix,clutch_distribution,24)
  q2_population_spread <- stochastic_simulation(q2_state_spread,growth_matrix,reproduction_matrix,clutch_distribution, 24)
  time <- 0:24
  
  png(filename="Results/question_2.png", width = 600, height = 400)
  plot(time, q2_population_adult,
       type = "l",
       lty = 1,
       col  = "blue",
       xlab = "Time",
       ylab = "Total population size",
       main = "Population size over time")
  lines(time, q2_population_spread, type = "l", col = "red")
  legend("topright", 
         legend = c("Initials all in final stages","Initials spread across"), 
         col = c("blue", "red"), 
         lty = 1)
        
  Sys.sleep(0.1)
  dev.off()
  
  return("The deterministic simulations produce smooth predictable curves whereas the stochastic simulations produce jagged irregular trajectories. This results from the fact that the deterministic model uses exact transition probabilities, which produces the same result every time. In the meantime, the stochastic model uses random sampling for survival, maturation and reproduction events, thus introducing demographic stochasticity that causes fluctuations. These fluctuations will be more noticeable in smaller populations.")
}

# Questions 3 and 4 involve writing code elsewhere to run your simulations on the cluster


# Question 5
question_5 <- function(){
  extinction_counts <- c(0, 0, 0, 0)
  total_sims <- c(0, 0, 0, 0)
  
  for (iter in 1:100) {
    if (iter <= 25){
      group <- 1
    } else if (iter <= 50) {
      group <- 2
    } else if (iter <= 75) {
      group <- 3
    } else {
      group <- 4
    }
    
    load(paste("Data/demographic_results/demographic_cluster_", iter, ".rda", sep = ""))
    
    for (i in 1:length(results)) {
      total_sims[group] <- total_sims[group] + 1
      if (results[[i]][length(results[[i]])] == 0) {
        extinction_counts[group] <- extinction_counts[group] +1
      }
    }
  }
  
  extinction_proportions <- extinction_counts / total_sims
  
  labels <- c("adults, large pop",
              "adults, small pop",
              "mixed, large pop",
              "mixed, small pop")
  
  png(filename="Results/question_5.png", width = 600, height = 400)
  # plot your graph here
  barplot(extinction_proportions,
          names.arg = labels,
          ylab = "Proportion of extinctions",
          xlab = "initial condition",
          main = "Proportion of simulations resulting in extinction",
          col = c("blue", "lightblue", "red", "pink"),
          ylim = c(0, max(extinction_proportions) * 1.2))
  Sys.sleep(0.1)
  dev.off()
  
  return("The small population with individuals spread across life stages was most likely to go extinct. Smaller populations are more vulnerable to demographic stochasticity where random fluctuations can easily drive the population to zero. When individuals are spread across life stages rather than concentrated as adults, fewer can reproduce immediately, further increasing extinction risk during the critical early period.")
}

# Question 6
question_6 <- function(){
  source("Code/Demographic.R")
  growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0,
                            0.5, 0.4, 0.0, 0.0,
                            0.0, 0.4, 0.7, 0.0,
                            0.0, 0.0, 0.25, 0.4),
                          nrow=4, ncol=4, byrow=T)
  reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0),
                                nrow=4, ncol=4, byrow=T)
  projection_matrix = reproduction_matrix + growth_matrix
  
  simulation_length <- 120
  
  # --- Initial condition 3: spread, large (100) ---
  initial_state_large <- state_initialise_spread(num_stages=4, initial_size=100)
  
  # Collect all stochastic results for this condition
  stoch_sum_large <- rep(0, simulation_length + 1)
  count_large <- 0
  for (iter in 51:75) {
    load(paste("Data/demographic_results/demographic_cluster_", iter, ".rda", sep=""))
    for (i in 1:length(results)) {
      stoch_sum_large <- stoch_sum_large + results[[i]]
      count_large <- count_large + 1
    }
  }
  stoch_mean_large <- stoch_sum_large / count_large
  
  # Deterministic simulation
  det_large <- deterministic_simulation(initial_state_large, projection_matrix, simulation_length)
  
  # Deviation
  deviation_large <- stoch_mean_large / det_large
  
  # --- Initial condition 4: spread, small (10) ---
  initial_state_small <- state_initialise_spread(num_stages=4, initial_size=10)
  
  stoch_sum_small <- rep(0, simulation_length + 1)
  count_small <- 0
  for (iter in 76:100) {
    load(paste("Data/demographic_results/demographic_cluster_", iter, ".rda", sep=""))
    for (i in 1:length(results)) {
      stoch_sum_small <- stoch_sum_small + results[[i]]
      count_small <- count_small + 1
    }
  }
  stoch_mean_small <- stoch_sum_small / count_small
  
  # Deterministic simulation
  det_small <- deterministic_simulation(initial_state_small, projection_matrix, simulation_length)
  
  # Deviation
  deviation_small <- stoch_mean_small / det_small
  
  # --- Plot ---
  time <- 0:simulation_length
  
  png(filename="Results/question_6.png", width = 600, height = 400)
  # plot your graph here
  plot(time, deviation_large,
       type = "l", col = "blue",
       xlab = "Time step",
       ylab = "Stochastic mean / Deterministic",
       main = "Deviation of stochastic model from deterministic model",
       ylim = c(min(c(deviation_large, deviation_small), na.rm=TRUE),
                max(c(deviation_large, deviation_small), na.rm=TRUE)))
  lines(time, deviation_small, col = "red")
  abline(h = 1, lty = 2, col = "grey")
  legend("bottomleft",
         legend = c("mixed, large pop (100)", "mixed, small pop (10)"),
         col = c("blue", "red"), lty = 1)
  Sys.sleep(0.1)
  dev.off()
  
  return("It is more appropriate to use a deterministic model for the large mixed population of 100 individuals. With a larger population, random fluctuations average out so the mean stochastic behaviour closely follows the deterministic prediction with the deviation ratio staying near 1. For the small population of 10 individuals, demographic stochasticity has a much larger relative effect and frequent extinctions pull the average population size below the deterministic prediction causing greater deviation.")
}


# Section Two: Individual-based ecological neutral theory simulation 

# Question 7
species_richness <- function(community){
  return(length(unique(community)))
}

# Question 8
init_community_max <- function(size){
  return(seq(1, size))
}

# Question 9
init_community_min <- function(size){
  return(rep(1, size))
}

# Question 10
choose_two <- function(max_value){
  return(sample(1:max_value, 2, replace=FALSE))
}

# Question 11
neutral_step <- function(community){
  indices <- choose_two(length(community))
  community[indices[1]] <- community[indices[2]]
  return(community)
}

# Question 12
neutral_generation <- function(community){
  x <- length(community)
  steps <- x / 2
  if (x %% 2 != 0) {
    steps <- sample(c(floor(steps), ceiling(steps)), 1)
  }
  for (i in 1:steps) {
    community <- neutral_step(community)
  }
  return(community)
}

# Question 13
neutral_time_series <- function(community,duration)  {
  richness <- numeric(duration + 1)
  richness[1] <- species_richness(community)
  for (i in 1:duration) {
    community <- neutral_generation(community)
    richness[i + 1] <- species_richness(community)
  }
  return(richness)
}

# Question 14
question_14 <- function() {
  community <- init_community_max(100)
  richness <- neutral_time_series(community, 200)
  
  png(filename="Results/question_14.png", width = 600, height = 400)
  # plot your graph here
  plot(0:200, richness,
       type = "l", col = "blue",
       xlab = "Generation",
       ylab = "Species richness",
       main = "Neutral model simulation from maximum diversity")
  Sys.sleep(0.1)
  dev.off()
  
  return("The system will always converge to monodominance with species richness equal to 1. Without speciation, species can only be lost through random drift and never gained. Each generation random deaths and replacements cause some species to lose individuals and eventually go extinct until only one species remains which is the absorbing state of the system.")
}

# Question 15
neutral_step_speciation <- function(community,speciation_rate)  {
  indices <- choose_two(length(community))
  if (runif(1) < speciation_rate) {
    # Speciation: dead individual replaced by new species
    community[indices[1]] <- max(community) + 1
  } else {
    # Normal replacement
    community[indices[1]] <- community[indices[2]]
  }
  return(community)
}

# Question 16
neutral_generation_speciation <- function(community,speciation_rate)  {
  x <- length(community)
  steps <- x / 2
  if (x %% 2 != 0) {
    steps <- sample(c(floor(steps), ceiling(steps)), 1)
  }
  for (i in 1:steps) {
    community <- neutral_step_speciation(community, speciation_rate)
  }
  return(community)
}

# Question 17
neutral_time_series_speciation <- function(community,speciation_rate,duration)  {
  richness <- numeric(duration + 1)
  richness[1] <- species_richness(community)
  for (i in 1:duration) {
    community <- neutral_generation_speciation(community, speciation_rate)
    richness[i + 1] <- species_richness(community)
  }
  return(richness)
}

# Question 18
question_18 <- function()  {
  richness_max <- neutral_time_series_speciation(
    init_community_max(100), speciation_rate=0.1, duration=200)
  richness_min <- neutral_time_series_speciation(
    init_community_min(100), speciation_rate=0.1, duration=200)
  
  png(filename="Results/question_18.png", width = 600, height = 400)
  # plot your graph here
  plot(0:200, richness_max,
       type = "l", col = "blue",
       xlab = "Generation",
       ylab = "Species richness",
       main = "Neutral model with speciation (rate = 0.1)",
       ylim = c(0, max(richness_max, richness_min)))
  lines(0:200, richness_min, col = "red")
  legend("topright",
         legend = c("Max initial diversity", "Min initial diversity"),
         col = c("blue", "red"), lty = 1)
  Sys.sleep(0.1)
  dev.off()
  
  return("Both initial conditions converge to a similar dynamic equilibrium of species richness. Starting from maximum diversity richness decreases as drift removes species. Starting from minimum diversity richness increases as speciation adds new species. They converge because the equilibrium is determined by the balance between speciation adding species and ecological drift removing species. This balance depends only on the speciation rate and community size, which will not be affected by the initial condition.")
}

# Question 19
species_abundance <- function(community)  {
  return(sort(as.numeric(table(community)), decreasing = TRUE))
  
}

# Question 20
octaves <- function(abundance_vector) {
  octave_index <- floor(log2(abundance_vector)) + 1
  return(tabulate(octave_index))
}

# Question 21
sum_vect <- function(x, y) {
  max_len <- max(length(x), length(y))
  if (length(x) < max_len) {
    x <- c(x, rep(0, max_len - length(x)))
  }
  if (length(y) < max_len) {
    y <- c(y, rep(0, max_len - length(y)))
  }
  return(x + y)
}

# Question 22
question_22 <- function() {
  speciation_rate <- 0.1
  size <- 100
  burn_in <- 200
  duration <- 2000
  interval <- 20
  
  # --- From max initial diversity ---
  community_max <- init_community_max(size)
  # Burn-in
  for (i in 1:burn_in) {
    community_max <- neutral_generation_speciation(community_max, speciation_rate)
  }
  # Record octaves
  oct_sum_max <- octaves(species_abundance(community_max))
  count_max <- 1
  for (i in 1:duration) {
    community_max <- neutral_generation_speciation(community_max, speciation_rate)
    if (i %% interval == 0) {
      oct_sum_max <- sum_vect(oct_sum_max, octaves(species_abundance(community_max)))
      count_max <- count_max + 1
    }
  }
  oct_mean_max <- oct_sum_max / count_max
  
  # --- From min initial diversity ---
  community_min <- init_community_min(size)
  # Burn-in
  for (i in 1:burn_in) {
    community_min <- neutral_generation_speciation(community_min, speciation_rate)
  }
  # Record octaves
  oct_sum_min <- octaves(species_abundance(community_min))
  count_min <- 1
  for (i in 1:duration) {
    community_min <- neutral_generation_speciation(community_min, speciation_rate)
    if (i %% interval == 0) {
      oct_sum_min <- sum_vect(oct_sum_min, octaves(species_abundance(community_min)))
      count_min <- count_min + 1
    }
  }
  oct_mean_min <- oct_sum_min / count_min
  
  png(filename="Results/question_22.png", width = 600, height = 400)
  # plot your graph here
  par(mfrow=c(1,2))
  barplot(oct_mean_max,
          names.arg = 1:length(oct_mean_max),
          main = "Max initial diversity",
          xlab = "Octave class",
          ylab = "Mean number of species")
  barplot(oct_mean_min,
          names.arg = 1:length(oct_mean_min),
          main = "Min initial diversity",
          xlab = "Octave class",
          ylab = "Mean number of species")
  Sys.sleep(0.1)
  dev.off()
  
  return("The long-term species abundance distribution will not be affected by the initial condition. After a sufficient burn-in period, both initial conditions produce very similar mean species abundance octave distributions. This is because the neutral model with speciation reaches a dynamic equilibrium determined solely by the speciation rate and community size. The burn-in period allows the system to lose memory of its starting state.")
}

# Question 23
neutral_cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name) {
  # Initialise
  community <- init_community_min(size)
  time_series <- c()
  abundance_list <- list()
  gen <- 0
  
  # Timer
  start_time <- proc.time()[3]
  wall_time_seconds <- wall_time * 60
  
  while ((proc.time()[3] - start_time) < wall_time_seconds) {
    # One generation
    community <- neutral_generation_speciation(community, speciation_rate)
    gen <- gen + 1
    
    # Record species richness during burn-in
    if (gen <= burn_in_generations) {
      if (gen %% interval_rich == 0) {
        time_series <- c(time_series, species_richness(community))
      }
    }
    
    # Record octaves throughout entire simulation
    if (gen %% interval_oct == 0) {
      abundance_list[[length(abundance_list) + 1]] <- octaves(species_abundance(community))
    }
  }
  
  # Save results
  total_time <- proc.time()[3] - start_time
  community_state <- community
  save(time_series, abundance_list, community_state, total_time,
       speciation_rate, size, wall_time, interval_rich, interval_oct, 
       burn_in_generations,
       file = output_file_name)
}

# Questions 24 and 25 involve writing code elsewhere to run your simulations on
# the cluster

# Question 26 
process_neutral_cluster_results <- function() {
  
  
  combined_results <- list() #create your list output here to return
  sizes <- c(500, 1000, 2500, 5000)
  # save results to an .rda file
  
  for (s in 1:4) {
    # Determine which iter values correspond to this size
    iter_start <- (s - 1) * 25 + 1
    iter_end <- s * 25
    
    oct_sum <- c()
    oct_count <- 0
    
    for (iter in iter_start:iter_end) {
      load(paste("Data/neutral_results/neutral_cluster_", iter, ".rda", sep=""))
      
      # Only use post-burn-in octaves
      # burn_in_generations = 8 * size, interval_oct = size / 10
      # So first (burn_in / interval_oct) = 80 entries are during burn-in
      burn_in_oct <- burn_in_generations / interval_oct
      
      if (length(abundance_list) > burn_in_oct) {
        for (i in (burn_in_oct + 1):length(abundance_list)) {
          if (length(oct_sum) == 0) {
            oct_sum <- abundance_list[[i]]
          } else {
            oct_sum <- sum_vect(oct_sum, abundance_list[[i]])
          }
          oct_count <- oct_count + 1
        }
      }
    }
    
    combined_results[[s]] <- oct_sum / oct_count
  }
  save(combined_results, file = "Data/neutral_cluster_combined_results.rda")
  return(combined_results)
}

plot_neutral_cluster_results <- function(){

    # load combined_results from your rda file
  load("Data/neutral_cluster_combined_results.rda")
  sizes <- c(500, 1000, 2500, 5000)
  
  
    png(filename="Results/plot_neutral_cluster_results.png", width = 600, height = 600)
    par(mfrow=c(2,2))
    
    for (i in 1:4) {
      barplot(combined_results[[i]],
              names.arg = 1:length(combined_results[[i]]),
              main = paste("Community size =", sizes[i]),
              xlab = "Octave class",
              ylab = "Mean number of species")
    }
    # plot your graph here
    Sys.sleep(0.1)
    dev.off()
    
    return(combined_results)
}


# Challenge questions - these are substantially harder and worth fewer marks.
# I suggest you only attempt these if you've done all the main questions. 

# Challenge question A
Challenge_A <- function(){
  total_rows <- 100 * 150 * 121
  simulation_number <- integer(total_rows)
  initial_condition <- character(total_rows)
  time_step <- integer(total_rows)
  population_size <- numeric(total_rows)
  
  sim_number <- 0
  row_idx <- 1
  
  for (iter in 1:100) {
    if (iter <= 25) {
      ic <- "large adult"
    } else if (iter <= 50) {
      ic <- "small adult"
    } else if (iter <= 75) {
      ic <- "large mixed"
    } else {
      ic <- "small mixed"
    }
    
    load(paste("Data/demographic_results/demographic_cluster_", iter, ".rda", sep=""))
    
    for (i in 1:length(results)) {
      sim_number <- sim_number + 1
      ts <- results[[i]]
      n <- length(ts)
      idx_range <- row_idx:(row_idx + n - 1)
      
      simulation_number[idx_range] <- sim_number
      initial_condition[idx_range] <- ic
      time_step[idx_range] <- 0:(n - 1)
      population_size[idx_range] <- ts
      
      row_idx <- row_idx + n
    }
  }
  
  population_size_df <- data.frame(
    simulation_number = simulation_number[1:(row_idx - 1)],
    initial_condition = initial_condition[1:(row_idx - 1)],
    time_step = time_step[1:(row_idx - 1)],
    population_size = population_size[1:(row_idx - 1)]
  )
  
  library(ggplot2)
  
  png(filename="Results/Challenge_A.png", width = 600, height = 400)
  # plot your graph here
  print(ggplot(population_size_df, 
               aes(x = time_step, y = population_size, 
                   group = simulation_number, colour = initial_condition)) +
          geom_line(alpha = 0.1) +
          xlab("Time step") +
          ylab("Population size") +
          ggtitle("All stochastic demographic simulations") +
          theme_minimal())
  Sys.sleep(0.1)
  dev.off()
  
}

# Challenge question B
Challenge_B <- function() {
  speciation_rate <- 0.1
  size <- 100
  duration <- 200
  n_repeats <- 200
  
  # Store richness for both initial conditions
  richness_max_all <- matrix(0, nrow = n_repeats, ncol = duration + 1)
  richness_min_all <- matrix(0, nrow = n_repeats, ncol = duration + 1)
  
  for (r in 1:n_repeats) {
    richness_max_all[r, ] <- neutral_time_series_speciation(
      init_community_max(size), speciation_rate, duration)
    richness_min_all[r, ] <- neutral_time_series_speciation(
      init_community_min(size), speciation_rate, duration)
  }
  
  # Calculate means
  mean_max <- colMeans(richness_max_all)
  mean_min <- colMeans(richness_min_all)
  
  # 97.2% CI: alpha = 0.028, so lower = 1.4%, upper = 98.6%
  ci_max_lower <- apply(richness_max_all, 2, quantile, probs = 0.014)
  ci_max_upper <- apply(richness_max_all, 2, quantile, probs = 0.986)
  ci_min_lower <- apply(richness_min_all, 2, quantile, probs = 0.014)
  ci_min_upper <- apply(richness_min_all, 2, quantile, probs = 0.986)
  
  time <- 0:duration
  
  png(filename="Results/Challenge_B.png", width = 600, height = 400)
  # plot your graph here
  plot(time, mean_max, type = "l", col = "blue",
       xlab = "Generation", ylab = "Species richness",
       main = "Mean species richness with 97.2% CI",
       ylim = c(0, max(ci_max_upper, ci_min_upper)))
  polygon(c(time, rev(time)), c(ci_max_lower, rev(ci_max_upper)),
          col = rgb(0, 0, 1, 0.2), border = NA)
  lines(time, mean_min, col = "red")
  polygon(c(time, rev(time)), c(ci_min_lower, rev(ci_min_upper)),
          col = rgb(1, 0, 0, 0.2), border = NA)
  legend("topright",
         legend = c("Max initial diversity", "Min initial diversity"),
         col = c("blue", "red"), lty = 1)
  Sys.sleep(0.1)
  dev.off()
  
}

# Challenge question C
Challenge_C <- function() {
  speciation_rate <- 0.1
  size <- 100
  duration <- 200
  n_repeats <- 50
  
  # Range of initial richnesses
  initial_richnesses <- seq(1, 100, by = 10)
  
  png(filename="Results/Challenge_C.png", width = 600, height = 400)
  # plot your graph here
  plot(NULL, xlim = c(0, duration), ylim = c(0, 100),
       xlab = "Generation", ylab = "Species richness",
       main = "Mean species richness for different initial conditions")
  
  colours <- rainbow(length(initial_richnesses))
  
  for (j in 1:length(initial_richnesses)) {
    r0 <- initial_richnesses[j]
    richness_all <- matrix(0, nrow = n_repeats, ncol = duration + 1)
    
    for (rep in 1:n_repeats) {
      # Create community where each individual randomly takes any of r0 species
      community <- sample(1:r0, size, replace = TRUE)
      richness_all[rep, ] <- neutral_time_series_speciation(
        community, speciation_rate, duration)
    }
    
    mean_richness <- colMeans(richness_all)
    lines(0:duration, mean_richness, col = colours[j])
  }
  
  legend("topright", legend = initial_richnesses, col = colours, 
         lty = 1, title = "Initial richness", cex = 0.7)
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question D
Challenge_D <- function() {
  sizes <- c(500, 1000, 2500, 5000)
  
  # Process data first
  mean_ts_list <- list()
  min_lens <- c()
  
  for (s in 1:4) {
    iter_start <- (s - 1) * 25 + 1
    iter_end <- s * 25
    
    all_ts <- list()
    for (iter in iter_start:iter_end) {
      load(paste("Data/neutral_results/neutral_cluster_", iter, ".rda", sep=""))
      all_ts[[length(all_ts) + 1]] <- time_series
    }
    
    min_len <- min(sapply(all_ts, length))
    min_lens[s] <- min_len
    
    ts_matrix <- matrix(0, nrow = length(all_ts), ncol = min_len)
    for (i in 1:length(all_ts)) {
      ts_matrix[i, ] <- all_ts[[i]][1:min_len]
    }
    mean_ts_list[[s]] <- colMeans(ts_matrix)
  }
  
  png(filename="Results/Challenge_D.png", width = 600, height = 400)
  par(mfrow = c(2, 2))
  # plot your graph here
  for (s in 1:4) {
    plot(1:min_lens[s], mean_ts_list[[s]], type = "l", col = "blue",
         xlab = "Generation", ylab = "Mean species richness",
         main = paste("Size =", sizes[s]))
    abline(v = 8 * sizes[s], col = "red", lty = 2)
    legend("bottomright", legend = paste("burn-in =", 8 * sizes[s]),
           col = "red", lty = 2, cex = 0.8)
  }
  Sys.sleep(0.1)
  dev.off()
}

# Challenge question E
Challenge_E <- function() {
  coalescence_simulation <- function(size, speciation_rate) {
    lineages <- rep(1, size)
    abundances <- c()
    J <- size
    theta <- speciation_rate * (size - 1) / (1 - speciation_rate)
    
    while (J > 1) {
      j <- sample(1:J, 1)
      randnum <- runif(1)
      
      if (randnum < theta / (theta + J - 1)) {
        abundances <- c(abundances, lineages[j])
      } else {
        i <- sample((1:J)[-j], 1)
        lineages[i] <- lineages[i] + lineages[j]
      }
      
      lineages <- lineages[-j]
      J <- J - 1
    }
    
    abundances <- c(abundances, lineages[1])
    return(abundances)
  }
  
  sizes <- c(500, 1000, 2500, 5000)
  speciation_rate <- 0.0028113
  n_repeats <- 100
  
  load("Data/neutral_cluster_combined_results.rda")
  
  # Process data
  oct_mean_coal_list <- list()
  total_coal_time <- 0
  
  for (s in 1:4) {
    oct_sum <- c()
    
    start_time <- proc.time()[3]
    for (r in 1:n_repeats) {
      abund <- coalescence_simulation(sizes[s], speciation_rate)
      oct <- octaves(sort(abund, decreasing = TRUE))
      if (length(oct_sum) == 0) {
        oct_sum <- oct
      } else {
        oct_sum <- sum_vect(oct_sum, oct)
      }
    }
    coal_time <- proc.time()[3] - start_time
    total_coal_time <- total_coal_time + coal_time
    
    oct_mean_coal_list[[s]] <- oct_sum / n_repeats
  }
  
  png(filename="Results/Challenge_E.png", width = 600, height = 400)
  par(mfrow = c(2, 2))
  # plot your graph here
  for (s in 1:4) {
    max_len <- max(length(combined_results[[s]]), length(oct_mean_coal_list[[s]]))
    cluster_vals <- c(combined_results[[s]], rep(0, max_len - length(combined_results[[s]])))
    coal_vals <- c(oct_mean_coal_list[[s]], rep(0, max_len - length(oct_mean_coal_list[[s]])))
    
    barplot(rbind(cluster_vals, coal_vals), beside = TRUE,
            names.arg = 1:max_len,
            col = c("blue", "red"),
            main = paste("Size =", sizes[s]),
            xlab = "Octave class",
            ylab = "Mean number of species")
    legend("topright", legend = c("Cluster", "Coalescence"),
           fill = c("blue", "red"), cex = 0.7)
  }
  
  Sys.sleep(0.1)
  dev.off()
  cluster_hours <- 100 * 11.5
  coal_hours <- total_coal_time / 3600
  
  return(paste("The coalescence simulation used approximately", 
               round(coal_hours, 4), 
               "CPU hours while the cluster used approximately", 
               cluster_hours,
               "CPU hours. The coalescence simulations are much faster because they work backwards from the present, only tracking lineages that survive to the present day, whereas the forward-time simulation must simulate every individual at every time step including all lineages that eventually go extinct."))
}
