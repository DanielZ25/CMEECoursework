# CMEE 2025 HPC exercises R code pro forma
# For stochastic demographic model cluster run

rm(list=ls()) # good practice 
graphics.off()
source("Demographic.R")
source("hz1321_HPC_2025_main.R")

iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
set.seed(iter)

growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0,
                          0.5, 0.4, 0.0, 0.0,
                          0.0, 0.4, 0.7, 0.0,
                          0.0, 0.0, 0.25, 0.4),
                        nrow=4, ncol=4, byrow=TRUE)

reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
                                0.0, 0.0, 0.0, 0.0,
                                0.0, 0.0, 0.0, 0.0,
                                0.0, 0.0, 0.0, 0.0),
                              nrow=4, ncol=4, byrow=TRUE)

clutch_distribution <- c(0.06,0.08,0.13,0.15,0.16,0.18,0.15,0.06,0.03)
simulation_length <- 120

if (iter <= 25) {
  initial_state <- state_initialise_adult(num_stages=4, initial_size=100)
} else if (iter <= 50) {
  initial_state <- state_initialise_adult(num_stages=4, initial_size=10)
} else if (iter <= 75) {
  initial_state <- state_initialise_spread(num_stages=4, initial_size=100)
} else {
  initial_state <- state_initialise_spread(num_stages=4, initial_size=10)
}

output_file_name <- paste("demographic_cluster_", iter, ".rda", sep="")

results <- list()
for (i in 1:150) {
  results[[i]] <- stochastic_simulation(
    initial_state = initial_state,
    growth_matrix = growth_matrix,
    reproduction_matrix = reproduction_matrix,
    clutch_distribution = clutch_distribution,
    simulation_length = simulation_length
  )
}

save(results, file = output_file_name)