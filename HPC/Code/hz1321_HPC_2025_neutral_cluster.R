# CMEE 2024 HPC exercises R code pro forma
# For neutral model cluster run

rm(list=ls()) # good practice 
graphics.off()
source("hz1321_HPC_2025_main.R")

# Read job number from cluster
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
# For local testing, comment out the line above and uncomment below:
# iter <- 1

# Set unique random seed for each job
set.seed(iter)

# Personal speciation rate
speciation_rate <- 0.0028113

# Select community size based on iter (25 jobs per size)
if (iter <= 25) {
  size <- 500
} else if (iter <= 50) {
  size <- 1000
} else if (iter <= 75) {
  size <- 2500
} else {
  size <- 5000
}

# Create unique output filename
output_file_name <- paste("neutral_cluster_", iter, ".rda", sep="")

# Run simulation
# interval_rich=1, interval_oct=size/10, burn_in_generations=8*size
# wall_time = 11.5 hours (690 minutes), cluster gets 12 hours
neutral_cluster_run(speciation_rate = speciation_rate,
                    size = size,
                    wall_time = 690,
                    interval_rich = 1,
                    interval_oct = size / 10,
                    burn_in_generations = 8 * size,
                    output_file_name = output_file_name)