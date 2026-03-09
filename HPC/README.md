# HPC Programming Exercises — CMEE 2025

**Author:** Daniel Zhu (hz1321@imperial.ac.uk)  
**Course:** CMEE
**Module:** HPC  
**Date:** March 2026

## Overview

This project contains solutions to the HPC programming exercises.


## Repository Structure

```
HPC/
├── Code/
│   ├── Demographic.R                              # Provided demographic model functions
│   ├── Get_my_speciation_rate.R                    # Generating my personal speciation rate
│   ├── hz1321_HPC_2025_main.R                     # Main R file with all functions (Q0–Q26 + Challenges)
│   ├── hz1321_HPC_2025_demographic_cluster.R       # Cluster script for demographic simulations (Q3)
│   ├── hz1321_HPC_2025_neutral_cluster.R           # Cluster script for neutral model simulations (Q24)
│   ├── run_demographic.sh                          # Shell script for demographic cluster jobs (Q4)
│   └── run_neutral.sh                              # Shell script for neutral cluster jobs (Q25)
├── Data/
│   ├── demographic_results/
│   │   └── demographic_cluster_x.rda (1–100)       # Output from 100 demographic cluster runs
│   ├── neutral_results/
│   │   └── neutral_cluster_x.rda (1–100)           # Output from 100 neutral model cluster runs
│   └── neutral_cluster_combined_results.rda         # Processed neutral results for Q26
├── Results/                                        # Plots generated from main script (Q0-26 and challenge questions)
│   ├── question_1.png                              
│   ├── question_2.png                              
│   ├── question_5.png                              
│   ├── question_6.png                            
│   ├── question_14.png                             
│   ├── question_18.png                            
│   ├── question_22.png                             
│   ├── plot_neutral_cluster_results.png           
│   ├── Challenge_A.png                            
│   ├── Challenge_B.png                             
│   ├── Challenge_C.png                             
│   ├── Challenge_D.png                             
│   └── Challenge_E.png                             
├── Log/                                            # Log files from cluster run (neutral model and demographic)
│   ├── run_demographic.sh.e                        
│   ├── run_demographic.sh.o                       
│   ├── run_neutral.sh.e                            
│   └── run_neutral.sh.o                          
└── HPC.Rproj                                       # RStudio project file
```

## How to Run

Set the working directory to the `HPC/` root folder, then:

```r
# Load all functions
source("Code/hz1321_HPC_2025_main.R")
source("Code/Demographic.R")

# Run any question function, e.g.
question_1()
question_5()
question_14()
plot_neutral_cluster_results()
```

## Dependencies

- R (version 4.4.2 used on cluster)
- ggplot2 (used in Challenge A only)
- No other external packages required

