#!/bin/bash
#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=1:mem=1gb
module load R/4.4.2-gfbf-2024a
echo "R is about to run"
R --vanilla < /rds/general/user/hz1321/home/hz1321_HPC_2025_neutral_cluster.R
echo "R has finished running"
