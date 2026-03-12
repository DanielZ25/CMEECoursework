#!/bin/bash
set -e

echo "=================================="
echo "Running MiniProject pipeline"
echo "=================================="

echo ""
echo "Step 1: Data preparation"
Rscript Code/data_preparation.R

echo ""
echo "Step 2: Model fitting"
Rscript Code/model_fitting.R

echo ""
echo "Step 3: Plotting and analysis"
Rscript Code/plotting_analysis.R

rm -f Rplots.pdf

echo ""
echo "Step 4: Compiling LaTeX report"
pdflatex Miniproject.tex
pdflatex Miniproject.tex

rm -f *.aux *.log *.out

echo ""
echo "=================================="
echo "MiniProject completed successfully and removed LaTeX log files"
echo "Output: Miniproject.pdf"
echo "=================================="