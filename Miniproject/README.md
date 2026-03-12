# Miniproject: Fitting Mathematical Models to Microbial Population Growth Data

**Author:** Daniel Zhu 

**Institution:** Imperial College London

---

## Before Running

Please ensure the following files and folders **MUST BE** present in the `Miniproject/` directory:

**Required folders:** `Code/`, `Data/`, `Results/`, `Plots/`

**Required files:**
- `Data/logistic_growth_data.csv` — raw input data
- `Code/data_preparation.R`
- `Code/model_fitting.R`
- `Code/plotting_analysis.R`
- `Miniproject.tex` — LaTeX report source

---

## How to Run

From the `Miniproject/` directory, run:

```bash
bash run_MiniProject.sh
```

This will execute the three R scripts in sequence (data preparation → model fitting → plotting), then compile the LaTeX report.

---

## Outputs

After running, the following outputs will be generated:

- `Miniproject.pdf` — compiled report
- `Results/model_results.csv` — per-curve AIC, BIC, R² and parameter estimates
- `Plots/` — all figures including individual model fits, AIC/BIC winners, convergence rates, and summary by temperature

---

## Project Structure

```
Miniproject/
├── Code/
│   ├── data_preparation.R       # Cleans raw data and assigns unique curve IDs
│   ├── model_fitting.R          # Fits all four models; computes AIC and BIC
│   └── plotting_analysis.R      # Generates all figures and summary statistics
├── Data/
│   ├── cleaned_growth_data.csv  # Cleaned data produced by data_preparation.R
│   └── logistic_growth_data.csv # Raw input data (required before running)
├── Plots/
│   ├── AIC_winner.pdf           # Best model by AIC
│   ├── BIC_winner.pdf           # Best model by BIC
│   ├── convergence_rates.pdf    # NLLS convergence rates for logistic and Gompertz
│   ├── individual_fits.pdf      # Model fits overlaid on every growth curve
│   └── summary_temperature.pdf  # Model selection and r_max by temperature
├── Results/
│   └── model_results.csv        # Parameter estimates and model selection results for each curve
├── Miniproject.pdf              # Compiled report (generated)
├── Miniproject.Rproj            # RStudio project file
├── Miniproject.tex              # LaTeX report source
└── run_MiniProject.sh           # Master script — run this to reproduce everything
```

---

## Dependencies

- **R** (≥ 4.0) with packages: `dplyr`, `tidyr`, `minpack.lm`, `ggplot2`, `gridExtra`
- **LaTeX** 