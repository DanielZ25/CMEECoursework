# MiniProject Assessment for Daniel Zhu

## Computing

### A1 — Project Organisation

The project is laid out clearly, with `Code/`, `Data/`, `Results/`, and `Plots/` all present alongside `run_MiniProject.sh`, `Miniproject.tex`, and a README, so the submission is easy to inspect and the main workflow components are easy to locate. The `.gitignore` is present, which is good practice, but `Results/model_results.csv` and several generated PDFs are committed, including `Plots/individual_fits.pdf`; that weakens reproducibility because outputs are mixed with source materials rather than being regenerated cleanly on run. The README gives the run command, dependencies, and a useful directory map, but it stops short of explaining what each package is for, which would have made the toolchain easier to understand for a fresh user.

### A2 — Single-Script Reproducibility

#### Workflow & Solution Quality

`run_MiniProject.sh` starts by installing R packages, then calls `Code/data_preparation.R`, `Code/model_fitting.R`, `Code/plotting_analysis.R`, and finally runs `pdflatex` twice on `Miniproject.tex`, but the graded execution exits with code 2 after the package-installation stage and no newly generated outputs were detected. The script itself shows a complete end-to-end pipeline, yet it is brittle in two important ways: it assumes a particular working directory layout through relative paths such as `Code/...` and `Data/...`, and it installs packages during every run, which adds avoidable overhead and another point of failure. The presence of an existing `Miniproject.pdf` and generated plot files shows that the pipeline has been run successfully in the student's environment, so this is not a missing workflow, but the grading run does not validate it end-to-end. A stronger submission would make the entry script robust to where it is launched from, add explicit checks for required files before execution, and separate one-time environment setup from the analysis run. It is also worth asking whether every non-core dependency is truly necessary for this submission, especially packages such as `scales`; removing unnecessary packages would improve reproducibility and reduce failure points.

### A3 — Code Quality & Style

#### Script-level Technical Feedback

The codebase contains 568 lines across R and shell, with 7 function definitions, and the main analytical logic is sensibly concentrated in `Code/model_fitting.R` and `Code/plotting_analysis.R`. In particular, `Code/model_fitting.R` uses named helper functions such as `logistic_log`, `gompertz_log`, `get_starting_values`, and `get_best`, which makes the fitting workflow easier to follow than a fully inline script would be, and the same model functions are reused consistently in plotting. Comment density is 0.053 with 30 comment lines in total, which is adequate but fairly lean for a project of this size, and `Code/plotting_analysis.R` at 307 lines is starting to become crowded because fitting, prediction, plotting, and summary output all sit in one long script. A next step would be to split `Code/plotting_analysis.R` into smaller helpers for per-curve prediction, summary figure generation, and export, and to document the contract of `get_starting_values()` more explicitly.

### A4 — Model Fitting & Statistical Analysis

#### NLLS

Four models are fitted in `Code/model_fitting.R`: quadratic, cubic, logistic, and Gompertz, with `fit_q`, `fit_c`, `fit_l`, and `fit_g` used to compute AIC and BIC for model comparison across 299 cleaned curves. The nonlinear part is handled appropriately with `nlsLM()` from `minpack.lm`, starting values are derived from the observed curve through `get_starting_values()`, lower bounds are set for `r_max` and `t_lag`, and `tryCatch()` is used so failed fits do not crash the whole batch. That is a strong and technically appropriate NLLS workflow for this assignment, and the report supports it with convergence summaries and biologically interpretable parameters such as `r_max` and `t_lag`; the main limitation is that the report mentions R² in the README outputs but the code shown exports AIC, BIC, convergence flags, and parameter estimates rather than an actual R² column. A next step would be to report the comparison metrics completely and consistently across code, README, and Results, and to log failed-fit reasons per curve rather than only storing convergence as `TRUE/FALSE`.

### A5 — Version Control & Workflow Discipline

The repository has 57 commits in total, but the log reports 0 commits touching `MiniProject/`, which makes the development history for this component impossible to credit as iterative MiniProject work. That pattern usually means the project was added in bulk or outside the tracked folder structure used by the checker, so the main strength here is simply that version control exists at repository level. Future submissions would benefit from smaller, clearly labelled MiniProject commits that show the progression from data cleaning to fitting to report drafting.

## Report

### B1 — Report Format & Presentation

The report meets most of the formal LaTeX requirements: `article` class at 11pt, 1.5 spacing, line numbers, title page details, and a body word count of 3179, which is comfortably within the 3500-word limit. The abstract is present but long at about 279 words, and the display-item count is only 3, below the target range of 4–6, so the visual communication is thinner than the rubric expects. A more substantial issue is bibliography handling: the references are listed manually in the `.tex` file, with no `.bib` file or bibliography command, so the submission does not fully meet the BibTeX/non-numeric citation expectation even though the in-text style is non-numeric.

### B2 — Introduction & Objectives

The introduction gives a clear biological setup around microbial growth phases, sigmoidal trajectories, and the contrast between phenomenological and mechanistic models, and it does build toward a concrete question about which models best describe microbial growth curves across species and conditions. The temperature angle is present, but the framing is more general predictive microbiology than the specific course expectation of temperature-dependent single-population metabolism/growth grounded in both relevant MQB chapter themes, and the automated checks found no explicit chapter grounding. The objectives also lean toward a blended biological-and-methodological statement rather than clearly separating the biological question from the modelling exercise. Future work could sharpen this section by making the biological hypothesis more explicit, tying it more directly to temperature dependence, and distinguishing biological aims from model-selection aims in separate sentences.

### B3 — Methods (including Computing Tools)

The Methods section is good. It describes the dataset size and provenance, the cleaning rules, the four model forms with equations, the fitting functions used (`lm()` and `nlsLM()`), the starting-value heuristics for `N_0`, `N_max`, `r_max`, and `t_lag`, and the use of `tryCatch()` and iteration limits, which gives the reader a reproducible account of the analysis. The `Computing tools` subsection is present and names R, bash, and the main packages, but the package choices are mostly listed rather than justified in depth, so the rationale for each tool remains brief. A next step would be to explain more explicitly why each package was chosen over base alternatives and to align the Methods wording with the exact outputs produced by the code.

### B4 — Results & Display Items

The Results section follows a sensible order from overall model comparison to AIC/BIC agreement, convergence, and temperature-related variation, and the numerical summaries are clear and easy to follow. The main weakness is the limited set of display items: 3 figures and no tables, when the rubric expects 4–6 items and specifically values a model-comparison summary table or equivalent. The captions are generally informative, and the section mostly stays descriptive rather than interpretive, although some explanatory phrasing around BIC penalties starts to edge toward discussion. A stronger results presentation would include a compact AIC/BIC comparison table and one additional figure or table showing parameter summaries or representative fitted curves.

### B5 — Discussion, Conclusions & Abstract

The discussion interprets the main finding well, especially the biological value of the modified Gompertz model through its lag-phase parameter and its stronger interpretability relative to polynomial fits. Limitations are acknowledged in a separate section, and the conclusion gives a clear take-home message, but engagement with advanced methods is too limited for the top band: Bayesian work is cited in the references and mentioned indirectly through alternative methods, yet there is no substantive paragraph explaining what MLE, Bayesian inference, or machine learning would add biologically to this dataset, such as handling species-level heterogeneity or uncertainty in parameter estimates. The abstract is self-contained and includes background, methods, and key findings, though it is somewhat long and could be tighter. Future submissions would benefit from a dedicated discussion paragraph on how Bayesian hierarchical modelling or likelihood-based approaches could address the multi-species, multi-temperature heterogeneity highlighted in the limitations.

## Summary

Final classification (student-facing):

- Part A (Computing): Merit
- Part B (Report): Merit
- Overall: Merit
