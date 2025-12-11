# My CMEE Coursework Repository
> "I am going to try this new naughty thing on this experimental branch!"

This repository contains coursework and scripts developed during the CMEE Bootcamp at Imperial College London, Silwood Park.

The CMEE Bootcamp covers:
- UNIX and shell scripting  
- Python programming  
- R programming and ecological modelling  
- Reproducible workflows (LaTeX, Git)  

Each week has its own directory with an individual README describing the exercises, scripts, data, and outputs.

---

## Repository Structure

```
CMEECoursework/
│
├── week1/                     # UNIX, Shell Scripting, Git, LaTeX
│   ├── code/                  # Shell scripts (tabtocsv.sh, variables.sh, etc.)
│   ├── data/                  # FASTA files, Temperature data
│   ├── results/               # Currently empty. Output files will be generated here after running the scripts.
│   ├── sandbox/               # Practice/test commands
│   └── README.md              # Week 1 overview
│
├── week2/                     # Python Programming
│   ├── code/                  # lc1.py, cfexercises1.py, align_seqs.py, etc.
│   ├── data/                  # CSV input files
│   ├── results/               # Currently empty.Output files will be generated here after running the scripts.
│   ├── sandbox/               # Testing area
│   └── README.md              # Week 2 overview
│
├── week3/
│   └── MyRCoursework/         # R Programming
│       ├── code/              # Florida.R, TreeHeight.R, Ricker.R, etc.
│       ├── data/              # Tree data, Florida weather data
│       ├── results/           # Currently empty. Output files will be generated here after running the scripts.
│       └── README.md          # Week 3 overview
│
├── week4/                     # Advanced R: Linear Modelling
│   ├── code/                  # PP_Regress.R, regression exercises
│   ├── data/                  # Ecological datasets (EcolArchives-E089-51-D1.csv)
│   ├── results/               # Currently empty.Output files will be generated here after running the scripts.
│   ├── sandbox/               # Testing area
│   └── README.md              # Week 4 overview
│
├── .gitignore                 # Ignore temp/log/auxiliary files
└── README.md                  # This file — top-level overview
```

---

## Weekly Assignment Summary

### Week 1 — UNIX, Shell and Git

Scripts include:
- UnixPrac1.txt
- tabtocsv.sh, csvtospace.sh
- ConcatenateTwoFiles.sh
- Basic boilerplate scripts

Stored in: `CMEECoursework/week1/code/`  
Detailed description: see `week1/README.md`.

---

### Week 2 — Python Programming

Exercises include:
- lc1.py, lc2.py, dictionary.py, tuple.py
- cfexercises1.py
- align_seqs.py
- oaks_debugme.py

Stored in: `CMEECoursework/week2/code/`  
Detailed description: see `week2/README.md`.

---

### Week 3 — R Programming

Main exercises:
- TreeHeight.R — compute tree heights
- Florida.R — temperature trend analysis  
- Girko ellipse simulation  
- apply-family and vectorisation practice  

Stored in: `CMEECoursework/week3/MyRCoursework/code/`  
Detailed description: see `week3/README.md`.

---

### Week 4 — Linear Regression in R

- PP_Regress.R — regression modelling  
- Additional R exercises

Stored in: `CMEECoursework/week4/code/`  
Detailed description: see `week4/README.md`.

---

## Notes

- All weekly folders include their own README summarising tasks, scripts, and workflows.
- This repository follows basic good practice for reproducible coursework:
  - File organisation  
  - Code–data separation  
  - Version control  
  - Documentation  