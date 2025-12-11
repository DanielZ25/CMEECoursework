# Week 4 – Data Wrangling, GPDD, and Profiling (Python + R)

This week blends Python and R for data wrangling, ecological modelling, mapping, and performance profiling.

## Folder Structure

```text
└── week4
    ├── code
    │   ├── DataWrang.R
    │   ├── DataWrangTidy.R
    │   ├── GPDD_Data.R
    │   ├── LV1.py
    │   ├── LV2.py
    │   ├── MyBars.R
    │   ├── MyFirstJupyterNb.ipynb
    │   ├── PP_Regress.R
    │   ├── plotLin.R
    │   ├── profileme.py
    │   ├── profileme2.py
    │   └── timeitme.py
    ├── data
    │   ├── EcolArchives-E089-51-D1.csv
    │   ├── GPDDFiltered.RData
    │   ├── PoundHillData.csv
    │   ├── PoundHillMetaData.csv
    │   └── Results.txt
    └── results

```

- `code/` — R + Python scripts (profiling, GPDD mapping, LV models)  
- `data/` — GPDDFiltered, ecological datasets  
- `results/` — Generated outputs  

## Topics Covered

### R Data Wrangling
- `DataWrang.R`, `DataWrangTidy.R`  
- Reading, cleaning, reshaping data  

### Ecological Modelling
- `LV1.py`, `LV2.py` — Lotka–Volterra predator–prey simulations  

### GPDD Mapping
- `GPDD_Data.R` — Plotting species locations globally  

### Profiling & Performance
- `profileme.py`, `profileme2.py`  
- `timeitme.py`  

## Example Usage

```bash
cd week4/code
Rscript GPDD_Data.R
python3 LV1.py
```

## Author

- **Name:** Daniel Zhu  
- **Email:** haotian.zhu21@imperial.ac.uk

