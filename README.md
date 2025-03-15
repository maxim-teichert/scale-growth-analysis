

# Coregonid Growth Analysis: R Scripts and Data Processing

## Overview

This repository contains R scripts for the analysis of long-term growth patterns in coregonid fish (*Coregonus* spp.) from Lake Starnberg, Germany. The analysis spans a 22-year period (1998–2020) and investigates the effects of environmental factors, such as phosphorus concentrations and spring temperatures, on age-specific growth patterns.

The project uses scale analysis to reconstruct growth histories and applies linear mixed-effects models to evaluate the roles of environmental and intrinsic factors in shaping growth trajectories.


### Preprocessing Scripts
The preprocessing scripts must be run **in order**, as they build upon each other to prepare the data for analysis.

1. **`preprocessing_01_radiuslength-&-n-circuli_2023-03-28.R`**:
   - Extracts and processes raw scale data, including identifying peaks in gray values and calculating radius lengths.
   - Outputs: `n_circuli_2023-03-28.xlsx`.

2. **`preprocessing_02_radius-length-by-age-class_2023-03-28.R`**:
   - Calculates adjusted radius lengths for each age class by subtracting smaller measurements from larger ones.
   - Outputs: `av-radius-length_by-age-class_2023-03-28.xlsx`.

3. **`preprocessing_03_merge_scales-and-env.data_2023-03-28.R`**:
   - Merges scale data with environmental data (temperature and phosphorus) to create a comprehensive dataset.
   - Outputs: `scale-radius-and-env.data_2023-04-05.xlsx`.

### Analysis Scripts
The analysis scripts can be run independently, depending on the specific research question.

1. **`analysis_01_mix.model_year-and-age-class_2023-08-01.R`**:
   - Fits a linear mixed-effects model to assess the interaction between year and age class on scale radius length.
   - Includes residual diagnostics and autocorrelation checks.

2. **`analysis_02_mix.model_all-variables_2023-08-01.R`**:
   - Extends the mixed-effects model to include environmental variables (temperature and phosphorus) and their interactions with age class.
   - Produces diagnostic plots and tables for model results.

3. **`analysis_03_env.data-vs-year_2024-11-14.R`**:
   - Analyzes trends in environmental variables (temperature and phosphorus) over time using linear regression.
   - Includes robust standard error calculations and visualizations.

4. **`analysis_04_t-test_sample-vs-population_2024-04-17.R`**:
   - Compares the subsample of fish used for scale analysis to the overall population using two-sample t-tests for length and weight.

## Requirements

### Software
- **R** (version 4.2.0 or higher)

### R Packages
The following R packages are required to run the scripts:
- `tidyverse`
- `nlme`
- `emmeans`
- `patchwork`
- `lubridate`
- `gt`
- `gstat`
- `sandwich`
- `readxl`
- `writexl`

To install these packages, run:
```R
install.packages(c("tidyverse", "nlme", "emmeans", "patchwork", "lubridate", 
                   "gt", "gstat", "sandwich", "readxl", "writexl"))
```                   

# Usage

## Preprocessing

1. Clone this repository:
   ```bash
   git clone https://github.com/maxim-teichert/coregonid-growth-analysis.git

2. Navigate to the preprocessing/ folder.
3. Run the preprocessing scripts in order:
- **`preprocessing_01_radiuslength-&-n-circuli_2023-03-28.R`**
- **`preprocessing_02_radius-length-by-age-class_2023-03-28.R`**
- **`preprocessing_03_merge_scales-and-env.data_2023-03-28.R`**
4. Ensure that the required raw data files are placed in the data/ folder before running the scripts.

## Analysis
1. Navigate to the analysis/ folder.
2. Run any of the analysis scripts based on your research question:
- **`analysis_01_mix.model_year-and-age-class_2023-08-01.R`**
- **`analysis_02_mix.model_all-variables_2023-08-01.R`**
- **`analysis_03_env.data-vs-year_2024-11-14.R`**
- **`analysis_04_t-test_sample-vs-population_2024-04-17.R`**

# Results
The analysis produces:
- Statistical summaries of growth patterns across age classes.
- Visualizations of growth trends over time.
- Insights into the effects of environmental factors (phosphorus, temperature) on growth.
- Diagnostic plots for model residuals and autocorrelation.

# Data Availability
The raw and processed datasets are available for download on Figshare: [https://doi.org/10.6084/m9.figshare.28601408.v1](https://doi.org/10.6084/m9.figshare.28601408.v1)

# Contact
For questions or further information, please contact:
- **Maxim Teichert**: [maxim.teichert@baw.at](mailto:maxim.teichert@baw.at)
