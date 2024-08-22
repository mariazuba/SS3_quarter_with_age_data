# SS3 Quarter with Age Data Repository

## Overview

This repository contains scripts and documents used for running and analyzing SS3 models with age composition data. The scripts automate various tasks such as data processing, model execution, results extraction, retrospective analysis, and report generation. Below is a description of each key script and its purpose.

## Scripts

### `boot_01_run.R`

This script automates the generation of `DATA.bib` and `SOFTWARE.bib` files, which contain metadata about the data and software used for an SS3 model. It organizes the data and software into the `boot/data` and `boot/software` directories and commits the changes to the Git repository. **To avoid affecting the main repository, it is recommended to comment out the commit and push section or switch to a different branch before running the script.**

### `data_01_run.R`

This script preprocesses data for an SS3 model by extracting, filtering, and organizing data types such as abundance indices, catches, age composition, and weight-at-age. It saves the processed data into `.RData` and Excel files, which are stored in the `data/run` directory for further analysis. The script then commits and pushes the changes to the Git repository. **Comment out the commit and push section or switch to another branch to prevent changes in the main repository.**

### `model_01_run.R`

This script sets up and runs SS3 model scenarios, managing directories and copying necessary data and software for each scenario. The models are executed in the `model/run` directory. After running the models, it commits and pushes the results to the Git repository, ensuring that the results are properly versioned. **To prevent changes in the main repository, comment out the commit and push section or switch to a different branch.**

### `model_02_retro.R`

This script runs retrospective analyses for SS3 model scenarios. It copies input files from `model/run` to a new directory in `model/retro`, ensures the SS3 executable is available, and performs the retrospective analysis for a specified range of years. The working directory is restored after the analysis.

### `output_01_run.R`

This script extracts key results from SS3 model runs and generates TAF output tables. It loads the results, generates plots, and saves them in the `output/run` directory, while managing the version control through Git. **To avoid impacting the main repository, comment out the commit and push section or switch to another branch before running the script.**

### `output_02_retro.R`

This script summarizes and saves the results of retrospective analyses for SS3 model scenarios. It retrieves and summarizes the model outputs for a series of years, saving the results as `.RData` files in the `output/retro` directory.

### `report_01_run.R`

This script generates plots and tables for reports based on SS3 model runs. It creates various diagnostic plots and summary tables, saving them in the `report/run` directory, and commits the generated files to the Git repository. **To avoid unintended changes in the main repository, comment out the commit and push section or switch branches before running.**

### `report_02_retro.R`

This script generates plots and tables for retrospective analysis reports of SS3 model scenarios. It summarizes the retrospective results and creates figures and tables, which are saved in the `report/retro` directory.

### `Report_SS3_quarter_with_age_data.Rmd`

This R Markdown document produces a comprehensive stock assessment report for the Anchovy Stock in ICES Subdivision 9a South, using age composition data. The report includes figures and tables on catch trends, abundance indices, age composition, model settings, and retrospective analysis, formatted according to ICES Journal of Marine Science standards.

## Conclusion

Each script in this repository is designed to be flexible and reproducible, facilitating the management and analysis of SS3 models. For safer operation, it is advised to manage version control carefully, especially when working within the main branch.
