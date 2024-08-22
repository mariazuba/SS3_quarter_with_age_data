# SS3 quarter with age data

## boot_01_run.R

The script automates the generation of `DATA.bib` and `SOFTWARE.bib` files, which contain metadata about the data and software used for an SS3 model, based on files in the `boot/initial` directory. It employs the `draft.data()` and `draft.software()` functions to create these files, and organizes the data and software into the `boot/data` and `boot/software` directories using `taf.bootstrap()`. Finally, it commits and pushes these changes to the Git repository, ensuring proper version control of the generated files. *To avoid making changes directly to the main repository, it is recommended to either comment out the section that performs the commit and push, or switch to a different branch before running the script. This will help ensure proper version control of the generated files without impacting the main branch.*

## data_01_run.R

This script automates the preprocessing of data for an SS3 model by extracting data from each executed model, filtering and organizing various data types (such as abundance indices, catches, age composition, and weight-at-age), and saving them into `.RData` and `Excel` files for generating input data tables and figures for the executed model. The files are then stored in the `data/run` directory, and finally, the script performs a commit and push to a Git repository, ensuring that all processed results are properly versioned in the current branch of the repository. *To avoid making changes directly to the main repository, it is recommended to either comment out the section that performs the commit and push, or switch to a different branch before running the script. This will help ensure proper version control of the generated files without impacting the main branch.*

## model_01_run.R

This script automates the execution of SS3 model scenarios. It sets up the necessary directories, copies the required data and software for each scenario (such as "S0"), and runs the SS3 model within the specified directory. After running the model, the script performs a commit and push to a Git repository, ensuring that all model results are properly versioned in the current branch. The process includes logging the execution time and managing the version control of the `model/run` directory, making sure that the results from each scenario are accurately tracked and stored. *To avoid making changes directly to the main repository, it is recommended to either comment out the section that performs the commit and push, or switch to a different branch before running the script. This will help ensure proper version control of the generated files without impacting the main branch.*

## model_01_retro.R

This script automates the process of running a retrospective analysis for SS3 model scenarios. It first loads the necessary libraries and sets the working directory. For each model scenario, the script creates a corresponding directory in model/retro, copies the input files from the `model/run` directory, and ensures the SS3 executable is available in the new directory. The script then runs the retrospective analysis over the specified number of years (0 to -5) in each scenario's directory. After the analysis is complete, the working directory is restored to its original location.


## output_01_run.R

This script automates the extraction of key results and the generation of TAF output tables from SS3 model runs. For each model scenario in the `model/run` directory, the script loads the results, generates plots, and saves them in the `output/run` directory, while removing any previously generated plot subdirectories. The results and summaries are stored in `.RData` files. Finally, the script commits and pushes the generated files to a Git repository, ensuring that the processed results are properly versioned in the current branch of the repository. *To avoid making changes directly to the main repository, it is recommended to either comment out the section that performs the commit and push, or switch to a different branch before running the script. This will help ensure proper version control of the generated files without impacting the main branch.*

## output_01_retro.R

This script automates the process of summarizing and saving retrospective analysis results for SS3 model scenarios. It first loads the necessary libraries and sets up directories for storing output. For each scenario in the model/retro directory, the script retrieves the retrospective model outputs for a series of years (0 to -5) using `SSgetoutput`, then summarizes these results with `SSsummarize`. The summarized results and model outputs are saved as `.RData` files in the `output/retro` directory, ensuring that all retrospective analyses are properly stored for each scenario. The script concludes by clearing the workspace.


## report_01_run.R

This script automates the preparation of plots and tables for a report based on the results of SS3 model runs. For each model scenario, the script loads input and output data, generates a series of plots including temporal data coverage, growth curves, catches, age compositions, residuals, and more. Additionally, the script creates summary tables using `flextable` for estimated parameters, time series, and other key diagnostics. The generated plots and tables are saved in the `report/run` directory corresponding to each scenario. Finally, the script performs a commit and push of the generated files to the Git repository, ensuring that all report elements are properly versioned. *To avoid making changes directly to the main repository, it is recommended to either comment out the section that performs the commit and push, or switch to a different branch before running the script. This will help ensure proper version control of the generated files without impacting the main branch.*

## report_01_retro.R

This script automates the generation of plots and tables for retrospective analysis reports of SS3 model scenarios. It first sets up the working directory and loads necessary packages. For each scenario with retrospective analysis results stored in the output/retro directory, the script creates a corresponding report directory in report/retro. It then loads the retrospective data, generates plots showing the retrospective patterns in spawning stock biomass (SSB) and fishing mortality (F), and saves these plots as PNG files. Additionally, the script calculates the retrospective bias metrics (Rho and ForecastRho), organizes these metrics into a table using flextable, and saves the table as both a PNG image and an .RData file. The final output for each scenario is stored in its respective report/retro directory.

# Report_SS3_quarter_with_age_data.Rmd

This R Markdown document generates a comprehensive stock assessment report for the Anchovy Stock in ICES Subdivision 9a South, using age composition data. It automates the creation of figures and tables, covering aspects such as catch trends, abundance indices, age composition, model settings, and retrospective analysis. The report is formatted according to a custom Word template with automatic figure and table captions, references, and a table of contents, ensuring a reproducible and well-structured output that adheres to the ICES Journal of Marine Science citation style.

