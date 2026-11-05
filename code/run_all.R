# Run the full pipeline: clean the raw data, estimate loss aversion, then regenerate the tables, figures,
# numbers and test results in output/ used by thesis_manuscript_latex.tex
# Run from the project root (e.g. open ma_thesis.Rproj), then upload output/ alongside the .tex file to Overleaf

# ----- Setup -----

# Flag telling each script that setup has already been run, so it does not source setup.R again

run_all <- TRUE

source("code/setup.R")

# ----- Data cleaning -----

source("code/raw_data_processing.R")
source("code/loss_aversion_estimation.R")

# ----- Analysis -----

## ----- Main text -----

source("code/descriptive_tables.R")
source("code/descriptive_figures.R")
source("code/distribution_test.R")
source("code/regression_test.R")

## ----- Appendix -----

source("code/appendix/distribution_test_appendix.R")
source("code/appendix/regression_test_appendix.R")

# ----- Clean up -----

# Remove the flag so scripts run on their own afterwards source setup.R again

rm(run_all)
