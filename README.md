# Analysis of San Mateo eviction case dataset

This repository contains R analysis scripts and results for the San Mateo eviction dataset. The dataset is not provided here, but everything else is. Note that all R scripts should be run from the top level directory to work.

# Results

- `outputs/frequency_summaries/` contains frequency and percentage summaries for various columns in the dataset where this sort of analysis makes sense.
- `figs/` contains figures visualizing some of the frequency relationships after running `R/analysis_and_figures.R`.

# Requirements to run

- R version >=3, gdata package
- Create a top-level directory with the csv file in it called `data/` and a top-level empty directory to hold figures called `figs/`.