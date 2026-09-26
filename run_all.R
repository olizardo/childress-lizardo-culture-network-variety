#!/usr/bin/env Rscript
# run_all.R
# Master pipeline: reproduces every figure and table in the paper.
# Usage: Rscript run_all.R

message("=== [1/4] Data preparation & factor models ===")
source("Scripts/01_prepare_data.R")

message("\n=== [2/4] Regression models (Tables 1 & 2) ===")
source("Scripts/02_fit_models.R")

message("\n=== [3/4] Figures ===")
source("Scripts/03_generate_figures.R")

message("\n=== [4/4] Tables ===")
source("Scripts/04_generate_tables.R")

message("\nPipeline complete. See cache/ for tables and Plots/ for figures.")
