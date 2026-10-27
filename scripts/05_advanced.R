# MAS531 - Food Project
# 05_advanced.R: customer segments and extra analysis (Phase 5, optional).
# Input: data/food_clean.rds (run 01_cleaning.R first).

rm(list = ls())

# Find the project folder, so the script works whether R was started in the
# project folder (as in VS Code) or in scripts/ (as in RStudio).
project_dir <- if (file.exists("data/Food_raw.csv")) "." else ".."
source(file.path(project_dir, "scripts", "00_setup.R"))

# Load the cleaned dataset.
data <- readRDS(clean_file)

# Planned (from Project_Plan.md):
# - Clustering: scale the variables, k-means (elbow + silhouette to choose k).
# - PCA on the spending and purchase variables.
# - Regression: log(Total_amount_spent) ~ Income + Children + Age + Education + Marital.
#   Note: use factor(schoolinglevel, ordered = FALSE) in lm() for the usual output.
