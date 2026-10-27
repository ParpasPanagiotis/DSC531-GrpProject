# MAS531 - Food Project
# 04_tests.R: hypothesis testing (Phase 4).
# Input: data/food_clean.rds (run 01_cleaning.R first).

rm(list = ls())

# Find the project folder, so the script works whether R was started in the
# project folder (as in VS Code) or in scripts/ (as in RStudio).
project_dir <- if (file.exists("data/Food_raw.csv")) "." else ".."
source(file.path(project_dir, "scripts", "00_setup.R"))

# Load the cleaned dataset.
data <- readRDS(clean_file)

# No numerical variable is normal (see 02_descriptives.R), so non-parametric tests are used.
# For every test: state H0 and H1, report the result and interpret it in plain words.

# Planned tests (from Project_Plan.md):
# - Children vs spending: Wilcoxon (HasChildren) and Kruskal-Wallis (Children 0-3).
# - Education vs spending: Kruskal-Wallis, then Dunn post-hoc.
# - Marital status vs spending: Kruskal-Wallis, then Dunn post-hoc
#   (check whether the widow effect is really an age effect).
# - Income vs spending: Spearman correlation test (cor.test).
# - Marital status vs education: Chi-square test of independence.
# - Complaints vs income/spending: Wilcoxon (only 19 complaints: low power).
# - Multiple testing: BH correction across all p-values (p.adjust).
