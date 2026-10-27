# MAS531 - Food Project
# main.R: runs the whole analysis in order, from a clean start.
# Run this before submitting, to check that everything works from start to finish.

rm(list = ls())

project_dir <- if (file.exists("data/Food_raw.csv")) "." else ".."
scripts <- c(
  "01_cleaning.R",
  "02_descriptives.R",
  "03_bivariate.R",
  "04_tests.R",
  "05_advanced.R"
)

# Each script runs in its own environment, so one script cannot
# accidentally use objects left over from another.
for (s in scripts) {
  cat("\n==========", s, "==========\n")
  source(file.path(project_dir, "scripts", s), local = new.env())
}
