# MAS531 - Food Project
# 00_setup.R: shared settings used by every other script.
# Each script sources this file first, so paths and helpers are defined once.

# Paths (project_dir is set by the script that sources this file).
data_file <- file.path(project_dir, "data", "Food_raw.csv")
clean_file <- file.path(project_dir, "data", "food_clean.rds")
fig_dir <- file.path(project_dir, "figures")

# Folder where the figures for the report are saved.
# Recreates it if it is missing - nothing happens if it already exists.
dir.create(fig_dir, showWarnings = FALSE)

# Copy the plot currently on screen to figures/<name>.png.
save_fig <- function(name) {
  dev.copy(
    png,
    file.path(fig_dir, paste0(name, ".png")),
    width = 900,
    height = 650
  )
  invisible(dev.off())
}

# The 15 numerical variables used in the descriptive and correlation analysis.
num_cols <- c(
  "Income",
  "Recency",
  "MntWines",
  "MntFruits",
  "MntMeatProducts",
  "MntFishProducts",
  "MntSweetProducts",
  "NumDealsPurchases",
  "NumWebPurchases",
  "NumCatalogPurchases",
  "NumStorePurchases",
  "NumWebVisitsMonth",
  "Age",
  "Customer_Days",
  "Total_amount_spent"
)
