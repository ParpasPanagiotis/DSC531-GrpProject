# MAS531 - Food Project
# 00_setup.R: shared settings for all scripts

# Paths
data_file <- file.path(project_dir, "data", "Food_raw.csv")
clean_file <- file.path(project_dir, "data", "food_clean.rds")
fig_dir <- file.path(project_dir, "figures")

# Figure folder (no change if already exists)
dir.create(fig_dir, showWarnings = FALSE)

# Copy the plot currently on screen to figures/{name}.png.
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

# Spending columns (their sum should be Total_amount_spent).
mnt_cols <- c(
  "MntWines",
  "MntFruits",
  "MntMeatProducts",
  "MntFishProducts",
  "MntSweetProducts"
)

# Purchase columns (deal purchases not included).
purchase_cols <- c(
  "NumWebPurchases",
  "NumCatalogPurchases",
  "NumStorePurchases"
)

# Marital status dummy columns (combined into Marital_State in 01_cleaning.R).
marital_cols <- c("Divorced", "Married", "Single", "Living_together", "Widow")

# Education dummy columns (combined into schoolinglevel in 01_cleaning.R).
education_cols <- c(
  "Up_to_high_school",
  "Up_to_undergraduate_degree",
  "Up_to_masters_degree",
  "PhD"
)
