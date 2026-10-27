# MAS531 - Food Project
# 01_cleaning.R: import, logic checks, cleaning and new variables.
# Output: data/food_clean.rds, the cleaned dataset used by all later scripts.

# Clear the workspace and release unused memory.
rm(list = ls())
gc()

# Find the project folder, so the script works whether R was started in the
# project folder (as in VS Code) or in scripts/ (as in RStudio).
project_dir <- if (file.exists("data/Food_raw.csv")) "." else ".."
source(file.path(project_dir, "scripts", "00_setup.R"))


# 1. IMPORT, CHECKS AND CLEANING --------------------------------------------

# Import the original CSV file.
data <- read.csv(data_file)

# Inspect the structure and the possible values of the two children variables.
str(data)
unique(data$Kidhome)
unique(data$Teenhome)

# Check for missing values.
sum(is.na(data))
# Result: 0 missing values.

# Duplicates: 203 rows are identical to another row in all 27 columns,
# including exact income and exact number of days as a customer.
# Two different customers matching on every column is very unlikely,
# so we treat these rows as repeated entries and remove them.
sum(duplicated(data))
data <- data[!duplicated(data), ]
nrow(data)
# Result: 2205 -> 2002 customers.

# Logic check 1: Total spent = sum(Mnt*)
####################################################

mnt_cols <- c(
  "MntWines",
  "MntFruits",
  "MntMeatProducts",
  "MntFishProducts",
  "MntSweetProducts"
)
all(data$Total_amount_spent == rowSums(data[mnt_cols]))
# Result: TRUE. Total_amount_spent is built from the Mnt* columns, so it
# should not be used in the same test or model as those columns.

# Logic check 2: a customer with 0 purchases should have spent nothing.
purchases <- data$NumWebPurchases +
  data$NumCatalogPurchases +
  data$NumStorePurchases
# Customers with 0 purchases but money spent.
sum(purchases == 0 & data$Total_amount_spent > 0)
# Show how much each of them spent.
data$Total_amount_spent[purchases == 0]
# Result: 4 customers made 0 purchases but spent 4-8, so they are removed.
data <- data[!(purchases == 0 & data$Total_amount_spent > 0), ]
nrow(data)
# 2002 -> 1998 customers.

# Logic check 3: discount purchases cannot exceed purchases in total.
# Recalculate (because rows were removed)
purchases <- data$NumWebPurchases +
  data$NumCatalogPurchases +
  data$NumStorePurchases
sum(data$NumDealsPurchases > purchases)
data[data$NumDealsPurchases > purchases, ]
# Result: 1 customer breaks this rule
data <- data[!(data$NumDealsPurchases > purchases), ]
rownames(data) <- NULL
nrow(data)
# 1998 -> 1997 customers.

# The remaining checks are run on the 1997 customers left after cleaning.
# They all pass (or are kept with a reason), so no more rows are removed.
purchases <- data$NumWebPurchases +
  data$NumCatalogPurchases +
  data$NumStorePurchases

# Logic check 4: each customer has exactly one marital status.
marital_cols <- c("Divorced", "Married", "Single", "Living_together", "Widow")
table(rowSums(data[marital_cols]))
# Result: all 1997 customers have exactly one (every row sums to 1).

# Logic check 5: each customer has exactly one education level.
education_cols <- c(
  "Up_to_high_school",
  "Up_to_undergraduate_degree",
  "Up_to_masters_degree",
  "PhD"
)
table(rowSums(data[education_cols]))
# Result: all 1997 customers have exactly one (every row sums to 1).

# Logic check 6: indicator columns (marital, education, Complain) contain only 0 or 1.
all(unlist(data[c(marital_cols, education_cols, "Complain")]) %in% c(0, 1))
# Result: TRUE.

# Logic check 7: no negative values in any column.
any(data < 0)
# Result: FALSE.

# Logic check 8: all counts and amounts are whole numbers.
all(sapply(data, function(x) all(x == round(x))))
# Result: TRUE.

# Logic check 9: number of small children and teenagers at home.
table(data$Kidhome)
table(data$Teenhome)
# Result: both only take the values 0, 1 and 2.

# Logic check 10: income is positive and age is realistic.
all(data$Income > 0)
range(data$Age)
# Result: TRUE, and ages are 24-80.

# Logic check 11: the last purchase cannot be before the customer joined.
sum(data$Recency > data$Customer_Days)
# Result: 0 (Recency is 0-99 days, Customer_Days is 2159-2858 days).

# Logic check 12: a customer with purchases should have spent something
# (the opposite direction of check 2).
sum(purchases > 0 & data$Total_amount_spent == 0)
# Result: 0.

# Logic check 13: age when the customer joined (age minus years as a customer).
age_at_joining <- data$Age - data$Customer_Days / 365
sum(age_at_joining < 18)
min(age_at_joining)
# Result: 4 customers were 16-18 when they joined (youngest about 16.4).
# We assume that it can happen

# CLEANING LOG (for the report):
# - 0 missing values.
# - 203 duplicate rows removed (2205 -> 2002).
# - 5 rows with impossible purchase counts removed (2002 -> 1997).
# - All other logic checks (4-13) pass.


# 2. GROUP MARITAL STATUS INDICATORS ----------------------------------------

# Combine marital status into one categorical variable.
data$Marital_State <- ifelse(data$Divorced == 1, "Divorced", "")

data$Marital_State <- ifelse(data$Married == 1, "Married", data$Marital_State)

data$Marital_State <- ifelse(data$Single == 1, "Single", data$Marital_State)

data$Marital_State <- ifelse(
  data$Living_together == 1,
  "Living together",
  data$Marital_State
)

data$Marital_State <- ifelse(data$Widow == 1, "Widow", data$Marital_State)

data$Marital_State <- as.factor(data$Marital_State)

# Remove the five original dummy columns (now represented by Marital_State).
data <- data[, !(names(data) %in% marital_cols)]


# 3. GROUP EDUCATION INDICATORS --------------------------------------------

# Combine Education into one categorical variable.
data$schoolinglevel <- ifelse(data$Up_to_high_school == 1, "hs", "")
data$schoolinglevel <- ifelse(
  data$Up_to_undergraduate_degree == 1,
  "bachelor",
  data$schoolinglevel
)
data$schoolinglevel <- ifelse(
  data$Up_to_masters_degree == 1,
  "masters",
  data$schoolinglevel
)
data$schoolinglevel <- ifelse(data$PhD == 1, "phd", data$schoolinglevel)

# Ordered factor, so tables and plots go from lowest to highest education
# (otherwise R sorts the levels alphabetically).
data$schoolinglevel <- factor(
  data$schoolinglevel,
  levels = c("hs", "bachelor", "masters", "phd"),
  ordered = TRUE
)

# Remove the four original education dummy columns.
data <- data[, !(names(data) %in% education_cols)]

# Quick frequency tables for the two new categorical variables.
table(data$Marital_State)
table(data$schoolinglevel)


# 4. NEW VARIABLES FOR THE ANALYSIS ----------------------------------------

# Total number of children at home.
data$Children <- data$Kidhome + data$Teenhome
table(data$Children)

# Yes/no version, used for the two-group comparison in the tests.
data$HasChildren <- factor(ifelse(data$Children > 0, "Yes", "No"))
table(data$HasChildren)

data$AgeGroup <- cut(
  data$Age,
  breaks = c(0, 29, 44, 59, Inf),
  labels = c("Under 30", "30-44", "45-59", "60+") # Create age groups.
)
table(data$AgeGroup)

# Divide customers into four approximately equal income groups.
# The boundaries are the quartiles of income, computed from the data.
income_q <- quantile(data$Income, c(0.25, 0.5, 0.75))
income_q
data$IncomeGroup <- cut(
  data$Income,
  breaks = c(-Inf, income_q, Inf),
  labels = c("Q1 Lowest", "Q2", "Q3", "Q4 Highest")
)
table(data$IncomeGroup)

# Total number of purchases through the Web, Catalogue and Store channels.
# (Customers with zero purchases were removed above, so there is no division by zero.)
data$TotalPurchases <- data$NumWebPurchases +
  data$NumCatalogPurchases +
  data$NumStorePurchases

# Share of each customer's purchases made through each channel (%).
data$WebPercent <- data$NumWebPurchases / data$TotalPurchases * 100
data$CataloguePercent <- data$NumCatalogPurchases / data$TotalPurchases * 100
data$StorePercent <- data$NumStorePurchases / data$TotalPurchases * 100


# 5. SAVE THE CLEANED DATASET -----------------------------------------------

# Everyone uses this file, so nobody cleans the data separately.
saveRDS(data, clean_file)
nrow(data)
# Result: 1997 customers.
