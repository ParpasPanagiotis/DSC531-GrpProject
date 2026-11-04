# ==================================================================
# MAS531 - Group Project "Fo" (Food)
#
# This file contains:
#   PART 0. Setup: paths, helper function and column lists
#   PART 1. Import, logic checks, cleaning and new variables
#   PART 2. Descriptive (univariate) analysis, outliers and normality
#   PART 3. Bivariate analysis: correlations and relationships
#   PART 4. Hypothesis testing
#   PART 5. PCA, clustering (customer segments) and regression
#
# Input:  the data CSV file, chosen in a window when the file is run
# Output: food_clean.rds (cleaned data) and all plots in a figures/
#         folder, both saved in the same folder as the CSV file
# Packages needed: e1071, nortest, ggplot2
# ==================================================================

# Clear the workspace and release unused memory.
rm(list = ls())
gc()

# ##################################################################
# PART 0. SETUP
# ##################################################################

# Paths
# Select the data CSV file
data_file <- file.choose()

# The cleaned data and the figures are saved in the same folder as the CSV.
clean_file <- file.path(dirname(data_file), "food_clean.rds")
fig_dir <- file.path(dirname(data_file), "figures")

# Create figure folder
dir.create(fig_dir, showWarnings = FALSE)

# Copy the plot currently on screen to figures/{name}.png.
save_fig <- function(name) {
  dev.copy(
    png,
    file.path(fig_dir, paste0(name, ".png")),
    width = 6,
    height = 4.5,
    units = "in", # size in inches instead of pixels
    res = 150 # resolution: 150 dots per inch (sharp but not huge)
  )
  invisible(dev.off())
}

# The 15 numerical variables
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

# Spending columns
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

# Marital status dummy columns
marital_cols <- c("Divorced", "Married", "Single", "Living_together", "Widow")

# Education dummy columns
education_cols <- c(
  "Up_to_high_school",
  "Up_to_undergraduate_degree",
  "Up_to_masters_degree",
  "PhD"
)


# ##################################################################
# PART 1. IMPORT, CHECKS, CLEANING AND NEW VARIABLES
# ##################################################################

# ------------------------------
# 1. IMPORT, CHECKS AND CLEANING
# ------------------------------

# Import the original CSV file.
data <- read.csv(data_file)

# Inspect the structure and the possible values of the two children variables.
str(data)
unique(data$Kidhome)
unique(data$Teenhome)

# Check for missing values.
sum(is.na(data))
# Result: 0 missing values.

#Check for duplicates
sum(duplicated(data))
# Duplicates: 203 rows identical to another row in all 27 columns,
# including exact income and exact number of days as a customer.

# Two different customers matching on every column is very unlikely,
# so we treat these rows as repeated entries and remove them.
data <- data[!duplicated(data), ]
nrow(data)
# Result: 2205 -> 2002 customers.

#Create TotalPurchases = Total number of purchases through the Web, Catalogue and Store channels.
data$TotalPurchases <- rowSums(data[purchase_cols])

# Logic check 1: Total spent = sum(Mnt*)
####################################################

all(data$Total_amount_spent == rowSums(data[mnt_cols]))
# Result: TRUE. Total_amount_spent is built from the Mnt* columns, so it
# should not be used in the same test or model as those columns.

# Logic check 2: a customer with 0 purchases should have spent nothing.
####################################################

# Customers with 0 purchases but money spent.
sum(data$TotalPurchases == 0 & data$Total_amount_spent > 0)

# Show how much each of them spent.
data$Total_amount_spent[data$TotalPurchases == 0]
# Result: 4 customers made 0 purchases but spent 4-8, so they are removed.
data <- data[!(data$TotalPurchases == 0 & data$Total_amount_spent > 0), ]
nrow(data)
# 2002 -> 1998 customers.

# Logic check 3: n of discount purchases < n of total purchases
####################################################

sum(data$NumDealsPurchases > data$TotalPurchases)
data[data$NumDealsPurchases > data$TotalPurchases, ]
# Result: 1 customer breaks this rule
data <- data[!(data$NumDealsPurchases > data$TotalPurchases), ]
nrow(data)
# 1998 -> 1997 customers.

# The remaining checks are run on the 1997 customers left after cleaning.
# They all pass (or are kept with a reason), so no more rows are removed.

# Logic check 4: each customer has exactly one marital status.
# (marital_cols and education_cols are defined in PART 0.)
table(rowSums(data[marital_cols]))
# Result: all 1997 customers have exactly one (every row sums to 1).

# Logic check 5: each customer has exactly one education level.
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
sum(data$TotalPurchases > 0 & data$Total_amount_spent == 0)
# Result: 0.

# Logic check 13: age when the customer joined (age minus years as a customer).
age_at_joining <- data$Age - data$Customer_Days / 365
sum(age_at_joining < 18)
min(age_at_joining)
# Result: 4 customers were 16-18 when they joined (youngest about 16.4).
# We assume that it can happen

# CLEANING LOG:
# - 0 missing values.
# - 203 duplicate rows removed (2205 -> 2002).
# - 5 rows with impossible purchase counts removed (2002 -> 1997).
# - All other logic checks (4-13) pass.

# ----------------------------------
# 2. GROUP MARITAL STATUS INDICATORS
# ----------------------------------

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

# Turn it into categorical variable
data$Marital_State <- as.factor(data$Marital_State)

# Remove the five original dummy columns (now represented by Marital_State).
data <- data[, !(names(data) %in% marital_cols)]

# -----------------------------
# 3. GROUP EDUCATION INDICATORS
# -----------------------------

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

# Turn it into ordered factor from lowest to highest education
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

# ---------------------------------
# 4. NEW VARIABLES FOR THE ANALYSIS
# ---------------------------------

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

# Share of each customer's purchases made through each channel (%).
# (TotalPurchases was created before logic check 2. Customers with zero
# purchases were removed there, so there is no division by zero.)
data$WebPercent <- data$NumWebPurchases / data$TotalPurchases * 100
data$CataloguePercent <- data$NumCatalogPurchases / data$TotalPurchases * 100
data$StorePercent <- data$NumStorePurchases / data$TotalPurchases * 100

# Share of each customer's purchases made with a discount (%).
# (Logic check 3 made sure deal purchases never exceed total purchases.)
data$DealPercent <- data$NumDealsPurchases / data$TotalPurchases * 100

# ---------------------------
# 5. SAVE THE CLEANED DATASET
# ---------------------------

# Everyone uses this file, so nobody cleans the data separately.
saveRDS(data, clean_file)
nrow(data)
# Result: 1997 customers.


# ####################################################
# PART 2. DESCRIPTIVE ANALYSIS, OUTLIERS AND NORMALITY
# ####################################################

# ----------------------------------------------
# NUMERICAL VARIABLES AND DESCRIPTIVE STATISTICS
# ----------------------------------------------

# The 15 numerical variables (listed in PART 0)
dataN <- data[num_cols]

# Load the e1071 package for skewness() and kurtosis().
library(e1071)

# Describe one numeric variable: print its statistics and draw its boxplot
# and histogram.
# v is the column name as text (e.g. "Income").
# (,save = TRUE) saves both plots to figures/ (default is False)
describe_var <- function(v, colour = "lightblue", save = FALSE) {
  # Input checks:
  # 1. v must be a column of data
  if (!(v %in% names(data))) {
    stop("'", v, "' is not a variable in the data. Check the spelling.")
  }
  # 2. The column must be numerical (e.g. Marital_State is a factor).
  if (!is.numeric(data[[v]])) {
    stop("'", v, "' is not a numerical variable.")
  }

  x <- data[[v]]

  # Descriptive statistics: range, centre, variability and shape.
  min_value <- min(x)
  max_value <- max(x)
  m <- mean(x)
  med <- median(x)
  variance <- var(x)
  standard_dev <- sd(x)
  iqr <- IQR(x)
  skew <- skewness(x)
  kurt <- kurtosis(x)
  frequency <- table(x)
  mode_value <- names(frequency)[which.max(frequency)]

  # Print the results in the Console.
  cat("\n-----------------------------\n")
  cat("Variable:", v, "\n")
  cat("Min:", min_value, "\n")
  cat("Max:", max_value, "\n")
  cat("Mean:", m, "\n")
  cat("Median:", med, "\n")
  cat("Mode:", mode_value, "\n")
  cat("Variance:", variance, "\n")
  cat("Standard deviation:", standard_dev, "\n")
  cat("IQR:", iqr, "\n")
  cat("Skewness:", skew, "\n")
  cat("Excess kurtosis:", kurt, "\n")

  # Split the graph window
  if (!save) {
    par(mfrow = c(1, 2))
  }

  # Boxplot: useful for spread and potential outliers.
  boxplot(x, main = paste0("Boxplot for ", v), col = colour, ylab = v)
  if (save) {
    save_fig(paste0("box_", v))
  }

  # Histogram: shows the distribution of the observed values.
  hist(
    x,
    breaks = 20,
    xlab = v,
    main = paste0("Histogram for ", v),
    col = colour
  )
  if (save) {
    save_fig(paste0("hist_", v))
  }

  # Back to one plot per window
  if (!save) par(mfrow = c(1, 1))
}

# Describe all 15 numeric variables and save all their figures
colours <- rep(c("red", "blue", "darkgreen", "gold", "purple"), 3)
for (i in seq_along(num_cols)) {
  describe_var(num_cols[i], colours[i], save = TRUE)
}
# Normality is checked separately below (NORMALITY TESTING), and only for
# the variables whose histogram looks roughly bell-shaped.

# Observations:
# All values are in a sensible range: no negative amounts, Age 24-80,
# Income 2,447-113,734.
# For the spending columns the mean is far above the median
# (e.g. MntFruits: median 8, mean 26.5), so they are strongly right-skewed.

# -------------
# OUTLIER CHECK
# -------------

# Count values outside the 1.5 x IQR rule
iqr_outliers <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  sum(x < q[1] - 1.5 * IQR(x) | x > q[2] + 1.5 * IQR(x))
}
sapply(dataN, iqr_outliers)
# Result: Income, Age, Recency, Customer_Days and NumStorePurchases have no outliers.
# The spending columns have many (e.g. 220 for fruits, 166 for meat), but the
# histograms and skewness above show these come from the strong right skew,
# not from errors: a few customers simply spend a lot. They are real customers,
# so all of them are kept.

# --------------------------------------------------------
# DESCRIPTIVE ANALYSIS: CATEGORICAL AND DISCRETE VARIABLES
# --------------------------------------------------------

# Number of Small Children per Household. This analysis shows how many customers have 0, 1, or more small children in their households.
table(data$Kidhome)
round(prop.table(table(data$Kidhome)) * 100, 2)
barplot(
  table(data$Kidhome),
  main = "Number of Small Children per Household",
  xlab = "Number of Small Children",
  ylab = "Number of Customers",
  col = "lightblue",
  ylim = c(0, 1200) # so the axis goes above the tallest bar
)
save_fig("bar_Kidhome")
# Observation:
# Most customers have no small children (57.79%). Only 2.05% of customers have two small children.

# Number of Teenagers per Household
table(data$Teenhome)
round(prop.table(table(data$Teenhome)) * 100, 2)
barplot(
  table(data$Teenhome),
  main = "Number of Teenagers per Household",
  xlab = "Number of Teenagers",
  ylab = "Number of Customers",
  col = "lightblue",
  ylim = c(0, 1200) # so the axis goes above the tallest bar
)
save_fig("bar_Teenhome")
# Observation:
# Most customers have no teenagers at home (51.58%).
# Customers with one teenager represent 45.97%, while only 2.45% have two.

# Customer Complaints. Complain is a binary variable: 0 indicates no complaint and 1 indicates a complaint. The percentages show how common complaints are among customers.
table(data$Complain)
round(prop.table(table(data$Complain)) * 100, 2)
# Observation:
# Most customers (99.05%) have not reported a complaint. Only 19 customers (0.95%) have complained,
# so any test on complaints will have very little power.

# Marital Status.
table(data$Marital_State)
round(prop.table(table(data$Marital_State)) * 100, 2)
barplot(
  table(data$Marital_State),
  main = "Distribution of Marital Status",
  xlab = "Marital Status",
  ylab = "Number of Customers",
  col = "lightblue",
  ylim = c(0, 800), # so the axis goes above the tallest bar (777)
  cex.names = 0.8 # smaller bar labels, so "Living together" fits
)
save_fig("bar_Marital")
# Observation:
# Married customers form the largest group (38.91%), followed by those living together (25.29%). Widowed customers represent the smallest group (3.46%).

# Education Level.
table(data$schoolinglevel)
round(prop.table(table(data$schoolinglevel)) * 100, 2)
barplot(
  table(data$schoolinglevel),
  main = "Distribution of Education Levels",
  xlab = "Education Level",
  ylab = "Number of Customers",
  col = "lightgreen"
)
save_fig("bar_Education")
# Observation:
# Bachelor's degree holders form the largest group (50.38%). Customers with high-school-level education form the smallest group (2.45%, 49 customers).

# -----------------
# NORMALITY TESTING
# -----------------

# A normality test only makes sense when the histogram looks roughly bell-shaped.
# From the histograms above, 13 of the 15 variables are clearly not normal, so they are not tested:
# -> Strongly right-skewed:
#       `Total_amount_spent`,
#       `NumDealsPurchases`,
#       `NumWebPurchases`,
#       `NumCatalogPurchases`,
#       `NumStorePurchases`
# -> Flat (~uniform):
#       `Recency`,
#       `Customer_Days`
# -> Piled up on the right with a few extreme high values: `NumWebVisitsMonth`.
# Only `Income` and `Age` are roughly symmetric with one main hump, so only they are tested.

# Load the nortest package for lillie.test()
library(nortest)

for (v in c("Income", "Age")) {
  x <- data[[v]]

  # Normal Q-Q plot: points far from the line suggest non-normality.
  qqnorm(x, main = paste0("Normal Q-Q plot for ", v), cex = 0.8)
  qqline(x, col = "red", lwd = 2)
  save_fig(paste0("qq_", v))

  print("----------------------------------")
  # Kolmogorov-Smirnov test against a normal curve with the same mean and SD.
  # H0: the data are normal; H1: they are not.
  # suppressWarnings() hides the "ties" warning: Age is in whole years,
  # so many customers share the same value.
  cat("\nVariable:", v, "\n")
  print(suppressWarnings(ks.test(x, "pnorm", mean = mean(x), sd = sd(x))))

  # Lilliefors test: the K-S test corrected for estimating the mean and SD
  # from the same data (the plain K-S p-value is too large in that case).
  print(lillie.test(x))
  print("----------------------------------")
}
# Observations:
# Both tests reject normality for Income and Age (all p-values < 0.001).
# - Income: D = 0.047 (K-S p = 0.0003, Lilliefors p < 0.001).
# - Age:    D = 0.058 (K-S p < 0.001, Lilliefors p < 0.001).
# D is the same in both tests, but the Lilliefors p-value is much smaller:
# the plain K-S test is too lenient when the mean and SD come from the data.
# The gaps from a normal curve are small (D < 0.06), but with about 2000 customers even
# small gaps are significant. The Q-Q plots show the kind of difference: the points bend
# away from the line at both ends (flatter top and thinner tails than a normal curve).
# Age has many tied values (only 56 different ages), which also affects the K-S test.
# Conclusion: none of the 15 numerical variables is normal, so the later analysis uses
# non-parametric methods (Spearman, Wilcoxon, Kruskal-Wallis).


# ##################################################################
# PART 3. BIVARIATE ANALYSIS
# ##################################################################

# Load ggplot2 for the heatmap and the violin plot
library(ggplot2)

# -----------------------------
# SPEARMAN CORRELATION ANALYSIS
# -----------------------------

cor_matrix <- cor(dataN, method = "spearman")

# Heatmap with ggplot2 (base R heatmap() cannot draw a colour scale bar).
# ggplot needs one row per pair of variables: as.table() turns the matrix
# into a table, and as.data.frame() into columns Var1, Var2 and the value.
cor_long <- as.data.frame(as.table(cor_matrix))
names(cor_long) <- c("Var1", "Var2", "rho")

p_heatmap <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = rho)) +
  geom_tile(color = "white") + # one coloured square per pair
  geom_text(aes(label = round(rho, 2)), size = 2.5) + # the value in each square
  scale_fill_gradient2( # blue = negative, white = 0, red = positive
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Spearman\nrho"
  ) +
  labs(title = "Spearman Correlation Heatmap", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # slanted names
print(p_heatmap)
ggsave(
  file.path(fig_dir, "heatmap_spearman.png"),
  p_heatmap,
  width = 9,
  height = 7.5,
  bg = "white"
)
# Examine correlations specifically with Total Spending.
round(cor_matrix["Total_amount_spent", ], 2)

# Observations:
# Income is strongly positively correlated with Total Spending (rho = 0.87).
# Wine and meat spending each have rho = 0.94 with Total Spending, but this is largely built in: Total Spending is the sum of the Mnt* columns, and wine and meat are its two largest parts.
# Website visits show a moderate negative correlation (rho = -0.47).
# Recency shows almost no correlation with Total Spending (rho = 0.02).

# ------------------------------------
# TOTAL SPENDING BY NUMBER OF CHILDREN
# ------------------------------------

# The violin plot shows the concentration of spending values, while the boxplot shows the median and quartiles.
p_children <- ggplot(
  data,
  aes(
    x = factor(Children),
    y = Total_amount_spent
  )
) +
  geom_violin(fill = "lightblue", color = "black") +
  geom_boxplot(width = 0.15, fill = "white") +
  labs(
    title = "Total Spending by Number of Children",
    x = "Number of Children",
    y = "Total Amount Spent"
  )
print(p_children)
ggsave(
  file.path(fig_dir, "violin_spending_children.png"),
  p_children,
  width = 6,
  height = 4.5,
  bg = "white"
)
# Display median spending for each group.
tapply(data$Total_amount_spent, data$Children, median)

# Observations:
# Median spending decreases as the number of children increases,
# from 1117 (0 children) to 62 (3 children).
# The 3-children group contains only 45 customers, so interpret cautiously.

# ---------------------------------
# TOTAL SPENDING BY EDUCATION LEVEL
# ---------------------------------
tapply(
  data$Total_amount_spent,
  data$schoolinglevel,
  median
)
boxplot(
  Total_amount_spent ~ schoolinglevel,
  data = data,
  main = "Total Spending by Education Level",
  xlab = "Education Level",
  ylab = "Total Amount Spent",
  col = "lightblue"
)
save_fig("box_spending_education")
# Observations:
# PhD customers have the highest median spending (448.5), while HS have the lowest (40).
# Spending does not consistently increase with education level: bachelor (353) is above masters (297).
# The HS group has only 49 customers

# --------------------------------
# TOTAL SPENDING BY MARITAL STATUS
# --------------------------------
tapply(data$Total_amount_spent, data$Marital_State, median)
boxplot(
  Total_amount_spent ~ Marital_State,
  data = data,
  main = "Total Spending by Marital Status",
  xlab = "Marital Status",
  ylab = "Total Amount Spent",
  col = "lightblue",
  cex.axis = 0.8 # smaller axis labels, so "Living together" fits
)
save_fig("box_spending_marital")
# Observations:
# Widowed customers have the highest median spending (597
# Other groups are similar median (303-350).
# The widow group is small (69 customers) and likely older, so this may reflect age rather than marital status.

# ------------------------
# INCOME VS TOTAL SPENDING
# ------------------------
plot(
  data$Income,
  data$Total_amount_spent,
  main = "Income vs Total Spending",
  xlab = "Income",
  ylab = "Total Amount Spent",
  col = rgb(0, 0, 1, 0.3),
  pch = 16
)
# Smooth curve to show the trend without assuming a straight line.
lines(lowess(data$Income, data$Total_amount_spent), col = "red", lwd = 2)
save_fig("scatter_income_spending")
# Observations:
# Spending rises clearly with income (Spearman rho = 0.87).
# The relationship is curved: spending stays low up to about 40,000 and then rises steeply.

# -------------------------------
# TOTAL PURCHASES ACROSS CHANNELS
# -------------------------------

# Calculate the average customer-level purchase percentage for each channel.
channel_percentages <- c(
  Web = mean(data$WebPercent),
  Catalogue = mean(data$CataloguePercent),
  Store = mean(data$StorePercent)
)
round(channel_percentages, 2)

# barplot() returns the x-position of each bar, used to place the labels.
bar_pos <- barplot(
  channel_percentages,
  main = "Average Percentage of Purchases by Channel",
  xlab = "Purchasing Channel",
  ylab = "Average Percentage (%)",
  col = "#043f53",
  ylim = c(0, 60)
)
# Write each percentage just above its bar.
text(
  bar_pos,
  channel_percentages,
  labels = paste0(round(channel_percentages, 1), "%"),
  pos = 3
)
save_fig("bar_channels")

# Observations:
# Store has the highest average purchase share (50.5%),
# followed by Web (33.2%) and Catalogue (16.4%).
# These percentages are averages across customers.

# -------------------------------------------
# PURCHASING CHANNEL PREFERENCES BY AGE GROUP
# -------------------------------------------
channel_age_percent <- rbind(
  Web = tapply(data$WebPercent, data$AgeGroup, mean),
  Catalogue = tapply(data$CataloguePercent, data$AgeGroup, mean),
  Store = tapply(data$StorePercent, data$AgeGroup, mean)
)
round(channel_age_percent, 2)
bar_pos <- barplot(
  channel_age_percent,
  beside = TRUE,
  main = "Average Purchasing Channel Percentages
by Age Group",
  xlab = "Age Group",
  ylab = "Average Percentage of Purchases (%)",
  col = c("lightblue", "lightgreen", "lightpink"),
  legend.text = TRUE,
  ylim = c(0, 70)
)
# Write each percentage just above its bar.
text(
  bar_pos,
  channel_age_percent,
  labels = round(channel_age_percent, 1),
  pos = 3,
  cex = 0.8
)
save_fig("bar_channels_age")

# Observations:
# Store has the highest average purchase share in every age group.
# Customers aged 30-44 have the highest Store percentage (52.9%).
# Web purchases have the highest share among customers aged 45-59 (34.0%).
# Under-30 customers have the highest Catalogue percentage (25.7%), but this group contains only 37 customers.

# -------------------------------
# TOTAL PURCHASES BY INCOME GROUP
# -------------------------------
purchases_income <- tapply(
  data$TotalPurchases,
  data$IncomeGroup,
  mean
)
round(purchases_income, 2)
bar_pos <- barplot(
  purchases_income,
  main = "Average Total Purchases by Income Group",
  xlab = "Income Group",
  ylab = "Average Number of Purchases",
  col = "lightblue",
  ylim = c(0, 22) # so the axis goes above the tallest bar (19.4)
)
text(
  bar_pos,
  purchases_income,
  labels = round(purchases_income, 1),
  pos = 3,
  cex = 1.3
)
save_fig("bar_purchases_income")

# Observations:
# Average total purchases increase across income groups,
# from 5.5 in Q1 (lowest income) to 19.4 in Q4 (highest income). This suggests that higher-income customers tend to make more purchases.

# ------------------------------------------------------
# DISCOUNT PURCHASES BY NUMBER OF CHILDREN
# ------------------------------------------------------
# Are families more price-sensitive?

tapply(data$NumDealsPurchases, data$Children, median)
round(tapply(data$DealPercent, data$Children, mean), 2)
boxplot(
  DealPercent ~ Children,
  data = data,
  main = "Share of Purchases Made with a Discount,
by Number of Children",
  xlab = "Number of Children",
  ylab = "Discount Purchases (% of all purchases)",
  col = "lightblue"
)
save_fig("box_deals_children")

# Spearman correlations of the discount share with children.
round(cor(data$DealPercent, data$Children, method = "spearman"), 2)
# ------------------------------------------------------
# DISCOUNT PURCHASES BY INCOME
# ------------------------------------------------------
# Are lower-income customers more price-sensitive?

round(tapply(data$DealPercent, data$IncomeGroup, mean), 2)
boxplot(
  DealPercent ~ IncomeGroup,
  data = data,
  main = "Share of Purchases Made with a Discount,
by Income Group",
  xlab = "Income Group",
  ylab = "Discount Purchases (% of all purchases)",
  col = "lightblue"
)
save_fig("box_deals_income")

# Spearman correlations of the discount share with income.
round(cor(data$DealPercent, data$Income, method = "spearman"), 2)

# Observations:
# Families with more children rely much more on discounts. The average discount share rises from 8.8% (no children) to 24.9% (1 child) and about 43% (2 or 3 children).
# The discount share also falls steadily with income: 36.4% in Q1, 32.6% in Q2, 19.7% in Q3 and only 7.6% in Q4 (Spearman rho = -0.73).
# So: families with more kids and lower-income customers are clearly more price-sensitive.

# ---------------------------
# WEB VISITS VS WEB PURCHASES
# ---------------------------
# Do visits to the website turn into purchases on the website?

plot(
  jitter(data$NumWebVisitsMonth),
  jitter(data$NumWebPurchases),
  main = "Web Visits (last month) vs Web Purchases",
  xlab = "Number of Web Visits Last Month",
  ylab = "Number of Web Purchases",
  col = rgb(0, 0, 1, 0.3),
  pch = 16
)
# jitter() moves each point slightly at random, so customers with the same whole-number values do not hide behind each other.
lines(
  lowess(data$NumWebVisitsMonth, data$NumWebPurchases),
  col = "red",
  lwd = 2
)
save_fig("scatter_webvisits_webpurchases")

round(cor(data$NumWebVisitsMonth, data$NumWebPurchases, method = "spearman"), 2)

# Who visits the website most often? We compare customers by number of visits.
visit_group <- cut(
  data$NumWebVisitsMonth,
  breaks = c(-1, 2, 5, 8, Inf),
  labels = c("0-2", "3-5", "6-8", "9+")
)
table(visit_group)
round(tapply(data$NumWebPurchases, visit_group, mean), 1)
tapply(data$Income, visit_group, median)
tapply(data$Total_amount_spent, visit_group, median)

# Observations:
# Web visits do not turn into web purchases: the correlation is almost zero (rho = -0.09), and customers make about 4 web purchases on average whatever their number of visits.
# The customers who visit most often are the lower-income, lower-spending ones: median income falls from 77,382 (0-2 visits) to 31,814 (9+ visits), and median spending from 1,112 to 118. Browsing for offers rather than buying?
# Note: visits cover the last month only, while purchases cover a longer period.

# -------------------------------------------------------
# RECENCY AND LENGTH OF CUSTOMER RELATIONSHIP VS SPENDING
# -------------------------------------------------------
# Are long-term customers more valuable? Do recent buyers spend more?

plot(
  data$Recency,
  data$Total_amount_spent,
  main = "Days Since Last Purchase vs Total Spending",
  xlab = "Days Since Last Purchase (Recency)",
  ylab = "Total Amount Spent",
  col = rgb(0, 0, 1, 0.3),
  pch = 16
)
lines(lowess(data$Recency, data$Total_amount_spent), col = "red", lwd = 2)
save_fig("scatter_recency_spending")

plot(
  data$Customer_Days,
  data$Total_amount_spent,
  main = "Days as a Customer vs Total Spending",
  xlab = "Days as a Customer",
  ylab = "Total Amount Spent",
  col = rgb(0, 0, 1, 0.3),
  pch = 16
)
lines(lowess(data$Customer_Days, data$Total_amount_spent), col = "red", lwd = 2)
save_fig("scatter_customerdays_spending")

round(cor(data$Recency, data$Total_amount_spent, method = "spearman"), 2)
round(cor(data$Customer_Days, data$Total_amount_spent, method = "spearman"), 2)

# Median spending for each quarter of customers, from newest to longest-standing.
customer_days_group <- cut(
  data$Customer_Days,
  breaks = quantile(data$Customer_Days, c(0, 0.25, 0.5, 0.75, 1)),
  labels = c("Newest 25%", "Q2", "Q3", "Longest 25%"),
  include.lowest = TRUE
)
tapply(data$Total_amount_spent, customer_days_group, median)

# Observations:
# Recency has no relationship with spending (rho = 0.02)
# How long ago a customer last bought tells us nothing about how much they spend.
# Length of relationship has a weak positive relationship with spending (rho = 0.18). Comparing the medians: from 150.5 for the newest customers, to 523 for the longest customers. Spending (Total_amount_spent) covers the last 2 years only, and every customer has been with the company for at least 5.9 years, so all had the full 2 years. So long-term customers spend more over the same period.


# ##########################
# PART 4. HYPOTHESIS TESTING
# ##########################

# APPROACH
# No numerical variable is normal we use so non-parametric tests:
# - Two groups vs a number:   Wilcoxon rank-sum test (Mann-Whitney), wilcox.test()
# - 3+ groups vs a number:    Kruskal-Wallis test, kruskal.test()
#   If it is significant, pairwise Wilcoxon tests (pairwise.wilcox.test) show WHICH
#   groups differ, with the BH correction for the many pairs compared.
# - Number vs number:         Spearman correlation test, cor.test(method = "spearman")
# - Group vs group:           Chi-square test of independence, chisq.test()
# For every test: H0 and H1, the result, and what it means in plain words.
# Significance level: 0.05.
# Every main p-value is stored in p_values, and corrected for multiple testing at the end.
p_values <- c()

# exact = FALSE in cor.test(): with repeated values R cannot compute an exact p-value, so it uses an approximation.

# ----------------------------------------------------
# TEST 1: SPENDING, CUSTOMERS WITH VS WITHOUT CHILDREN
# ----------------------------------------------------
# Do customers with children spend less?
# H0: spending is the same for customers with and without children.
# H1: spending differs between the two groups.
tapply(data$Total_amount_spent, data$HasChildren, median)
test1 <- wilcox.test(Total_amount_spent ~ HasChildren, data = data)
test1
p_values["children (yes/no)"] <- test1$p.value
# Result: p < 0.001, reject H0.
# Customers without children spend far more: median 1117 vs 182 for customers with children.

# --------------------------------------------
# TEST 2: SPENDING BY NUMBER OF CHILDREN (0-3)
# --------------------------------------------
# H0: spending is the same for all numbers of children.
# H1: at least one group spends differently.
tapply(data$Total_amount_spent, data$Children, median)
test2 <- kruskal.test(Total_amount_spent ~ Children, data = data)
test2
p_values["number of children"] <- test2$p.value

# Which groups differ?
pairwise.wilcox.test(
    data$Total_amount_spent,
    data$Children,
    p.adjust.method = "BH"
)
# Result: p < 0.001, reject H0.
# Spending falls with every extra child from 0 to 2 but the groups with 2 or 3 children do not differ much (p = 0.15),

# -----------------------------------
# TEST 3: SPENDING BY EDUCATION LEVEL
# -----------------------------------
# H0: spending is the same for all education levels.
# H1: at least one level spends differently.
tapply(data$Total_amount_spent, data$schoolinglevel, median)
test3 <- kruskal.test(Total_amount_spent ~ schoolinglevel, data = data)
test3
p_values["education"] <- test3$p.value

# Which levels differ?
pairwise.wilcox.test(
    data$Total_amount_spent,
    data$schoolinglevel,
    p.adjust.method = "BH"
)
# Result: p < 0.001, reject H0.
# - High school customers spend much less than every other level (all p < 0.001; median 40).
# - PhD customers spend more than master's customers (p = 0.004; median 448.5 vs 297).
# - Bachelor's does not differ significantly from master's (p = 0.10) or PhD (p = 0.07).
# So the clear finding is high school vs the rest; above that, education matters little.
# The high school group is small (49 customers).

# ----------------------------------
# TEST 4: SPENDING BY MARITAL STATUS
# ----------------------------------
# H0: spending is the same for all marital statuses.
# H1: at least one status spends differently.
tapply(data$Total_amount_spent, data$Marital_State, median)
test4 <- kruskal.test(Total_amount_spent ~ Marital_State, data = data)
test4
p_values["marital status"] <- test4$p.value
# Result: p = 0.24, do not reject H0.
# There is no evidence that spending differs by marital status. Widows have the highest
# median (597), but the group is small (69) and the difference is not significant.

# Is the higher widow spending really an age effect?
# Widows are older than the other groups (median age 63 vs 48-53):
tapply(data$Age, data$Marital_State, median)
# and older customers spend more (Spearman rho = 0.18, p < 0.001):
cor.test(data$Age, data$Total_amount_spent, method = "spearman", exact = FALSE)

# Compare widows with other customers of the same age (60 and over only).
older <- data[data$Age >= 60, ]
tapply(older$Total_amount_spent, older$Marital_State == "Widow", median)
wilcox.test(
    older$Total_amount_spent[older$Marital_State == "Widow"],
    older$Total_amount_spent[older$Marital_State != "Widow"]
)
# Result: p = 0.67. Among customers aged 60+, widows (44 customers, median 606) spend about the same as non-widows (median 546).
# So the higher widow spending is explained by age and not by marital status.

# --------------------------
# TEST 5: INCOME VS SPENDING
# --------------------------
# H0: there is no monotonic relationship between income and spending (rho = 0).
# H1: there is one (rho is not 0).
test5 <- cor.test(
    data$Income,
    data$Total_amount_spent,
    method = "spearman",
    exact = FALSE
)
test5
p_values["income"] <- test5$p.value
# Result: rho = 0.87, p < 0.001, reject H0.
# A very strong positive relationship: higher-income customers spend much more.

# -----------------------------------
# TEST 6: MARITAL STATUS VS EDUCATION
# -----------------------------------
# H0: marital status and education are independent.
# H1: they are related.
marital_education <- table(data$Marital_State, data$schoolinglevel)
marital_education
test6 <- chisq.test(marital_education)
test6
p_values["marital vs education"] <- test6$p.value

# Result: p = 0.26, do not reject H0.
# There is no evidence that marital status and education are related.

# ------------------
# TEST 7: COMPLAINTS
# ------------------
# Do customers who complained differ in income or spending?
# H0: income (or spending) is the same for customers who did and did not complain.
# H1: it differs.
tapply(data$Income, data$Complain, median)
test7a <- wilcox.test(Income ~ Complain, data = data)
test7a
p_values["complaints: income"] <- test7a$p.value

tapply(data$Total_amount_spent, data$Complain, median)
test7b <- wilcox.test(Total_amount_spent ~ Complain, data = data)
test7b
p_values["complaints: spending"] <- test7b$p.value
# Result: income p = 0.22, spending p = 0.13, do not reject H0.
# Customers who complained have lower medians (income 39,684 vs 51,566; spending 89 vs 345), but the differences are not significant.

# --------------------------------------------
# TEST 8: DISCOUNT SHARE BY NUMBER OF CHILDREN
# --------------------------------------------
# Are families more price-sensitive?
# H0: the share of purchases made with a discount is the same for all numbers of children.
# H1: at least one group differs.
tapply(data$DealPercent, data$Children, median)
test8 <- kruskal.test(DealPercent ~ Children, data = data)
test8
p_values["discounts: children"] <- test8$p.value

# Which groups differ?
pairwise.wilcox.test(data$DealPercent, data$Children, p.adjust.method = "BH")
# Result: p < 0.001, reject H0.
# The discount share rises with each child from 0 to 2 (all pairs differ, p < 0.001).
# We have: median 5.9% (0), 25.0% (1), 43.3% (2). 2 and 3 children do not differ (p = 0.9).

# --------------------------------
# TEST 9: DISCOUNT SHARE VS INCOME
# --------------------------------
# Are lower-income customers more price-sensitive?
# H0: no monotonic relationship between discount share and income.
# H1: there is one.
test9 <- cor.test(
    data$DealPercent,
    data$Income,
    method = "spearman",
    exact = FALSE
)
test9
p_values["discounts: income"] <- test9$p.value
# Result: rho = -0.73, p < 0.001, reject H0.
# A strong negative relationship: the lower the income, the larger the share of purchases made with a discount.

# ------------------------------------
# TEST 10: WEB VISITS VS WEB PURCHASES
# ------------------------------------
# Do website visits turn into website purchases?
# H0: no monotonic relationship between web visits and web purchases.
# H1: there is one.
test10 <- cor.test(
    data$NumWebVisitsMonth,
    data$NumWebPurchases,
    method = "spearman",
    exact = FALSE
)
test10
p_values["web visits vs purchases"] <- test10$p.value
# Result: rho = -0.09, p < 0.001, reject H0.
# Statistically significant, but the relationship is very weak (and slightly negative):
# with about 2000 customers, even a tiny correlation is significant.

# -------------------------------------------
# TEST 11: LENGTH OF RELATIONSHIP VS SPENDING
# -------------------------------------------
# Are long-term customers more valuable?
# H0: no monotonic relationship between days as a customer and spending.
# H1: there is one.
test11 <- cor.test(
    data$Customer_Days,
    data$Total_amount_spent,
    method = "spearman",
    exact = FALSE
)
test11
p_values["customer days"] <- test11$p.value
# Result: rho = 0.18, p < 0.001, reject H0.
# A weak but real positive relationship. Spending covers the last 2 years and every
# customer has been with the company for at least 5.9 years
# Long-term customers spend more over the same period.

# ----------------------------
# TEST 12: RECENCY VS SPENDING
# ----------------------------
# Do recent buyers spend more?
# H0: no monotonic relationship between recency and spending.
# H1: there is one.
test12 <- cor.test(
    data$Recency,
    data$Total_amount_spent,
    method = "spearman",
    exact = FALSE
)
test12
p_values["recency"] <- test12$p.value
# Result: rho = 0.02, p = 0.43, do not reject H0.
# How recently a customer last bought is not related to how much they spend.

# ---------------------------
# MULTIPLE TESTING CORRECTION
# ---------------------------

# With many tests at the 0.05 level, some could be significant by chance.
# The Benjamini-Hochberg (BH) correction adjusts the p-values for this.
results <- data.frame(
    test = names(p_values),
    p_value = signif(p_values, 3),
    p_adjusted = signif(p.adjust(p_values, method = "BH"), 3)
)
results$significant <- results$p_adjusted < 0.05
rownames(results) <- NULL
results
# Result: the conclusions do not change after the correction.
# Significant: children (both tests), education, income, discounts (both tests),
# web visits vs purchases (but very weak), customer days.
# Not significant: marital status, marital vs education, complaints (both), recency.


# ######################################
# PART 5. PCA, CLUSTERING AND REGRESSION
# ######################################


# ------------------------------------------
# PART A: PCA (Principal Component Analysis)
# ------------------------------------------
# Goal: see which variables "move together", and get 2 summary axes (PC1 and PC2) that 
# can be used to plot all customers on one graph.

# Step A1. Choose the variables: income, spending on each product, purchases
# per channel and web visits.
pca_vars <- c("Income", mnt_cols, purchase_cols, "NumWebVisitsMonth")

# Step A2. Run the PCA.
# scale. = TRUE scales the variables first, otherwise Income (tens of thousands) would dominate.
pca <- prcomp(data[pca_vars], scale. = TRUE)

# Step A3. How much of the variation does each component explain?
summary(pca)
# Result: PC1 explains 56% of the variation and PC2 another 12%, so together they keep 68% of the information in the 10 variables.

# Step A4. Which variables make up PC1 and PC2?
round(pca$rotation[, 1:2], 2)
# Result:
# - PC1 = "overall buying power". Income, all spending columns and all
#   purchase channels have similar loadings (-0.23 to -0.38), and web visits
#   has the opposite sign (0.27). The sign itself has no meaning (R picks it
#   at random), so here a LOW PC1 score means high income and high spending.
# - PC2 = "online wine buyers". It is highest for web purchases (0.63),
#   web visits (0.44) and wine (0.40), and negative for fish, fruit and
#   sweets (-0.21 to -0.26).
# So most of the differences between customers come down to one thing:
# how much they earn and spend (PC1). The second, much smaller difference is
# whether they buy online and buy wine, or buy more fresh and sweet products.


# ------------------------------------------------------
# PART B: CLUSTERING with k-means
# ------------------------------------------------------
# Goal: split customers into groups with similar behaviour

# Step B1. Choose and scale the variables.
# Customer behaviour: income, spending on each product, purchases per channel
# and web visits. Total_amount_spent is left out because it is the sum of the
# Mnt* columns (it would count spending twice).
cluster_vars <- c("Income", mnt_cols, purchase_cols, "NumWebVisitsMonth")
cluster_data <- data[, cluster_vars]

# k-means groups customers by distance, so all variables are scaled first for the same reason as before.
scaled_cluster_data <- scale(cluster_data)

# Step B2. Choose the number of clusters k with the ELBOW method.
# For k = 1 to 10, store the total within-cluster sum of squares
# set.seed() makes the results the same every run (k-means starts randomly).
# nstart = 25 tries 25 random starts and keeps the best;
# iter.max = 50 gives k-means enough rounds to finish
tot_withinss <- numeric(10)
for (k in 1:10) {
  set.seed(123)
  km_temp <- kmeans(scaled_cluster_data, centers = k, nstart = 25, iter.max = 50)
  tot_withinss[k] <- km_temp$tot.withinss
}
plot(
  1:10,
  tot_withinss,
  type = "b",
  pch = 19,
  xlab = "Number of clusters (k)",
  ylab = "Total within-cluster sum of squares",
  main = "Elbow Method for Choosing k"
)
save_fig("cluster_elbow")
# Result: the biggest drop is from k = 1 to 2 (19,960 -> 11,086), then 2 to 3
# (-> 9,112). After k = 3 the line flattens, so the elbow is at k = 2-3.

# Step B3. Check k with the SILHOUETTE method.
# For each customer, the silhouette compares how close they are to their own
# cluster with the next nearest one (-1 to 1; higher = better separated).
library(cluster)

# dist() computes the distances between all customers once, reused for every k.
dist_matrix <- dist(scaled_cluster_data)
avg_sil_widths <- numeric(10)
for (k in 2:10) {
  set.seed(123)
  km_temp <- kmeans(scaled_cluster_data, centers = k, nstart = 25, iter.max = 50)
  sil_obj <- silhouette(km_temp$cluster, dist_matrix)
  avg_sil_widths[k] <- mean(sil_obj[, 3])
}
plot(
  2:10,
  avg_sil_widths[2:10],
  type = "b",
  pch = 19,
  xlab = "Number of clusters (k)",
  ylab = "Average silhouette width",
  main = "Silhouette Analysis for Choosing k"
)
save_fig("cluster_silhouette")
round(avg_sil_widths[2:10], 3)
# Result: k = 2 has the highest average silhouette (0.43), k = 3 is next (0.35),
# and it keeps falling after that.

# Choice of k:
# k = 2 is best by the silhouette, but it only splits customers into "high" and
# "low" spenders. k = 3 is chosen because it also separates a distinct middle
# group (mid-income families who still spend a lot), which is more useful for
# describing customer profiles, while its silhouette (0.35) is still reasonable
# and it sits at the elbow.

# Step B4. Run the final k-means with k = 3.
chosen_k <- 3
set.seed(123)
final_km <- kmeans(scaled_cluster_data, centers = chosen_k, nstart = 25, iter.max = 50)

# Add each customer's cluster to the data as a factor.
data$Cluster <- factor(final_km$cluster)
table(data$Cluster)
# Result: cluster 1 = 960 customers, cluster 2 = 491, cluster 3 = 546.

# Step B5. Describe each cluster.
# Medians of the ORIGINAL (unscaled) variables, so the numbers are easy to read.
# Children, Age and the purchase shares were NOT used for clustering, but they
# help describe the groups.
cluster_profile <- aggregate(
  cbind(
    Income,
    Total_amount_spent,
    MntWines,
    MntMeatProducts,
    Age,
    Children,
    DealPercent,
    WebPercent,
    CataloguePercent,
    StorePercent
  ) ~ Cluster,
  data = data,
  FUN = median
)
cluster_profile

# Share of customers with children in each cluster (%).
round(prop.table(table(data$Cluster, data$HasChildren), 1) * 100)

# Cluster profiles:
# - Cluster 1 "Budget families" (960 customers, the largest group):
#   lowest income (median 35,081) and spending (54). 89% have children.
#   A third of their purchases are on discount (33%), mostly in store (57%),
#   and they almost never use the catalogue.
# - Cluster 2 "Affluent big spenders" (491 customers):
#   highest income (76,412) and spending (1,290), especially on wine (548)
#   and meat (435). 75% have no children. They rarely use discounts (6%) and
#   use the catalogue the most (30% of purchases).
# - Cluster 3 "Established families" (546 customers):
#   mid-high income (59,987) and spending (652), mostly on wine (434).
#   84% have children, they are a little older (median age 54), use some
#   discounts (15%) and buy online the most (36% of purchases).

# Step B6. Visualise the clusters.
# The same colour for each cluster in every plot.
cluster_colours <- c("darkorange", "steelblue", "forestgreen")

boxplot(
  Total_amount_spent ~ Cluster,
  data = data,
  main = "Total Spending by Customer Cluster",
  xlab = "Customer Cluster",
  ylab = "Total Amount Spent",
  col = cluster_colours
)
save_fig("cluster_spending_boxplot")

plot(
  data$Income,
  data$Total_amount_spent,
  col = cluster_colours[data$Cluster],
  pch = 19,
  xlab = "Income",
  ylab = "Total Amount Spent",
  main = "Customer Clusters: Income vs Spending"
)
legend(
  "topleft",
  legend = paste("Cluster", 1:chosen_k),
  col = cluster_colours,
  pch = 19
)
save_fig("cluster_income_vs_spending")
# Result: cluster 1 (budget families) is clearly separate: low income and low
# spending. Clusters 2 and 3 overlap at mid-to-high incomes - they differ less in
# income and more in HOW they buy (children, catalogue vs web, wine vs meat),
# which a 2-variable plot cannot show.
# Note: 5 of the 10 clustering variables are spending columns, so spending has
# a large weight in the clustering - which is why the clusters follow spending
# level so closely.

# The customers on the two PCA axes from PART A, coloured by cluster.
plot(
  pca$x[, 1],
  pca$x[, 2],
  col = cluster_colours[data$Cluster],
  pch = 19,
  xlab = "PC1 (overall buying power, 56%)",
  ylab = "PC2 (online wine buyers, 12%)",
  main = "Customer Clusters on the
First Two Principal Components"
)
legend(
  "topright",
  legend = paste("Cluster", 1:chosen_k),
  col = cluster_colours,
  pch = 19
)
save_fig("cluster_pca")
# Median PC1 and PC2 score of each cluster.
round(tapply(pca$x[, 1], data$Cluster, median), 2)
round(tapply(pca$x[, 2], data$Cluster, median), 2)
# Result: the clusters separate much better here than in the income vs spending
# plot. Along PC1 (low = high buying power): affluent big spenders (cluster 2,
# median -3.27), then established families (cluster 3, -0.59), then budget
# families (cluster 1, 2.20). Along PC2, established families are the highest
# (1.06), because they buy online and buy wine the most. So clusters 2 and 3,
# which overlap in income, are told apart by HOW they buy.


# ------------------------------------------------------
# PART C: REGRESSION - what explains spending?
# ------------------------------------------------------
# Goal: measure the effect of each customer characteristic on spending,
# while holding the others fixed (e.g. the effect of children at the same income).

# Step C1. Response variable: log(Total_amount_spent).
# Spending is strongly right-skewed. The log makes it more symmetric
data$log_spending <- log(data$Total_amount_spent)
hist(
  data$log_spending,
  main = "Distribution of log(Total Amount Spent)",
  xlab = "log(Total Amount Spent)",
  col = "lightblue"
)
save_fig("hist_log_spending")
# Result: much less skewed than the original spending.

# Step C2. Fit the model.
# Predictors: income, number of children, age, education and marital status.
# factor(..., ordered = FALSE) gives the usual "bachelor vs hs" comparisons
# (hs and Divorced are the reference groups).
# Spending and purchase columns are NOT used as predictors: they are part of
# spending itself.
spending_model <- lm(
  log_spending ~ Income + Children + Age +
    factor(schoolinglevel, ordered = FALSE) + Marital_State,
  data = data
)
summary(spending_model)

# Step C3. Interpretation.
# Effect of each predictor on spending, in %: (exp(b) - 1) * 100
round((exp(coef(spending_model)) - 1) * 100, 2)
# Income per 10,000 (its coefficient is per 1 unit of income):
round((exp(coef(spending_model)["Income"] * 10000) - 1) * 100, 1)

# - Income: significant (p < 0.001). +10,000 in income goes with about
#   78% higher spending.
# - Children: significant (p < 0.001). Each extra child goes with about
#   29% lower spending, AT THE SAME INCOME - so children reduce spending
#   on their own, not only because families earn less.
# - Age: significant (p = 0.006) but small: about +0.45% per year of age.
# - Education: NOT significant once income is in the model (all p > 0.6).
#   So the low spending of high-school customers (test 3 in PART 4)
#   is mostly explained by their lower income.
# - Marital status: NOT significant (all p > 0.4), which agrees with test 4.
# - R-squared = 0.73: the model explains 73% of the variation in log(spending),
#   mostly thanks to income and children.

# Step C4. Check the model assumptions.
par(mfrow = c(2, 2))
plot(spending_model)
save_fig("regression_diagnostics")
par(mfrow = c(1, 1))
# - Normal Q-Q: the points follow the line closely except for a couple of
#   extreme customers, so the residuals are roughly normal.
# - Residuals vs Fitted: the red line curves (an arch) instead of staying flat
#   at 0. The model misses part of the pattern: spending does not rise in a
#   straight line with income (it stays low and then rises steeply, as in the
#   income vs spending scatterplot in PART 3).
# - Scale-Location: the spread of the residuals is a little uneven across the
#   fitted values.
# - Residuals vs Leverage: no customer has a large influence on the model.
# Limitation for the report: the model fits well overall (R-squared 0.73),
# but the curved income effect is not fully captured by a straight-line model.
