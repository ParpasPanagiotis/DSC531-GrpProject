# MAS531 - Food Project

# 1. IMPORT, CHECKS AND CLEANING --------------------------------------------

# Clear the workspace and release unused memory.
rm(list = ls())
gc()

# Find the project folder, so the script works whether R was started in the
# project folder (as in VS Code) or in scripts/ (as in RStudio).
project_dir <- if (file.exists("data/Food_raw.csv")) "." else ".."
data_file <- file.path(project_dir, "data", "Food_raw.csv")
fig_dir <- file.path(project_dir, "figures")

# Import the original CSV file.
data <- read.csv(data_file)

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
# - All other logic checks (4-14) pass.

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


# 4. NUMERICAL VARIABLES AND DESCRIPTIVE STATISTICS ------------------------

# Exclude Kidhome, Teenhome and Complain from the continuous-variable plots,
# and the two categorical factors created above.
# These excluded variables are described later using frequency tables.
dataN <- data[,
  !(names(data) %in%
    c("Kidhome", "Teenhome", "Complain", "Marital_State", "schoolinglevel"))
]

# The e1071 package provides skewness() and kurtosis().
# Run install.packages("e1071") once if the package is not installed.
library(e1071)

colnames <- names(dataN)
colours <- rep(c("red", "blue", "darkgreen", "gold", "purple"), 3)

# Loop over each numeric variable to calculate statistics and display plots.
i <- 0
for (v in colnames) {
  i <- i + 1
  x <- dataN[[v]]

  # Descriptive statistics: range, centre, variability and shape.
  min_value <- min(x)
  max_value <- max(x)
  m <- mean(x)
  med <- median(x)
  variance <- var(x)
  standard_dev <- sd(x)
  iqr <- IQR(x)
  skew <- skewness(x)
  kurt <- kurtosis(x) # Excess kurtosis: a normal distribution has value 0.

  # Statistical mode = most frequent observed value (not always informative
  # for continuous variables; in case of ties, the first mode is shown).
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

  # Boxplot: useful for spread and potential outliers.
  boxplot(x, main = paste0("Boxplot for ", v), col = colours[i], ylab = v)
  save_fig(paste0("box_", v))

  # Histogram: shows the distribution of the observed values.
  hist(
    x,
    breaks = 20,
    xlab = v,
    main = paste0("Histogram for ", v),
    col = colours[i]
  )
  save_fig(paste0("hist_", v))

  # Compare the density of standardized data with a standard normal curve.
  dens <- density(as.numeric(scale(x)))
  plot(
    dens,
    main = paste0("Standardized data vs normal: ", v),
    col = "red",
    xlab = "Standardized values"
  )
  lines(dens$x, dnorm(dens$x), col = "blue")
  legend(
    "topright",
    legend = c("Standardized data", "Standard normal"),
    col = c("red", "blue"),
    lty = 1,
    cex = 0.8
  )

  # Normal Q-Q plot: large deviations from the line suggest non-normality.
  qqnorm(scale(x), main = paste0("Normal Q-Q plot for ", v), cex = 0.8)
  qqline(scale(x), col = "red", lwd = 2)
  # The blue line represents y=x after standardization (visual reference).
  abline(a = 0, b = 1, col = "blue", lwd = 2)
  save_fig(paste0("qq_", v))
}
# Observations:
# All values are in a sensible range: no negative amounts, Age 24-80,
# Income 2,447-113,734. (An income of 2,447 is unusually low but possible, so it is kept.)
# For the spending columns the mean is far above the median
# (e.g. MntFruits: median 8, mean 26.5), so they are strongly right-skewed.

# OUTLIER CHECK
# Count values outside the 1.5 x IQR rule (the points beyond the boxplot whiskers).
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

# DESCRIPTIVE ANALYSIS: CATEGORICAL AND DISCRETE VARIABLES
# Number of Small Children per Household. This analysis shows how many customers have 0, 1, or more small children in their households.
table(data$Kidhome)
round(prop.table(table(data$Kidhome)) * 100, 2)
barplot(
  table(data$Kidhome),
  main = "Number of Small Children per Household",
  xlab = "Number of Small Children",
  ylab = "Number of Customers",
  col = "lightblue"
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
  col = "lightblue"
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
  col = "lightblue"
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

# NORMALITY TESTING
for (v in names(dataN)) {
  cat("\nVariable:", v, "\n")
  print(shapiro.test(dataN[[v]]))
}
# Observations:
# Shapiro-Wilk tests were performed on all 15 numerical variables.
# All p-values were below 0.05, providing evidence against normality.
# Note: with about 2000 customers, Shapiro-Wilk flags even tiny departures from normality,
# so we also judge by the Q-Q plots and skewness. These agree for the spending variables:
# MntFruits (W = 0.69) and MntMeatProducts (W = 0.74) are strongly right-skewed.
# Income (W = 0.985) and Age (W = 0.983) are close to normal in their Q-Q plots.
# Overall we use non-parametric methods (Spearman, Wilcoxon, Kruskal-Wallis).

# SPEARMAN CORRELATION ANALYSIS
cor_matrix <- cor(dataN, method = "spearman")
heatmap(
  cor_matrix,
  main = "Spearman Correlation Heatmap",
  col = colorRampPalette(c("blue", "white", "red"))(100),
  symm = TRUE,
  Rowv = NA,
  Colv = NA,
  scale = "none",
  margins = c(10, 10)
)
save_fig("heatmap_spearman")
# Examine correlations specifically with Total Spending.
round(cor_matrix["Total_amount_spent", ], 2)

# Observations:
# Income is strongly positively correlated with Total Spending (rho = 0.87).
# Wine and meat spending each have rho = 0.94 with Total Spending, but this is
# largely built in: Total Spending is the sum of the Mnt* columns, and wine and meat
# are its two largest parts. It should not be read as a separate finding.
# Website visits show a moderate negative correlation (rho = -0.47).
# Recency shows almost no correlation with Total Spending (rho = 0.02).

# CREATE CUSTOMER GROUPS FOR FURTHER ANALYSIS
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


# TOTAL SPENDING BY NUMBER OF CHILDREN
# The violin plot shows the concentration of spending values, while the boxplot shows the median and quartiles.
library(ggplot2)

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
  width = 8,
  height = 6
)
# Display median spending for each group.
tapply(data$Total_amount_spent, data$Children, median)

# Observations:
# Median spending decreases as the number of children increases,
# from 1117 (0 children) to 62 (3 children).
# The 3-children group contains only 45 customers, so interpret cautiously.

# TOTAL SPENDING BY EDUCATION LEVEL
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
# The HS group has only 49 customers, so interpret cautiously.
# Spending varies considerably across groups, with some outliers.

# TOTAL SPENDING BY MARITAL STATUS
tapply(data$Total_amount_spent, data$Marital_State, median)
boxplot(
  Total_amount_spent ~ Marital_State,
  data = data,
  main = "Total Spending by Marital Status",
  xlab = "Marital Status",
  ylab = "Total Amount Spent",
  col = "lightblue"
)
save_fig("box_spending_marital")
# Observations:
# Widowed customers have the highest median spending (597); the other groups are similar (303-350).
# The widow group is small (69 customers) and likely older, so this may reflect age rather than marital status.

# INCOME VS TOTAL SPENDING
plot(
  data$Income,
  data$Total_amount_spent,
  main = "Income vs Total Spending",
  xlab = "Income",
  ylab = "Total Amount Spent",
  col = rgb(0, 0, 1, 0.3),
  pch = 16
)
# Smooth curve (lowess) to show the trend without assuming a straight line.
lines(lowess(data$Income, data$Total_amount_spent), col = "red", lwd = 2)
save_fig("scatter_income_spending")
# Observations:
# Spending rises clearly with income (Spearman rho = 0.87).
# The relationship is curved: spending stays low up to about 40,000 and then rises steeply.

# TOTAL PURCHASES ACROSS CHANNELS
# Calculate the total number of purchases through the Web, Catalogue and Store channels.
# (Customers with zero purchases were removed during cleaning, so there is no division by zero.)
data$TotalPurchases <- data$NumWebPurchases +
  data$NumCatalogPurchases +
  data$NumStorePurchases

# Share of each customer's purchases made through each channel (%).
data$WebPercent <- data$NumWebPurchases / data$TotalPurchases * 100
data$CataloguePercent <- data$NumCatalogPurchases / data$TotalPurchases * 100
data$StorePercent <- data$NumStorePurchases / data$TotalPurchases * 100

# Calculate the average customer-level purchase percentage for each channel.
channel_percentages <- c(
  Web = mean(data$WebPercent),
  Catalogue = mean(data$CataloguePercent),
  Store = mean(data$StorePercent)
)
round(channel_percentages, 2)
barplot(
  channel_percentages,
  main = "Average Percentage of Purchases by Channel",
  xlab = "Purchasing Channel",
  ylab = "Average Percentage (%)",
  col = "lightblue",
  ylim = c(0, 60)
)
save_fig("bar_channels")

# Observations:
# Store has the highest average purchase share (50.48%),
# followed by Web (33.15%) and Catalogue (16.36%).
# These percentages are averages across customers.

# PURCHASING CHANNEL PREFERENCES BY AGE GROUP
channel_age_percent <- rbind(
  Web = tapply(data$WebPercent, data$AgeGroup, mean),
  Catalogue = tapply(data$CataloguePercent, data$AgeGroup, mean),
  Store = tapply(data$StorePercent, data$AgeGroup, mean)
)
round(channel_age_percent, 2)
barplot(
  channel_age_percent,
  beside = TRUE,
  main = "Average Purchasing Channel Percentages by Age Group",
  xlab = "Age Group",
  ylab = "Average Percentage of Purchases (%)",
  col = c("lightblue", "lightgreen", "lightpink"),
  legend.text = TRUE,
  ylim = c(0, 70)
)
save_fig("bar_channels_age")

# Observations:
# Store has the highest average purchase share in every age group.
# Customers aged 30-44 have the highest Store percentage (52.93%).
# Web purchases have the highest share among customers aged 45-59 (34.00%).
# Under-30 customers have the highest Catalogue percentage (25.73%), but this group contains only 37 customers.

# TOTAL PURCHASES BY INCOME GROUP
purchases_income <- tapply(
  data$TotalPurchases,
  data$IncomeGroup,
  mean
)
round(purchases_income, 2)
barplot(
  purchases_income,
  main = "Average Total Purchases by Income Group",
  xlab = "Income Group",
  ylab = "Average Number of Purchases",
  col = "lightblue"
)
save_fig("bar_purchases_income")

# Observations:
# Average total purchases increase across income groups,
# from 5.54 in Q1 (lowest income) to 19.44 in Q4 (highest income). This suggests that higher-income customers tend to make more purchases.
