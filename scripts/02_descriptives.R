# MAS531 - Food Project
# 02_descriptives.R: univariate analysis, outliers and normality.
# Input: data/food_clean.rds (run 01_cleaning.R first).

rm(list = ls())

# Find the project folder, so the script works whether R was started in the
# project folder (as in VS Code) or in scripts/ (as in RStudio).
project_dir <- if (file.exists("data/Food_raw.csv")) "." else ".."
source(file.path(project_dir, "scripts", "00_setup.R"))

# Load the cleaned dataset.
data <- readRDS(clean_file)


# NUMERICAL VARIABLES AND DESCRIPTIVE STATISTICS ---------------------------

# The 15 numerical variables (listed in 00_setup.R). Kidhome, Teenhome and
# Complain are described later using frequency tables.
dataN <- data[num_cols]

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
# A normality test only makes sense when the histogram looks roughly bell-shaped.
# From the histograms above, 13 of the 15 variables are clearly not normal, so they are not tested:
# - Strongly right-skewed (long tail to the right): the five Mnt* columns,
#   Total_amount_spent, NumDealsPurchases, NumWebPurchases, NumCatalogPurchases,
#   NumStorePurchases.
# - Flat (every value about equally common, uniform): Recency, Customer_Days.
# - Piled up on the right with a few extreme high values: NumWebVisitsMonth.
# Only Income and Age are roughly symmetric with one main hump, so only they are tested.

# The nortest package provides lillie.test().
# Run install.packages("nortest") once if the package is not installed.
library(nortest)

for (v in c("Income", "Age")) {
  x <- data[[v]]

  # Normal Q-Q plot: points far from the line suggest non-normality.
  qqnorm(x, main = paste0("Normal Q-Q plot for ", v), cex = 0.8)
  qqline(x, col = "red", lwd = 2)
  save_fig(paste0("qq_", v))

  # Kolmogorov-Smirnov test against a normal curve with the same mean and SD.
  # H0: the data are normal; H1: they are not.
  # suppressWarnings() hides the "ties" warning: Age is in whole years,
  # so many customers share the same value.
  cat("\nVariable:", v, "\n")
  print(suppressWarnings(ks.test(x, "pnorm", mean = mean(x), sd = sd(x))))

  # Lilliefors test: the K-S test corrected for estimating the mean and SD
  # from the same data (the plain K-S p-value is too large in that case).
  print(lillie.test(x))
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
