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


# ------------------------------------------------------
# PART A: PCA (Principal Component Analysis)
# ------------------------------------------------------
# Goal: see which variables "move together", and get 2 summary axes
# (PC1 and PC2) that can be used to plot all customers on one graph.

# Step A1. Choose the variables: income, spending on each product, purchases
# per channel and web visits (the same 10 variables used for clustering in PART B).
pca_vars <- c("Income", mnt_cols, purchase_cols, "NumWebVisitsMonth")

# Step A2. Run the PCA.
# scale. = TRUE scales the variables first, for the same reason as in
# clustering: otherwise Income (tens of thousands) would dominate.
pca <- prcomp(data[pca_vars], scale. = TRUE)

# Step A3. How much of the variation does each component explain?
summary(pca)
# Result: PC1 explains 56% of the variation and PC2 another 12%,
# so together they keep 68% of the information in the 10 variables.

# Step A4. Which variables make up PC1 and PC2?
# The "loadings": a large number (positive or negative) means the variable
# has a big part in that component.
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
# The plot of the customers on PC1 and PC2, coloured by cluster, is in Step B6.


# ------------------------------------------------------
# PART B: CLUSTERING (customer segments) with k-means
# ------------------------------------------------------
# Goal: split customers into groups with similar behaviour, then describe
# each group in plain words.

# Step B1. Choose and scale the variables.
# Customer behaviour: income, spending on each product, purchases per channel
# and web visits. Total_amount_spent is left out because it is the sum of the
# Mnt* columns (it would count spending twice).
cluster_vars <- c("Income", mnt_cols, purchase_cols, "NumWebVisitsMonth")
cluster_data <- data[, cluster_vars]

# k-means groups customers by distance, so all variables are scaled first.
# Otherwise Income (tens of thousands) would dominate the purchase counts (0-30).
scaled_cluster_data <- scale(cluster_data)

# Step B2. Choose the number of clusters k with the ELBOW method.
# For k = 1 to 10, store the total within-cluster sum of squares
# (how spread out the customers are inside their clusters; lower = tighter).
# set.seed() makes the results the same every run (k-means starts randomly).
# nstart = 25 tries 25 random starts and keeps the best;
# iter.max = 50 gives k-means enough rounds to finish (the default 10 is too few).
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
# Spending is strongly right-skewed. The log makes it more symmetric, which
# suits a linear model better. (The minimum spending is 6, so there are no zeros.)
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
# With log(spending), a coefficient b means spending changes by about
# (exp(b) - 1) * 100 % for +1 of that predictor, with the others held fixed.
# - Income: significant (p < 0.001). +10,000 in income goes with about
#   78% higher spending.
# - Children: significant (p < 0.001). Each extra child goes with about
#   29% lower spending, AT THE SAME INCOME - so children reduce spending
#   on their own, not only because families earn less.
# - Age: significant (p = 0.006) but small: about +0.45% per year of age.
# - Education: NOT significant once income is in the model (all p > 0.6).
#   So the low spending of high-school customers (test 3 in 04_tests.R)
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
#   income vs spending scatterplot in 03_bivariate.R).
# - Scale-Location: the spread of the residuals is a little uneven across the
#   fitted values.
# - Residuals vs Leverage: no customer has a large influence on the model.
# Limitation for the report: the model fits well overall (R-squared 0.73),
# but the curved income effect is not fully captured by a straight-line model.
