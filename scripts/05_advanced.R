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


# ==========================================================================
# GUIDE - replace each "TODO" with your own code and comments.
# Suggested order: PART A (PCA) -> PART B (clustering) -> PART C (regression).
# If time is short, do PART B first: customer segments are the most useful
# result for the report ("understand the profiles of its customers").
# ==========================================================================


# ------------------------------------------------------
# PART A: PCA (Principal Component Analysis)
# ------------------------------------------------------
# Goal: see which variables "move together", and get 2 summary axes that can
# be used to plot all customers (and later the clusters) on one graph.

# Step A1. Choose the variables.
# - Use customer BEHAVIOUR variables, e.g. the 5 Mnt* columns, the 4 Num*
#   purchase columns, NumWebVisitsMonth, and Income.
# - Do NOT add Total_amount_spent together with the Mnt* columns
#   (it is their sum, so it repeats the same information).
# - Hint: build a vector of column names (you already have mnt_cols and
#   purchase_cols in 00_setup.R) and take data[those_columns].
# TODO

# Step A2. Run the PCA on SCALED variables.
# - Why scale: Income is in tens of thousands and purchases are 0-30. Without
#   scaling, Income would dominate just because its numbers are bigger.
# - Function: prcomp(your_data, scale. = TRUE)   (note the dot in "scale.")
# TODO

# Step A3. How much information does each component keep?
# - Function: summary(your_pca). Look at the row "Cumulative Proportion".
# - Question to answer in a comment: how much of the variation do PC1 and PC2
#   explain together? (e.g. "PC1 and PC2 explain 65% of the variation")
# - Optional plot: screeplot(your_pca) or plot(your_pca)
# TODO

# Step A4. What does each component mean?
# - Look at your_pca$rotation (the "loadings"): one column per component.
# - Large numbers (positive or negative) = variables that matter for that component.
# - Question: PC1 probably has all spending variables with the same sign.
#   What would you call it? (e.g. "overall spending level")
#   What does PC2 separate? (look at which variables are + and which are -)
# TODO

# Step A5. Plot the customers on PC1 and PC2.
# - The customer coordinates are in your_pca$x (columns PC1, PC2, ...).
# - Simple version: plot(your_pca$x[, 1], your_pca$x[, 2], ...)
# - Alternative: biplot(your_pca) shows customers AND variable arrows
#   (it can look crowded with 2000 customers - try it and decide).
# - Save the plot you want in the report with save_fig("...").
# TODO


# ------------------------------------------------------
# PART B: CLUSTERING (customer segments) with k-means
# ------------------------------------------------------
# Goal: split customers into a few groups with similar behaviour, then
# describe each group in plain words (e.g. "affluent wine buyers",
# "budget families", "deal seekers").

# Step B1. Choose and scale the variables.
# - Use the same kind of behaviour variables as in Step A1 (you can reuse them).
# - k-means works with distances, so SCALE them first: scale(your_data)
# TODO

# Step B2. Choose the number of clusters k with the ELBOW method.
# - Run kmeans() for k = 1 to 10 and store tot.withinss for each k
#   (total within-cluster sum of squares = how spread out the clusters are).
# - Hint: a for loop over k, like the loops in 02_descriptives.R.
# - Always call set.seed(...) before kmeans(), so results are the same every run
#   (k-means starts from random points).
# - Use kmeans(your_scaled_data, centers = k, nstart = 25)
#   nstart = 25 tries 25 random starts and keeps the best one.
# - Plot tot.withinss against k (type = "b" gives points + lines).
# - Look for the "elbow": the k after which the line stops dropping a lot.
# TODO

# Step B3. Check k with the SILHOUETTE method (optional but recommended).
# - Package: cluster (already installed with R). library(cluster)
# - For each k from 2 to 10:
#     silhouette(km$cluster, dist(your_scaled_data))
#   then take the average silhouette width: mean(sil[, 3])
# - Higher average = better separated clusters. Pick the k with the highest value,
#   or the one that agrees with the elbow.
# - Note: dist() on ~2000 customers takes a few seconds - compute it once
#   before the loop and reuse it.
# TODO

# Step B4. Run the final k-means with your chosen k.
# - set.seed(...) again, then kmeans(..., centers = your_k, nstart = 25)
# - Add the result to the data as a factor:
#     data$Cluster <- factor(your_km$cluster)
# - table(data$Cluster): how many customers in each cluster?
# TODO

# Step B5. Describe each cluster (the most important step!).
# - Use the ORIGINAL (unscaled) variables so the numbers are easy to read.
# - Hint: tapply(data$Income, data$Cluster, median) for one variable,
#   or aggregate(cbind(Income, Total_amount_spent, ...) ~ Cluster, data, median)
#   for several at once.
# - Also look at variables you did NOT use for clustering: Children, Age,
#   schoolinglevel, DealPercent, WebPercent, StorePercent ...
#   e.g. table(data$Cluster, data$HasChildren)
# - Write a short profile for each cluster in a comment, and give it a name.
# TODO

# Step B6. Visualise the clusters.
# - Idea 1: the PCA plot from Step A5, coloured by cluster:
#     col = data$Cluster   (inside your plot() call)
# - Idea 2: boxplot(Total_amount_spent ~ Cluster, data = data)
# - Save the best one with save_fig("...").
# TODO


# ------------------------------------------------------
# PART C: REGRESSION - what explains spending?
# ------------------------------------------------------
# Goal: measure the effect of each customer characteristic on spending,
# while holding the others fixed (e.g. the effect of children at the same income).

# Step C1. Response variable: log(Total_amount_spent).
# - Why the log: spending is strongly right-skewed. The log makes it more
#   symmetric, which suits a linear model better.
# - Quick check: hist(log(data$Total_amount_spent)) - does it look less skewed?
# TODO

# Step C2. Fit the model with lm().
# - Predictors from the plan: Income, Children, Age, education, marital status.
# - Use factor(schoolinglevel, ordered = FALSE) inside the formula, otherwise
#   the output shows .L / .Q / .C terms instead of "bachelor vs hs" etc.
# - Do NOT use the Mnt* columns or purchase counts as predictors: they are
#   part of spending itself, so the model would just be explaining spending
#   with spending.
# - Then look at summary(your_model).
# TODO

# Step C3. Read the output.
# - Estimate column: the effect of each predictor.
#   With log(spending), a coefficient b means roughly a (100 * b)% change in
#   spending for +1 of that predictor (this is only accurate for small b).
#   For Income, +1 dollar is tiny - think about +10,000 instead.
# - Pr(>|t|) column: is each effect significant?
# - Multiple R-squared: how much of the variation in log(spending) the model explains.
# - Question: after including income, do children and education still matter?
#   Does marital status matter? (compare with the tests in 04_tests.R)
# TODO

# Step C4. Check the model assumptions with residual plots.
# - par(mfrow = c(2, 2)) then plot(your_model) gives 4 diagnostic plots.
#   Remember par(mfrow = c(1, 1)) afterwards.
# - Residuals vs Fitted: points should be scattered randomly around 0 (no curve).
# - Normal Q-Q: points close to the line = residuals roughly normal.
# - Write in a comment what you see, and any problems (that is fine - just
#   mention them as a limitation in the report).
# TODO
