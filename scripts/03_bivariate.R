# MAS531 - Food Project
# 03_bivariate.R: correlations and relationships between variables.
# Input: data/food_clean.rds (run 01_cleaning.R first).

rm(list = ls())

# Find the project folder, so the script works whether R was started in the
# project folder (as in VS Code) or in scripts/ (as in RStudio).
project_dir <- if (file.exists("data/Food_raw.csv")) "." else ".."
source(file.path(project_dir, "scripts", "00_setup.R"))

# Load the cleaned dataset.
data <- readRDS(clean_file)

# The 15 numerical variables (listed in 00_setup.R).
dataN <- data[num_cols]

library(ggplot2)


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

# TOTAL SPENDING BY NUMBER OF CHILDREN
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
# TotalPurchases and the channel percentages are created in 01_cleaning.R.

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
  col = "#043f53",
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
