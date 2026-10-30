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
bar_pos <- barplot(
  channel_age_percent,
  beside = TRUE,
  main = "Average Purchasing Channel Percentages by Age Group",
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

# DISCOUNT PURCHASES BY NUMBER OF CHILDREN AND BY INCOME
# Are families and lower-income customers more price-sensitive?
# DealPercent (created in 01_cleaning.R) is the share of each customer's purchases
# made with a discount. A share is fairer than a count, because customers who buy
# more also tend to have more discount purchases.
tapply(data$NumDealsPurchases, data$Children, median)
round(tapply(data$DealPercent, data$Children, mean), 2)
boxplot(
  DealPercent ~ Children,
  data = data,
  main = "Share of Purchases Made with a Discount, by Number of Children",
  xlab = "Number of Children",
  ylab = "Discount Purchases (% of all purchases)",
  col = "lightblue"
)
save_fig("box_deals_children")

round(tapply(data$DealPercent, data$IncomeGroup, mean), 2)
boxplot(
  DealPercent ~ IncomeGroup,
  data = data,
  main = "Share of Purchases Made with a Discount, by Income Group",
  xlab = "Income Group",
  ylab = "Discount Purchases (% of all purchases)",
  col = "lightblue"
)
save_fig("box_deals_income")

# Spearman correlations of the discount share with children and income.
round(cor(data$DealPercent, data$Children, method = "spearman"), 2)
round(cor(data$DealPercent, data$Income, method = "spearman"), 2)

# Observations:
# Families rely much more on discounts. The average discount share rises from 8.8%
# (no children) to 24.9% (1 child) and about 43% (2 or 3 children);
# the median number of discount purchases goes from 1 to 3.
# The discount share also falls steadily with income: 36.4% in Q1, 32.6% in Q2,
# 19.7% in Q3 and only 7.6% in Q4 (Spearman rho = -0.73).
# So yes: families and lower-income customers are clearly more price-sensitive.
# (Children and income are related, so part of these two effects may overlap.)

# WEB VISITS VS WEB PURCHASES
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
# jitter() moves each point slightly at random, so customers with the same
# whole-number values do not hide behind each other.
lines(
  lowess(data$NumWebVisitsMonth, data$NumWebPurchases),
  col = "red",
  lwd = 2
)
save_fig("scatter_webvisits_webpurchases")

round(cor(data$NumWebVisitsMonth, data$NumWebPurchases, method = "spearman"), 2)

# Who visits the website most often? Compare customers by number of visits.
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
# Web visits do not turn into web purchases: the correlation is almost zero
# (rho = -0.09), and customers make about 4 web purchases on average whatever
# their number of visits.
# The customers who visit most often are the lower-income, lower-spending ones:
# median income falls from 77,382 (0-2 visits) to 31,814 (9+ visits), and median
# spending from 1,112 to 118. They may be browsing for offers rather than buying.
# Note: visits cover the last month only, while purchases cover a longer period.

# RECENCY AND LENGTH OF CUSTOMER RELATIONSHIP VS SPENDING
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
# Recency has no relationship with spending (rho = 0.02): how long ago a customer
# last bought tells us nothing about how much they spend.
# Length of relationship has a weak positive relationship (rho = 0.18), but it is
# clear in the medians: from 150.5 for the newest quarter of customers to 523 for
# the longest-standing quarter. So long-term customers are more valuable.
# Note: all customers have been with the company for 5.9-7.8 years, so this compares
# long-term customers with very long-term ones, not new with old.
