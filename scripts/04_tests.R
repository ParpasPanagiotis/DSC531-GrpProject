# MAS531 - Food Project
# 04_tests.R: hypothesis testing.

rm(list = ls())

project_dir <- if (file.exists("data/Food_raw.csv")) "." else ".."
source(file.path(project_dir, "scripts", "00_setup.R"))

# Load the cleaned dataset.
data <- readRDS(clean_file)

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

# ------------------------------------------------------
# TEST 1: SPENDING, CUSTOMERS WITH VS WITHOUT CHILDREN
# ------------------------------------------------------
# Do customers with children spend less?
# H0: spending is the same for customers with and without children.
# H1: spending differs between the two groups.
tapply(data$Total_amount_spent, data$HasChildren, median)
test1 <- wilcox.test(Total_amount_spent ~ HasChildren, data = data)
test1
p_values["children (yes/no)"] <- test1$p.value
# Result: p < 0.001, reject H0.
# Customers without children spend far more: median 1117 vs 182 for customers with children.

# ------------------------------------------------------
# TEST 2: SPENDING BY NUMBER OF CHILDREN (0-3)
# ------------------------------------------------------
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

# ------------------------------------------------------
# TEST 3: SPENDING BY EDUCATION LEVEL
# ------------------------------------------------------
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

# ------------------------------------------------------
# TEST 4: SPENDING BY MARITAL STATUS
# ------------------------------------------------------
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

# ------------------------------------------------------
# TEST 5: INCOME VS SPENDING
# ------------------------------------------------------
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

# ------------------------------------------------------
# TEST 6: MARITAL STATUS VS EDUCATION
# ------------------------------------------------------
# H0: marital status and education are independent.
# H1: they are related.
marital_education <- table(data$Marital_State, data$schoolinglevel)
marital_education
test6 <- chisq.test(marital_education)
test6
p_values["marital vs education"] <- test6$p.value

# Result: p = 0.26, do not reject H0.
# There is no evidence that marital status and education are related.

# ------------------------------------------------------
# TEST 7: COMPLAINTS
# ------------------------------------------------------
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

# ------------------------------------------------------
# TEST 8: DISCOUNT SHARE BY NUMBER OF CHILDREN
# ------------------------------------------------------
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

# ------------------------------------------------------
# TEST 9: DISCOUNT SHARE VS INCOME
# ------------------------------------------------------
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

# ------------------------------------------------------
# TEST 10: WEB VISITS VS WEB PURCHASES
# ------------------------------------------------------
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

# ------------------------------------------------------
# TEST 11: LENGTH OF RELATIONSHIP VS SPENDING
# ------------------------------------------------------
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

# ------------------------------------------------------
# TEST 12: RECENCY VS SPENDING
# ------------------------------------------------------
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

# ------------------------------------------------------
# MULTIPLE TESTING CORRECTION
# ------------------------------------------------------

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
