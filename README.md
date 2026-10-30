# Food Customers: Exploratory Data Analysis

Group project for **MAS531**, MSc Data Science.

An exploratory analysis of the marketing data of a food company: who its customers are, how they spend, and which customer characteristics are related to spending.

## Data

`data/Food_raw.csv`: 2,205 customers and 27 variables, including:

- **Profile:** income, age, children and teenagers at home, marital status, education
- **Spending (last 2 years):** wine, fruit, meat, fish and sweet products, and the total
- **Purchases:** through the website, the catalogue and in stores, and with a discount
- **Relationship:** days as a customer, days since the last purchase, complaints

Marital status and education come as indicator (0/1) columns. The cleaning script combines each group into one categorical variable.

## Project structure

```
├── data/
│   ├── Food_raw.csv        original dataset
│   └── food_clean.rds      cleaned dataset (created by 01_cleaning.R)
├── figures/                plots saved by the scripts (PNG)
└── scripts/
    ├── 00_setup.R          shared paths, helper function and column lists
    ├── 01_cleaning.R       checks, cleaning and new variables
    ├── 02_descriptives.R   univariate analysis, outliers, normality
    ├── 03_bivariate.R      correlations and relationships between variables
    ├── 04_tests.R          hypothesis tests
    ├── 05_advanced.R       PCA, clustering and regression
    └── main.R              runs every script in order
```

## How to run

1. Install R (version 4.x) and the required packages once:

   ```r
   install.packages(c("e1071", "nortest", "ggplot2"))
   ```

   `cluster` (used in `05_advanced.R`) comes with R.

2. Run the whole analysis from a fresh R session:

   ```r
   source("scripts/main.R")
   ```

   The scripts work whether R is started in the project folder or in `scripts/`.

Each script can also be run on its own once `01_cleaning.R` has been run at least once, because the later scripts load `data/food_clean.rds`.

## Method

1. **Cleaning:** 203 duplicate rows and 5 rows with inconsistent purchase counts were removed (2,205 → 1,997 customers). Thirteen logic checks were run on the data, and new variables were created: number of children, age and income groups, total purchases, and the share of purchases made through each channel and with a discount.
2. **Descriptive analysis:** summary statistics, boxplots, histograms and bar charts. Normality was checked with Q-Q plots, the Kolmogorov–Smirnov test and the Lilliefors test.
3. **Bivariate analysis:** Spearman correlations, group comparisons and scatterplots.
4. **Hypothesis testing:** none of the numerical variables is normally distributed, so non-parametric tests are used: Wilcoxon, Kruskal–Wallis with pairwise comparisons, Spearman correlation and chi-square.

## Main findings

- **Income** is the strongest driver of spending (Spearman ρ = 0.87).
- **Customers with children spend much less** (median 182 vs 1,117) and **rely far more on discounts**.
- **Lower-income customers are more price-sensitive:** their share of discounted purchases is much higher (ρ = −0.73).
- **High-school-educated customers spend much less** than all other education levels.
- **Marital status has no significant effect** on spending. The higher spending of widowed customers is explained by their older age.
- **Website visits do not turn into website purchases.**
- **Stores are the main purchase channel** (about half of all purchases).
