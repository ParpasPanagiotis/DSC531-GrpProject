# MAS531 Group Project "Fo" (Food): Work Plan

**Team:** Davide, Fanourios Loucas, Olga, Panagiotis
**Dataset:** `Food_new.csv` (2205 customers, 27 variables)

## What we have to hand in

- **A full report** explaining the analysis and our conclusions.
- **An R script** with comments explaining each part of the code. This is required.
- **A presentation** on 27 Nov or 1 Dec, depending on the order we're given.

| Milestone | Date |
|---|---|
| Report + R file submission | **Thu 5 Nov 2026** (20% penalty per day late) |
| Our internal target for submission | **Wed 4 Nov 2026** |
| Presentation | 27 Nov or 1 Dec |

---

## 1. Quick look at the data (already checked)

- **No missing values.** There are 2205 rows and 27 columns, as described in the email.
- **203 rows are exact duplicates.** We need to decide whether to remove them, and explain why in the report.
- **The dummy columns are consistent.** Every customer has exactly one marital status and exactly one education level.
- **`Total_amount_spent` is exactly the sum of the 5 `Mnt*` columns.** We should not put it in the same model or test as those 5 columns, because it is built from them.
- **Some rows look odd:**
  - 4 customers have 0 web, catalogue and store purchases.
  - 3 customers have more discount purchases than total purchases.
- **Some groups are very small:**
  - `Complain` = 1 for only 20 customers.
  - `Widow` covers 76 customers.
  - `Up_to_high_school` covers 54 customers.
  - Tests on these groups will have low power, so we should mention this as a limitation.
- **Values to check by eye:** income ranges from 1,730 to 113,734, and age from 24 to 80.

---

## 2. Timeline

| Phase | What                               | Dates                           |
| ----- | ---------------------------------- | ------------------------------- |
| 0     | Setup                              | Sun 18 Oct                      |
| 1     | Data cleaning and preparation      | 18 – 20 Oct                     |
| 2     | Univariate analysis                | 20 – 23 Oct                     |
| 3     | Bivariate and correlation analysis | 22 – 26 Oct                     |
| 4     | Hypothesis testing                 | 24 – 28 Oct                     |
| 5     | Customer profiles / extra analysis | 26 – 30 Oct                     |
| 6     | Report writing                     | 27 Oct – 2 Nov                  |
| 7     | Final checks and submission        | 2 – 4 Nov                       |
| 8     | Presentation                       | After submission, before 27 Nov |

Phases overlap on purpose: once the cleaned dataset is ready, people can work in parallel.

---

## 3. Phases in detail

### Phase 0: Setup (everyone)
- Create a shared folder (Google Drive / OneDrive) or GitHub repo with one R project.
- Agree on a file structure:
  - `01_cleaning.R`
  - `02_descriptives.R`
  - `03_bivariate.R`
  - `04_tests.R`
  - `05_advanced.R`
  - `main.R` (runs them all at the end)
- Agree on a coding style:
  - `tidyverse` + `ggplot2`.
  - One shared plot theme and colour palette.
  - A comment above every block of code (comments are required).
- Pick a report template (R Markdown or Word) and a slide template.

### Phase 1: Data cleaning and preparation
- Load the data and check that the column types are correct.
- Duplicates: count them, decide whether to remove them, and record the reasoning.
- Check the logical rules:
  - Total spent equals the sum of the `Mnt*` columns.
  - Discount purchases are not more than total purchases.
  - Customers with 0 purchases but money spent.
- Look for outliers with boxplots and z-scores or IQR on Income, Age, the `Mnt*` columns and the `Num*` columns. Decide whether to keep, cap or remove each one, and justify it.
- **Group the dummy columns (the email hints at this):**
  - Turn the 5 marital columns into one factor, `Marital`.
  - Optionally simplify it further to `Partner` (Married or Living together) vs `Alone`.
  - Turn the 4 education columns into one ordered factor, `Education`.
- **Create new variables:**
  - `Children` = Kidhome + Teenhome, plus `HasChildren` (yes/no)
  - `TotalPurchases` = Web + Catalog + Store
  - `Customer_Years` = Customer_Days / 365
  - Age groups and income bands
  - Share of spending on each category (for example wine as a % of the total)
  - Average spend per purchase = Total_amount_spent / TotalPurchases
  - Deal ratio = NumDealsPurchases / TotalPurchases
- Convert `Complain`, `Kidhome` and `Teenhome` to factors where that makes sense.
- **Output:**
  - A cleaned dataset `food_clean` that everyone else uses.
  - A short cleaning log (what we changed and why) for the report.

### Phase 2: Univariate analysis
- Summary tables (mean, median, SD, min/max, skewness) for the numeric variables.
- Frequency tables and bar charts for the categorical variables.
- Histograms and density plots. The spending variables will be heavily right-skewed, so consider a log or square-root transform.
- Normality checks (Shapiro–Wilk and QQ-plots) to decide between parametric and non-parametric tests later.

### Phase 3: Bivariate and correlation analysis
- Correlation matrix and heatmap of the numeric variables. Use Spearman because of the skew.
- Income vs Total spent (scatter plot with a fitted line).
- Spending and purchase channels by Education, Marital status, Children and Age group (boxplots or violin plots).
- Channel preference: Web vs Catalogue vs Store, and how it relates to Income and Age.
- Discount purchases vs Children and Income (are families more price-sensitive?).
- Web visits vs web purchases (do visits turn into purchases?).
- Recency and Customer_Days vs spending (are long-term customers more valuable?).

### Phase 4: Hypothesis testing
For every test: state H0 and H1, check the assumptions, report the result and interpret it in plain words.

Candidate tests:
- **Children vs spending:** customers with children spend less (t-test or Wilcoxon).
- **Education vs spending:** spending differs by education level (ANOVA or Kruskal–Wallis, then a post-hoc test such as Tukey or Dunn).
- **Marital status vs spending:** spending differs by marital status (same tests).
- **Income vs spending:** income is correlated with total spent (correlation test).
- **Marital status vs education:** the two are independent (Chi-square test).
- **Complaints:** do customers who complained differ in income or spending? Keep the small sample (n = 20) in mind.
- **Multiple testing:** use a Bonferroni or BH correction because we run many tests.

### Phase 5: Customer profiles / extra analysis
This is optional but makes the report much stronger.
- **Clustering:**
  - Scale the variables.
  - Use k-means, with the elbow and silhouette methods to choose k (or use hierarchical clustering).
  - Describe each segment, for example "affluent wine buyers", "budget families" and "deal seekers".
- **PCA** on the spending and purchase variables, to see which variables move together.
- **Linear regression:** model `log(Total_amount_spent)` from Income, Children, Age, Education and Marital status, and check the residual diagnostics.

### Phase 6: Report writing
- **Report structure:**
  1. Introduction and aims
  2. Data description
  3. Data cleaning
  4. Univariate analysis
  5. Bivariate analysis and correlation
  6. Hypothesis tests
  7. Customer segments
  8. Conclusions and business recommendations
  9. Limitations
  10. Appendix
- Each person writes up their own part, and one person edits the whole report so it reads as one voice.
- Use consistent figure and table numbering.
- Interpret every plot and table. Don't just paste output.

### Phase 7: Final checks and submission
- Run the full R script from a clean R session to make sure it runs from start to finish.
- Check that all code is commented.
- Check that every number in the report matches the code output.
- Proofread the report.
- Submit the report and the R file by **Wed 4 Nov**, one day before the deadline.

### Phase 8: Presentation
- About 10–15 slides covering:
  - The key findings.
  - The 4–6 best plots.
  - The customer segments.
  - Our recommendations.
- Each member presents their own part.
- Do at least 2 rehearsals.

---

## 4. Suggested split (roles to assign)

| Role                     | Main work                                | Also responsible for                           | Member |
| ------------------------ | ---------------------------------------- | ---------------------------------------------- | ------ |
| **A: Data lead**         | Phase 1 (cleaning, new variables)        | Combining the R script and the final run check | O, D   |
| **B: Descriptives**      | Phase 2 + part of Phase 3 (demographics) | Plot theme and figure consistency              | O, D   |
| **C: Statistical tests** | Phase 4 + correlation analysis           | Checking test assumptions                      | F, P   |
| **D: Segmentation**      | Phase 5 (clustering, PCA, regression)    | Report editing and slide deck                  | F, P   |

**The one dependency:** B, C and D all need Person A's cleaned dataset. A should share a first version by **Tue 20 Oct**. Until then, the others can start drafting their code on the raw data.

---

## 5. Working rules

- Everyone uses the same cleaned dataset. Don't clean the data separately in your own script.
- Comment your code as you write it, not at the end.
- Save every plot you want in the report as a PNG in a shared `figures/` folder, with a clear file name.
- Short check-in (in person or online) twice a week to share progress and blockers.
