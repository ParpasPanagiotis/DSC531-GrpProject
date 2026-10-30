# Variables: cleaned dataset

`data/food_clean.rds` is created by `scripts/01_cleaning.R` and loaded by every later script with `data <- readRDS(clean_file)`.

**1997 customers, 29 columns.** Ranges are for the cleaned data.

## Original variables (from `Food_raw.csv`)

### Customer profile

| Variable | Type | Range | Meaning |
|---|---|---|---|
| `Income` | numeric | 2,447 – 113,734 | Annual household income (dollars) |
| `Age` | numeric | 24 – 80 | Age of the customer (years) |
| `Kidhome` | discrete count | 0 – 2 | Number of small children at home |
| `Teenhome` | discrete count | 0 – 2 | Number of teenagers at home |
| `Customer_Days` | numeric | 2,159 – 2,858 | Days as a customer (5.9 – 7.8 years) |
| `Recency` | numeric | 0 – 99 | Days since the last purchase |
| `Complain` | binary (0/1) | 0 – 1 | 1 = complained in the last 2 years (only 19 customers) |

### Spending (last 2 years)

| Variable | Type | Range | Meaning |
|---|---|---|---|
| `MntWines` | numeric | 0 – 1,493 | Amount spent on wine |
| `MntFruits` | numeric | 0 – 199 | Amount spent on fruit |
| `MntMeatProducts` | numeric | 0 – 1,725 | Amount spent on meat |
| `MntFishProducts` | numeric | 0 – 259 | Amount spent on fish |
| `MntSweetProducts` | numeric | 0 – 262 | Amount spent on sweets |
| `Total_amount_spent` | numeric | 6 – 2,491 | Sum of the five `Mnt*` columns |

> `Total_amount_spent` is built from the `Mnt*` columns. Do not use it in the same test or model as them.

### Purchases

| Variable | Type | Range | Meaning |
|---|---|---|---|
| `NumWebPurchases` | count | 0 – 27 | Purchases made on the website |
| `NumCatalogPurchases` | count | 0 – 28 | Purchases made from the catalogue |
| `NumStorePurchases` | count | 0 – 13 | Purchases made in stores |
| `NumDealsPurchases` | count | 0 – 15 | Purchases made with a discount |
| `NumWebVisitsMonth` | count | 0 – 20 | Visits to the website **last month** |

## Combined variables (replace the dummy columns)

| Variable | Type | Levels | Replaces |
|---|---|---|---|
| `Marital_State` | factor | Divorced, Living together, Married, Single, Widow | `Divorced`, `Married`, `Single`, `Living_together`, `Widow` |
| `schoolinglevel` | **ordered** factor | hs < bachelor < masters < phd | `Up_to_high_school`, `Up_to_undergraduate_degree`, `Up_to_masters_degree`, `PhD` |

> The 9 original dummy columns are removed. In `lm()`, use `factor(schoolinglevel, ordered = FALSE)` to get the usual "vs hs" comparisons instead of `.L / .Q / .C` terms.

## New variables (created in `01_cleaning.R`)

| Variable | Type | Values | How it is made |
|---|---|---|---|
| `Children` | discrete count | 0 – 3 | `Kidhome + Teenhome` |
| `HasChildren` | factor | No, Yes | Yes if `Children > 0` |
| `AgeGroup` | factor | Under 30, 30-44, 45-59, 60+ | `Age` cut at 29 / 44 / 59 |
| `IncomeGroup` | factor | Q1 Lowest, Q2, Q3, Q4 Highest | `Income` cut at its quartiles (35,641 / 51,529 / 68,352), about 500 customers each |
| `TotalPurchases` | count | 1 – 32 | `NumWebPurchases + NumCatalogPurchases + NumStorePurchases` |
| `WebPercent` | % | 0 – 100 | Share of purchases made on the web |
| `CataloguePercent` | % | 0 – 100 | Share of purchases made from the catalogue |
| `StorePercent` | % | 0 – 100 | Share of purchases made in stores |
| `DealPercent` | % | 0 – 100 | Share of purchases made with a discount (`NumDealsPurchases / TotalPurchases`) |

> `WebPercent + CataloguePercent + StorePercent = 100` for every customer.

## Variable groups used in the scripts

- **`num_cols`** (in `00_setup.R`): the 15 numerical variables used for descriptive statistics and correlations: `Income`, `Recency`, the five `Mnt*`, the four `Num*Purchases`, `NumWebVisitsMonth`, `Age`, `Customer_Days`, `Total_amount_spent`.
- **Described with frequency tables instead:** `Kidhome`, `Teenhome`, `Complain`, `Marital_State`, `schoolinglevel`.
- **Small groups (interpret with care):** `Complain = 1` (19), `Widow` (69), `hs` (49), `Under 30` (37), `Children = 3` (45).

## Distribution notes

- **None of the numerical variables is normal**, so use non-parametric methods (Spearman, Wilcoxon, Kruskal–Wallis).
- **Strongly right-skewed:** all spending columns and purchase counts.
- **Roughly uniform:** `Recency` and `Customer_Days`.
- **Roughly symmetric but not normal:** `Income` and `Age` (rejected by Lilliefors).
