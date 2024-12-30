#---- --- ---- --- ---- ---  ---- --- ----#
library(dplyr)
library(rvest)
library(stringr)
# Cancer Survival rates (2000 - 2020)
# Set seed for reproducibility
set.seed(123)

# Define cancer types and years
cancer_types <- c(
  "Lung", "Breast", "Prostate", "Colorectal", "Stomach",
  "Liver", "Ovarian", "Pancreatic", "Leukemia", "Melanoma",
  "Kidney", "Bladder", "Esophageal", "Cervical", "Thyroid",
  "Brain", "Lymphoma", "Testicular", "Oral", "Bone"
)
years <- seq(2000, 2020, by = 5)  # 20-year period in 5-year intervals

cancer_parameters <- data.frame(
  Cancer = cancer_types,
  InitialSurvivalRate = round(runif(length(cancer_types), min = 70, max = 95), 1),
  DecayFactor = round(runif(length(cancer_types), min = 0.01, max = 0.05), 3)
)

# Generate survival data with distinct trajectories
cancer_survival <- expand.grid(Cancer = cancer_types, Year = years) |>
  left_join(cancer_parameters, by = "Cancer") |>
  mutate(
    TimeElapsed = Year - min(years),
    SurvivalRate = round(InitialSurvivalRate * exp(-DecayFactor * TimeElapsed), 1),
    Cancer = as.factor(Cancer)
  ) |>
  select(Cancer, Year, SurvivalRate)

# Save the data frame as an .rds file
saveRDS(cancer_survival, "data/cancer_survival.rds")

# Confirm the data is saved
print("Data frame saved as 'cancer_survival.rds'.")
#---- --- ---- --- ---- ---  ---- --- ----#
# TOTAL GOVERNMENT EXPENDITURES AS PERCENTAGES OF GDP: 1947-1995
# Total (Federal and State and Local) Government Finances

url <- "https://www.govinfo.gov/content/pkg/BUDGET-1997-TAB/html/BUDGET-1997-TAB-17-3.htm"
gov_spending <- read_html(url) |>
  html_elements("pre") |>
  html_text2()

# Split text into lines and clean
lines <- str_split(gov_spending, "\n")[[1]]
lines <- str_trim(lines)  # Remove extra spaces

# Filter rows of interest (e.g., years and data rows)
data_lines <- lines[grepl("^\\d{4}|^TQ", lines)]  # Rows starting with a year or "TQ"

# Split rows into columns based on multiple spaces
gov_df <- data_lines %>%
  str_squish() %>%              # Remove unnecessary spaces
  str_split("\\s+", simplify = TRUE) %>%  # Split by one or more spaces
  as.data.frame(stringsAsFactors = FALSE)

# Add meaningful column names
colnames(gov_df) <- c(
  "Fiscal_Year",
  "Total_Government_Expenditures",
  "Federal_Total",
  "On_Budget",
  "Off_Budget",
  "Federal_Grants",
  "State_Local_Expenditures"
)

# Clean numeric columns (remove parentheses and convert to numeric)
gov_df <- gov_df %>%
  mutate(across(-Fiscal_Year, ~ as.numeric(str_remove_all(., "[()]")))) |>
  mutate(Fiscal_Year = c(seq(1947,1976),"TQ",seq(1977,1995)))

saveRDS(gov_df, "data/govt_spending.rds")

#---- --- ---- --- ---- ---  ---- --- ----#
# Current receipts of government as a percentage of Gross Domestic Product, 1970 and 1979
#Current receipts of fifteen national governments as a percentage of gross domestic product
# A data frame containing fifteen country observations for two years.
# Edward Tufte. \emph{Beautiful Evidence}. Graphics Press, 174-176.
#
gdp <- data.frame(
Country = c("Sweden", "Netherlands", "Norway", "Britain", "France",
            "Germany", "Belgium", "Canada", "Finland", "Italy",
            "US", "Greece", "Switzerland", "Spain", "Japan"),
Year1970 = c(46.9, 44.0, 43.5, 40.7, 39.0, 37.5, 35.2, 35.2, 34.9, 30.4,
             30.3, 26.8, 26.5, 22.5, 20.7),
Year1979 = c(57.4, 55.8, 52.2, 39.0, 43.4, 42.9, 43.2, 35.8, 38.2, 35.7,
             32.5, 30.6, 33.2, 27.1, 26.6)
)

# Save the data frame as an RDS file
saveRDS(gdp, file = "data/gdp_data.rds")
