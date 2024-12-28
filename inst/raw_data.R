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

url <- "https://www.govinfo.gov/content/pkg/BUDGET-1997-TAB/html/BUDGET-1997-TAB-17-3.htm"
gov_spending <- read_html(url) |>
  html_elements("pre") |>
  html_text2()

# Split text into lines and clean
lines <- str_split(gov_spending, "\n")[[1]]
lines <- str_trim(lines)  # Remove extra spaces

# Extract rows of interest (skip headers and footnotes)
data_lines <- lines[grepl("^\\d{4}|^TQ", lines)]  # Filter years (e.g., "1947", "TQ")

# Parse into a data frame
gov_df <- data_lines %>%
  str_remove_all("\\.{2,}") %>%  # Remove extra dots
  str_squish() %>%              # Remove extra spaces
  str_split_fixed("\\s{2,}", n = 7) %>%  # Split columns by multiple spaces
  as.data.frame()

# Add column names
colnames(gov_df) <- c(
  "Fiscal_Year",
  "Total_Government_Expenditures",
  "Federal_Total",
  "On_Budget",
  "Off_Budget",
  "Federal_Grants",
  "State_Local_Expenditures"
)

gov_df <- gov_df %>%
  mutate(across(-Fiscal_Year, ~ str_remove_all(., "[()]") %>% as.numeric()))
