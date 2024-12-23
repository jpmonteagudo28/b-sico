#---- --- ---- --- ---- ---  ---- --- ----#
# Cancer Survival rates (2000 - 2020)
library(dplyr)

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
