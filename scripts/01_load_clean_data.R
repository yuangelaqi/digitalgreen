# ---- Purpose: Load, clean, and standardize Farmer.Chat raw data ----
# This script ingests raw .csv exports from the chatbot platform, merges them into a single
# dataset, and applies standard cleaning operations. These include consistent timestamp parsing,
# handling missing values, and saving a cleaned version of the dataset for downstream analysis.

# ---- Load Libraries ----
library(tidyverse)
library(lubridate)
library(here)

# ---- Define File Paths ----
data_path <- here("data")
output_path <- here("output")

# ---- Load and Combine CSV Files ----
csv_files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)

combined_df <- csv_files %>%
  map_dfr(read_csv, show_col_types = FALSE)

# ---- Standardize Column Names ----
combined_df <- combined_df %>%
  janitor::clean_names()  # optional, for consistent snake_case

# ---- Convert Timestamps ----
combined_df <- combined_df %>%
  mutate(timestamp = ymd_hms(created_on, quiet = TRUE))
         # add additional conversions here if needed

# ---- Clean Missing Values ----
# Replace empty strings with NA in all character columns
combined_df <- combined_df %>%
  mutate(across(where(is.character), ~ na_if(trimws(.), "")))

# ---- Review Missingness ----
missing_summary <- combined_df %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") %>%
  arrange(desc(missing_pct))

# Write this to disk for diagnostics
write_csv(missing_summary, file = here("output", "missing_summary.csv"))

# ---- Save Cleaned Dataset ----
write_rds(combined_df, file = here("output", "combined_df_clean.rds"))
