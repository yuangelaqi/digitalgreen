# ---- Purpose: Identify where users are located (country + region) ----
# This script extracts unique users from the chat data and lists the countries and subnational
# geographies (level 3) they are associated with, based on user profile metadata.

# ---- Load Libraries ----
library(tidyverse)
library(here)

# ---- Load Cleaned Data ----
combined_df <- read_rds(here("output", "combined_df_clean.rds"))

# ---- Extract Unique Users and Their Locations ----
user_locations <- combined_df %>%
  select(user_id, user_country, user_geography_level3) %>%
  distinct(user_id, .keep_all = TRUE)

# ---- Count Users by Country ----
user_counts_by_country <- user_locations %>%
  count(user_country, sort = TRUE)

# ---- Preview Results ----
print(user_counts_by_country)