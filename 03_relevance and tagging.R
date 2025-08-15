# ---- Purpose: Identify / tag farming-relevant questions, weather questions, etc. ----

# ---- Load Libraries ----
library(tidyverse)
library(here)
library(sf)
library(ggplot2)
library(glue)
library(openxlsx)
library(dplyr)
library(readr)

# ---- Define File Paths ----
data_path <- here("/Users/angelayu/Documents/RStudio TAF/Input")
output_path <- here("/Users/angelayu/Documents/RStudio TAF/Output")

# ---- Load Cleaned Dataset ----
combined_df <- read_rds(here(output_path, "combined_df_clean.rds"))
View(combined_df)

# ---- 1. View & Export Questions Only ----
questions <- combined_df["translated_message"] %>%
  filter(!if_all(everything(), ~ is.na(.) | . == ""))

View(questions)

output_file <- file.path(output_path, "farmer_questions.csv")
write.csv(questions, output_file, row.names = FALSE)

# 2. Main theme categorization function
categorize_question <- function(q) {
  q_lower <- tolower(q)
  if (grepl("fertilizer|soil|manure|compost|soil fertility", q_lower)) {
    return("Soil & Fertilizer Management")
  } else if (grepl("pest|insect|disease|weed|repellent", q_lower)) {
    return("Pest, Disease & Weed Control")
  } else if (grepl("weather|rain|climate|drought", q_lower)) {
    return("Weather & Climate")
  } else if (grepl("seed|nursery|planting|cuttings|germination", q_lower)) {
    return("Planting & Propagation")
  } else if (grepl("harvest|yield|production|acre", q_lower)) {
    return("Harvest & Yield")
  } else if (grepl("livestock|cow|goat|chicken|egg|dairy|pig", q_lower)) {
    return("Livestock & Poultry")
  } else if (grepl("market|price|sell|cost|profit|expenses", q_lower)) {
    return("Market & Farm Economics")
  } else {
    return("Other / Miscellaneous")
  }
}

# Apply main categorization
questions <- questions %>%
  mutate(theme = sapply(translated_message, categorize_question))

# 3. Refine "Other / Miscellaneous" sub-themes
refine_misc <- function(q) {
  q_lower <- tolower(q)
  if (grepl("training|teach|learn|advice|help", q_lower)) {
    return("Training & Advice")
  } else if (grepl("equipment|tractor|machine|tool|sprayer", q_lower)) {
    return("Farming Equipment & Tools")
  } else if (grepl("irrigation|water|drip|sprinkler|watering", q_lower)) {
    return("Irrigation & Water Management")
  } else if (grepl("storage|preserve|packaging|transport", q_lower)) {
    return("Post-Harvest Handling & Storage")
  } else if (grepl("government|policy|subsidy|program|support", q_lower)) {
    return("Government Policies & Support")
  } else if (grepl("organic|regenerative|sustainable|eco-friendly", q_lower)) {
    return("Sustainable Farming Practices")
  } else if (grepl("variety|species|type|breed", q_lower)) {
    return("Crop & Livestock Varieties")
  } else {
    return("General / Unclassified")
  }
}

questions <- questions %>%
  mutate(refined_theme = ifelse(theme == "Other / Miscellaneous",
                                sapply(translated_message, refine_misc),
                                theme))
#1,784,673 entries

# 4. Count results
main_theme_counts <- questions %>%
  count(theme, sort = TRUE)

refined_misc_counts <- questions %>%
  filter(theme == "Other / Miscellaneous") %>%
  count(refined_theme, sort = TRUE)

# Print results
print(main_theme_counts)
cat("\nBreakdown of Other / Miscellaneous:\n")
print(refined_misc_counts)

# Save results to CSV
write_csv(main_theme_counts, "main_theme_counts.csv")
write_csv(refined_misc_counts, "refined_misc_counts.csv")


