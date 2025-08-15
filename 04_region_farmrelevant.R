# ---- Purpose: Identify where users are located and map regional engagement ----
# This script extracts unique users from the Farmer.Chat data and lists the countries and 
# subnational geographies (level 3) in which farming-relevant questions are asked
# It then generates maps of user engagement by region for Kenya, Ethiopia, and India.

# ---- Load Libraries ----
library(tidyverse)
library(here)
library(sf)
library(ggplot2)
library(glue)

## Unique NUMBER of farmers who have asked farming relevant question

# ---- Define File Paths ----
data_path <- here("/Users/angelayu/Documents/RStudio TAF/Input")
output_path <- here("/Users/angelayu/Documents/RStudio TAF/Output")

# ---- Load Cleaned Dataset ----
combined_df <- read_rds(here(output_path, "combined_df_clean.rds"))

# ---- Extract Unique Users and Their Locations ----
user_locations <- combined_df %>%
  select(user_id, user_country, user_geography_level3) %>%
  distinct(user_id, .keep_all = TRUE)

View(user_locations) #101,527 entries

# ---- Count Users by Country ----
user_counts_by_country <- user_locations %>%
  count(user_country, sort = TRUE)

print(user_counts_by_country)  # Preview country-level engagement

# ---- Load Admin Level 2 Shapefiles ----
geo <- here("/Users/angelayu/Documents/RStudio TAF/Geo")
ethiopia_sf <- st_read(here(geo, "gadm41_ETH_2.shp"))
kenya_sf    <- st_read(here(geo, "gadm41_KEN_2.shp"))
india_sf    <- st_read(here(geo, "gadm41_IND_2.shp"))

# ---- Function to Generate Country-Level User Maps ----
plot_user_map <- function(combined_df, shape_df, country_name, region_col = "NAME_2") {
  # Aggregate user counts by geography
  user_counts <- combined_df %>%
    filter(
      user_country == country_name,
      user_intent == "Farming_related"
    ) %>%
    group_by(user_geography_level3) %>%
    summarise(unique_farmers = n_distinct(user_id), .groups = "drop") %>%
    rename(!!region_col := user_geography_level3)
  
  # Join user data to shapefile
  map_df <- shape_df %>%
    left_join(user_counts, by = region_col)
  
  # Plot
  ggplot(map_df) +
    geom_sf(aes(fill = unique_farmers), color = "white", size = 0.1) +
    scale_fill_viridis_c(option = "C", na.value = "grey90", direction = -1) +
    labs(
      title = glue("Farmer.Chat Engagement in {country_name} by Region"),
      subtitle = "Unique farmers who have asked a farming-related question",
      fill = "Unique farmers"
    ) +
    theme_minimal()
}

# ---- Generate and Save Maps by Country ----

# Kenya
kenya_plot <- plot_user_map(combined_df, kenya_sf, "Kenya")
ggsave(here(output_path, "map_kenya_farmrel.png"), plot = kenya_plot, width = 8, height = 6, dpi = 300)

# Ethiopia
ethiopia_plot <- plot_user_map(combined_df, ethiopia_sf, "Ethiopia")
ggsave(here(output_path, "map_ethiopia_farmrel.png"), plot = ethiopia_plot, width = 8, height = 6, dpi = 300)

# India
india_plot <- plot_user_map(combined_df, india_sf, "India")
ggsave(here(output_path, "map_india_farmrel.png"), plot = india_plot, width = 8, height = 6, dpi = 300)

# Nigeria
nigeria_plot <- plot_user_map(combined_df, nigeria_sf, "Nigeria")
ggsave(here(output_path, "map_nigeria_farmrel.png"), plot = nigeria_plot, width = 8, height = 6, dpi = 300)

## PROPORTION of farmers who have asked farming relevant question

plot_user_map_prop <- function(full_df, shape_df, country_name, region_col = "NAME_2") {
  
  # Total unique farmers per region (denominator)
  total_farmers <- full_df %>%
    filter(user_country == country_name) %>%
    group_by(user_geography_level3) %>%
    summarise(total_unique_farmers = n_distinct(user_id), .groups = "drop")
  
  # Unique farming-related farmers per region (numerator)
  farming_related <- full_df %>%
    filter(user_country == country_name, user_intent == "Farming_related") %>%
    group_by(user_geography_level3) %>%
    summarise(farming_unique_farmers = n_distinct(user_id), .groups = "drop")
  
  # Join numerator and denominator, calculate proportion
  user_props <- total_farmers %>%
    left_join(farming_related, by = "user_geography_level3") %>%
    mutate(
      farming_unique_farmers = replace_na(farming_unique_farmers, 0),
      proportion_farming = farming_unique_farmers / total_unique_farmers
    ) %>%
    rename(!!region_col := user_geography_level3)
  
  # Join to shapefile
  map_df <- shape_df %>%
    left_join(user_props, by = region_col)
  
  # Plot proportion
  ggplot(map_df) +
    geom_sf(aes(fill = proportion_farming), color = "white", size = 0.1) +
    scale_fill_viridis_c(
      option = "C",
      na.value = "grey90",
      direction = -1,
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      title = glue("Proportion of Farmers Asking Farming-related Questions in {country_name}"),
      subtitle = "Proportion of unique farmers with at least one farming-related question",
      fill = "Proportion"
    ) +
    theme_minimal()
}

# ---- Generate and Save Maps ----
kenya_plot <- plot_user_map_prop(combined_df, kenya_sf, "Kenya")
ggsave(here(output_path, "map_kenya_farming_proportion.png"), kenya_plot, width = 8, height = 6, dpi = 300)

ethiopia_plot <- plot_user_map_prop(combined_df, ethiopia_sf, "Ethiopia")
ggsave(here(output_path, "map_ethiopia_farming_proportion.png"), ethiopia_plot, width = 8, height = 6, dpi = 300)

india_plot <- plot_user_map_prop(combined_df, india_sf, "India")
ggsave(here(output_path, "map_india_farming_proportion.png"), india_plot, width = 8, height = 6, dpi = 300)


