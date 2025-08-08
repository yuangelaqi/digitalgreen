# ---- Purpose: Identify where users are located and map regional engagement ----
# This script extracts unique users from the Farmer.Chat data and lists the countries and 
# subnational geographies (level 3) they are associated with, based on profile metadata.
# It then generates maps of user engagement by region for Kenya, Ethiopia, and India.

# ---- Load Libraries ----
library(tidyverse)
library(here)
library(sf)
library(ggplot2)
library(glue)

# ---- Load Cleaned Dataset ----
combined_df <- read_rds(here("output", "combined_df_clean.rds"))

# ---- Extract Unique Users and Their Locations ----
user_locations <- combined_df %>%
  select(user_id, user_country, user_geography_level3) %>%
  distinct(user_id, .keep_all = TRUE)

# ---- Count Users by Country ----
user_counts_by_country <- user_locations %>%
  count(user_country, sort = TRUE)

print(user_counts_by_country)  # Preview country-level engagement

# ---- Load Admin Level 2 Shapefiles ----
ethiopia_sf <- st_read(here("geo", "gadm41_ETH_2.shp"))
kenya_sf    <- st_read(here("geo", "gadm41_KEN_2.shp"))
india_sf    <- st_read(here("geo", "gadm41_IND_2.shp"))

# ---- Function to Generate Country-Level User Maps ----
plot_user_map <- function(user_df, shape_df, country_name, region_col = "NAME_2") {
  # Aggregate user counts by geography
  user_counts <- user_df %>%
    filter(user_country == country_name) %>%
    count(user_geography_level3, name = "n") %>%
    rename(!!region_col := user_geography_level3)
  
  # Join user data to shapefile
  map_df <- shape_df %>%
    left_join(user_counts, by = region_col)
  
  # Plot
  ggplot(map_df) +
    geom_sf(aes(fill = n), color = "white", size = 0.1) +
    scale_fill_viridis_c(option = "C", na.value = "grey90", direction = -1) +
    labs(
      title = glue("Farmer.Chat Engagement in {country_name} by Region"),
      subtitle = "Based on user profile metadata",
      fill = "User count"
    ) +
    theme_minimal()
}

# ---- Generate and Save Maps by Country ----

# Kenya
kenya_plot <- plot_user_map(user_locations, kenya_sf, "Kenya")
ggsave(here("output", "map_kenya_users.png"), plot = kenya_plot, width = 8, height = 6, dpi = 300)

# Ethiopia
ethiopia_plot <- plot_user_map(user_locations, ethiopia_sf, "Ethiopia")
ggsave(here("output", "map_ethiopia_users.png"), plot = ethiopia_plot, width = 8, height = 6, dpi = 300)

# India
india_plot <- plot_user_map(user_locations, india_sf, "India")
ggsave(here("output", "map_india_users.png"), plot = india_plot, width = 8, height = 6, dpi = 300)

