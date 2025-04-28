library(sf)
library(dplyr)
library(purrr)

# List all KML files
kml_files <- list.files("KML files", pattern = "\\.kml$", full.names = TRUE)

# Load each KML and standardise
regions_sf <- map_dfr(kml_files, function(file) {
  sf_obj <- st_read(file, quiet = TRUE)
  
  # Strip to geometry only
  sf_obj <- sf_obj %>%
    st_zm(drop = TRUE, what = "ZM") %>%  # remove Z or M dimensions if present
    select(geometry)  # ⚡ keep only the geometry column
  
  sf_obj$region_name <- gsub("\\.kml$", "", basename(file))  # add filename as region
  
  sf_obj
})

library(ggplot2)



ggplot(regions_sf) +
  geom_sf(aes(fill = region_name), colour = "black", alpha = 0.6) +
  theme_minimal() +
  labs(title = "UK Coastal Regions (From KML)", fill = "Region") +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.position = "bottom"
  )

library(readr)


# 2. Load all combined NBN records
nbn_data <- read_csv("all_nbn_records.csv")  # your saved combined file

# 3. Convert NBN data to sf points
nbn_points <- nbn_data %>%
  filter(!is.na(`Longitude (WGS84)`), !is.na(`Latitude (WGS84)`)) %>%
  st_as_sf(coords = c("Longitude (WGS84)", "Latitude (WGS84)"), crs = 4326)

# 4. Spatial join: points -> regions
joined <- st_join(nbn_points, regions_sf, join = st_intersects, left = FALSE)

# 5. Summarise: species counts per region
species_region_summary <- joined %>%
  group_by(region_name, species_name) %>%  # assuming 'Name' is your region name field
  summarise(n_records = n(), .groups = "drop") %>%
  arrange(region_name, desc(n_records))

# 6. View result
print(species_region_summary)

# 7. Save if you want
write_csv(species_region_summary, "outputs/species_counts_by_region.csv")

