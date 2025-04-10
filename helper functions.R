#functions for accessing and working with NBN data

nbn_occ_dat <- function(species_name){
  
# Load required packages
require(httr)
require(jsonlite)
require(stringdist)
require(dplyr)

# Define species name
species_name <- "Magallana gigas"

# Step 1: Find the best match GUID
search_url <- paste0("https://species-ws.nbnatlas.org/search?q=", URLencode(species_name))
search_response <- GET(search_url)
search_content <- content(search_response, as = "text")
search_results <- fromJSON(search_content)

results <- search_results$searchResults$results

if (length(results) > 0) {
  names_list <- results$name
  guids_list <- results$guid
  
  # Match closest name
  distances <- stringdist(tolower(species_name), tolower(names_list), method = "lv")
  best_match_index <- which.min(distances)
  
  species_guid <- guids_list[best_match_index]
  matched_name <- names_list[best_match_index]
  cat("Best match for", species_name, "is:", matched_name, "\nGUID:", species_guid, "\n")
  
  # Step 2: Automatically download all occurrence records
  page_size <- 1000
  start <- 0
  all_occurrences <- list()
  more_results <- TRUE
  page_count <- 1
  
  while (more_results) {
    cat("Fetching page", page_count, "\n")
    
    url <- paste0(
      "https://records-ws.nbnatlas.org/occurrences/search?",
      "fq=taxon_concept_lsid:", URLencode(species_guid),
      "&pageSize=", page_size,
      "&start=", start
    )
    
    response <- GET(url)
    content_json <- content(response, as = "text", encoding = "UTF-8")
    data <- fromJSON(content_json)
    
    if (length(data$occurrences) == 0) {
      more_results <- FALSE
    } else {
      all_occurrences[[page_count]] <- as.data.frame(data$occurrences)
      start <- start + page_size
      page_count <- page_count + 1
    }
  }
  
  # Combine into one data frame
  if (length(all_occurrences) > 0) {
    occurrences_df <- bind_rows(all_occurrences)  # <- FIXED LINE
    cat("✅ Total records retrieved:", nrow(occurrences_df), "\n")
  } else {
    cat("⚠️ No occurrence records found for", matched_name, "\n")
  }
  
} else {
  cat("❌ No species found for search term:", species_name, "\n")
}

return(occurrences_df)

}


plot_occurrences_map <- function(data, species_name = NULL, max_year = NULL, max_month = NULL) {

  require(ggplot2)
  require(rnaturalearth)
  require(rnaturalearthdata)
  require(sf)
  
  
    # Filter valid coordinates
  map_data <- data[!is.na(data$decimalLatitude) & !is.na(data$decimalLongitude), ]
  
  # Ensure year and month exist and are numeric
  if (!is.null(max_year) && !is.null(max_month) &&
      "year" %in% names(map_data) && "month" %in% names(map_data)) {
    
    # Convert year and month to numeric
    map_data$year <- suppressWarnings(as.numeric(map_data$year))
    map_data$month <- suppressWarnings(as.numeric(map_data$month))
    
    # Create comparable YYYYMM value
    map_data <- map_data[!is.na(map_data$year) & !is.na(map_data$month), ]
    map_data$year_month <- map_data$year * 100 + map_data$month
    
    # Construct target YYYYMM
    max_year_month <- as.numeric(paste0(max_year, sprintf("%02d", max_month)))
    
    # Filter
    map_data <- subset(map_data, year_month <= max_year_month)
  }
  
  # Load UK base map
  uk <- ne_countries(scale = "medium", country = "United Kingdom", returnclass = "sf")
  
  # Create the map
  ggplot() +
    geom_sf(data = uk, fill = "grey90", colour = "black") +
    geom_point(
      data = map_data,
      aes(x = decimalLongitude, y = decimalLatitude),
      colour = "blue", alpha = 0.6, size = 1.5
    ) +
    coord_sf(xlim = c(-11, 2), ylim = c(49.5, 61), expand = FALSE) +
    labs(
      title = paste0("Occurrences",
                     if (!is.null(species_name)) paste0(" of ", species_name) else "",
                     if (!is.null(max_year)) paste0(" up to ", max_year, "-", sprintf("%02d", max_month)) else ""),
      x = "Longitude", y = "Latitude"
    ) +
    theme_minimal()
}


animate_occurrences_over_time <- function(data, species_name = NULL, filename = "species_animation.gif", speed = 0.5) {
  
  library(ggplot2)
  library(gganimate)
  library(gifski)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  library(dplyr)
  
  # Filter valid lat/lon and year fields
  map_data <- data %>%
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(year)) %>%
    mutate(year = as.numeric(year))
  
  # UK base map
  uk <- ne_countries(scale = "medium", country = "United Kingdom", returnclass = "sf")
  
  # Adjust speed scaling (lower = slower)
  base_transition <- 2
  base_state <- 1
  transition_len <- base_transition / speed
  state_len <- base_state / speed
  
  # Create the plot
  p <- ggplot() +
    geom_sf(data = uk, fill = "grey90", colour = "black") +
    
    # Past points (faded)
    shadow_mark(alpha = 0.3, size = 1.8, colour = "grey30") +
    
    # New points (highlighted)
    geom_point(
      data = map_data,
      aes(x = decimalLongitude, y = decimalLatitude),
      colour = "dodgerblue3", alpha = 0.9, size = 2.8
    ) +
    
    coord_sf(xlim = c(-11, 2), ylim = c(49.5, 61), expand = FALSE) +
    labs(
      title = paste0("Spread of",
                     if (!is.null(species_name)) paste0(" ", species_name),
                     "\n{closest_state}"),
      x = "Longitude", y = "Latitude"
    ) +
    theme_minimal() +
    
    # Animate by year
    transition_states(
      states = year,
      transition_length = transition_len,
      state_length = state_len,
      wrap = FALSE
    ) +
    ease_aes('linear')
  
  # Animate and save
  anim <- animate(p, renderer = gifski_renderer(), width = 800, height = 600)
  anim_save(filename, animation = anim)
  
  cat("✅ Animation saved as:", filename, "\n")
}
