#functions for accessing and working with NBN data

nbn_occ_dat <- function(species_name){
  
# Load required packages
require(httr)
require(jsonlite)
require(stringdist)
require(dplyr)

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
    return()
  }
  
} else {
  cat("❌ No species found for search term:", species_name, "\n")
  return()
  }


return(occurrences_df)

}


plot_occurrences_map <- function(
    data,
    species_name = NULL,
    max_year = NULL,
    max_month = NULL,
    point_colour = "#2176FF",
    point_size = 2.8,
    background_colour = "#FFFFFF",
    basemap_colour = "#D7D7E6",
    show_caption = TRUE,
    caption_text = "Data from NBN Atlas",
    caption_bg = "#191D2D",
    caption_col = "#FFFFFF"
) {
  # Load required packages
  library(ggplot2)
  library(rnaturalearth)
  library(sf)
  library(dplyr)
  library(showtext)
  library(cowplot)
  library(ggtext)
  
  font_add_google("Montserrat", "mont")
  showtext_auto()
  
  # Filter and prep data
  map_data <- data %>%
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(year), !is.na(month)) %>%
    mutate(year = as.numeric(year), month = as.numeric(month)) %>%
    mutate(year_month = year * 100 + month)
  
  if (!is.null(max_year) && !is.null(max_month)) {
    max_val <- max_year * 100 + max_month
    map_data <- map_data %>% filter(year_month <= max_val)
  }
  
  # Load UK base map
  uk <- ne_countries(scale = "medium", country = "United Kingdom", returnclass = "sf")
  
  # Format subtitle (date)
  subtitle_text <- if (!is.null(max_year) && !is.null(max_month)) {
    formatted_date <- format(as.Date(paste0(max_year, "-", max_month, "-01")), "%b %Y")
    paste0("Occurrences up to ", formatted_date)
  } else {
    NULL
  }
  
  # Format italic species name
  title_text <- if (!is.null(species_name)) {
    paste0("<i>", paste(strsplit(species_name, " ")[[1]], collapse = " "), "</i>")
  } else {
    ""
  }
  
  # Base plot
  base_plot <- ggplot() +
    geom_sf(data = uk, fill = basemap_colour, colour = "black") +
    geom_point(
      data = map_data,
      aes(x = decimalLongitude, y = decimalLatitude),
      colour = point_colour, alpha = 0.9, size = point_size
    ) +
    coord_sf(xlim = c(-9, 2), ylim = c(49.5, 61), expand = FALSE) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 14, base_family = "mont") +
    theme(
      plot.title.position = "plot",
      plot.title = ggtext::element_textbox_simple(
        halign = 0.5,
        size = 16,
        padding = margin(0, 0, 0, 0),
        margin = margin(b = 6)
      ),
      ,
      plot.subtitle = element_text(hjust = 0.5, size = 13, family = "mont"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = background_colour, colour = NA),
      plot.margin = margin(10, 20, 20, 20)
    )
  
  # Assign plot
  plot_with_logo <- base_plot
  
  # Add footer caption (optional)
  if (show_caption) {
    full_plot <- cowplot::plot_grid(
      plot_with_logo,
      cowplot::ggdraw() +
        cowplot::draw_label(
          caption_text,
          fontfamily = "mont",
          fontface = "plain",
          size = 11,
          x = 0.5, hjust = 0.5,
          colour = caption_col
        ) +
        theme(plot.background = element_rect(fill = caption_bg, colour = NA)),
      ncol = 1,
      rel_heights = c(1, 0.05)
    )
  } else {
    full_plot <- plot_with_logo
  }
  
  return(full_plot)
}




animate_occurrences_over_time <- function(
    data,
    species_name = NULL,
    filename = "species_animation.gif",
    format = "gif",  # "gif" or "mp4"
    speed = 1,
    point_colour = "dodgerblue3",
    point_size = 2.8,
    old_point_colour = "grey30",
    pause_at_end = TRUE
) {
  require(ggplot2)
  require(gganimate)
  require(gifski)
  require(av)
  require(rnaturalearth)
  require(rnaturalearthdata)
  require(sf)
  require(dplyr)
  require(magick)
  require(stringr)
  
  # Filter and prepare data
  map_data <- data %>%
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(year)) %>%
    mutate(year = as.numeric(year)) %>%
    arrange(year) %>%
    mutate(point_id = row_number())
  
  # Load UK map
  uk <- rnaturalearth::ne_countries(scale = "medium", country = "United Kingdom", returnclass = "sf")
  
  # Compute running total
  frame_data <- map_data %>%
    count(year, name = "records") %>%
    arrange(year) %>%
    mutate(
      running_total = cumsum(records),
      frame_label = paste0("Year: ", year, "   •   Total records: ", running_total)
    )
  
  # Join frame label back to map_data
  map_data <- left_join(map_data, frame_data[, c("year", "frame_label")], by = "year")
  
  # If pausing at end, repeat final year state a few times
  if (pause_at_end) {
    final_label <- tail(unique(map_data$frame_label), 1)
    pause_frames <- rep(final_label, 10)  # 10 extra frames
    map_data <- bind_rows(
      map_data,
      map_data %>% filter(frame_label == final_label) %>% slice(rep(1:n(), 10))
    )
  }
  
  
  # Build plot
  p <- ggplot() +
    geom_sf(data = uk, fill = "grey90", colour = "black") +
    shadow_mark(alpha = 0.3, size = point_size * 0.7, colour = old_point_colour) +
    geom_point(
      data = map_data,
      aes(x = decimalLongitude, y = decimalLatitude, group = point_id),
      colour = point_colour, alpha = 0.9, size = point_size
    ) +
    coord_sf(xlim = c(-11, 2), ylim = c(49.5, 61), expand = FALSE) +
    labs(
      title = paste0("Spread of", if (!is.null(species_name)) paste0(" ", species_name)),
      subtitle = "{closest_state}",
      x = "Longitude", y = "Latitude"
    ) +
    theme_minimal(base_size = 14, base_family = "mont") +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 10),
      plot.margin = margin(20, 20, 40, 20)
    ) +
    transition_states(states = frame_label, transition_length = 1, state_length = 2) +
    ease_aes('linear')
  
  # Animation settings
  nframes <- length(unique(map_data$frame_label)) * 10
  fps <- 10 * speed
  
  # Auto filename fix
  filename <- str_replace(filename, "\\.gif$|\\.mp4$", paste0(".", format))
  
  # Choose renderer
  renderer <- if (format == "mp4") av_renderer(filename) else gifski_renderer()
  
  # Animate
  anim <- animate(p, renderer = renderer, width = 900, height = 700, fps = fps, nframes = nframes)
  
  # Save manually for GIF (to allow post-edit)
  if (format == "gif") {
    anim_save(filename, animation = anim)
    
    # Optional: Add logo for GIF
    if (file.exists("rock_pool_logo.png")) {
      gif <- magick::image_read(filename)
      logo <- magick::image_read("rock_pool_logo.png") %>%
        magick::image_scale("150")
      gif_with_logo <- magick::image_apply(gif, function(frame) {
        magick::image_composite(frame, logo, offset = "+730+500")
      })
      magick::image_write(gif_with_logo, path = filename)
      cat("✅ Logo added to animation\n")
    } else {
      cat("⚠️ Logo file not found: 'rock_pool_logo.png'\n")
    }
  }
  
  cat("✅ Animation saved as:", filename, "with speed =", speed, "\n")
}



