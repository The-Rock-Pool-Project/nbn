#functions for accessing and working with NBN data

nbn_occ_dat <- function(species_name, email = "you@example.com", dest_folder = tempdir(), filter_presence_only = TRUE) {
  
  require(httr)
  require(jsonlite)
  require(stringdist)
  require(readr)
  require(dplyr)
  require(glue)
  
  # Step 1: Get LSID via species search
  search_url <- paste0("https://species-ws.nbnatlas.org/search?q=", URLencode(species_name))
  search_response <- GET(search_url)
  search_content <- content(search_response, as = "text")
  search_results <- fromJSON(search_content)
  
  results <- search_results$searchResults$results
  
  if (length(results) == 0) {
    cat("❌ No species found for search term:", species_name, "\n")
    return(NULL)
  }
  
  # Find best match using string distance
  names_list <- results$name
  guids_list <- results$guid
  distances <- stringdist::stringdist(tolower(species_name), tolower(names_list), method = "lv")
  best_match_index <- which.min(distances)
  matched_name <- names_list[best_match_index]
  species_guid <- guids_list[best_match_index]
  
  cat("✅ Best match for", species_name, "is:", matched_name, "\nGUID:", species_guid, "\n")
  
  # Step 2: Download CSV via NBN API (zipped)
  zip_filename <- file.path(dest_folder, paste0(gsub(" ", "_", matched_name), ".zip"))
  csv_download_url <- glue(
    "https://records-ws.nbnatlas.org/occurrences/index/download?",
    "reasonTypeId=10&",
    "email={email}&",
    "q=*:*&",
    "fq=taxon_concept_lsid:{species_guid}&",
    "format=csv&",
    "type=full&",
    "qa=none"
  )
  
  download.file(csv_download_url, destfile = zip_filename, mode = "wb")
  
  # Step 3: Unzip and read data.csv
  unzip_dir <- file.path(dest_folder, gsub(" ", "_", matched_name))
  unzip(zip_filename, exdir = unzip_dir)
  data_path <- file.path(unzip_dir, "data.csv")
  
  if (!file.exists(data_path)) {
    cat("❌ No data.csv found in zip file for", matched_name, "\n")
    return(NULL)
  }
  
  occ_data <- read_csv(data_path, show_col_types = FALSE)
  
  # Step 4: Filter for presence-only if desired
  if (filter_presence_only && "absence" %in% names(occ_data)) {
    occ_data <- occ_data %>%
      filter(is.na(absence) | tolower(absence) != "true")
    cat("✅ Records after filtering absences:", nrow(occ_data), "\n")
  }
  
  return(occ_data)
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
  library(ggplot2)
  library(rnaturalearth)
  library(sf)
  library(dplyr)
  library(showtext)
  library(cowplot)
  library(ggtext)
  
  font_add_google("Montserrat", "mont")
  showtext_auto()
  
  # 🧠 Rename NBN Atlas fields to standard ones
  data <- data %>%
    rename(
      decimalLatitude = `Latitude (WGS84)`,
      decimalLongitude = `Longitude (WGS84)`
    )
  
  # ✅ Clean and filter data
  map_data <- data %>%
    filter(is.na(`Occurrence status`) | tolower(`Occurrence status`) != "absent") %>%
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(`Start date year`), !is.na(`Start date month`)) %>%
    mutate(
      year = as.numeric(`Start date year`),
      month = as.numeric(`Start date month`),
      year_month = year * 100 + month
    )
  
  if (!is.null(max_year) && !is.null(max_month)) {
    max_val <- max_year * 100 + max_month
    map_data <- map_data %>% filter(year_month <= max_val)
  }
  
  # 🗺️ UK base map
  uk <- rnaturalearth::ne_countries(scale = "medium", country = "United Kingdom", returnclass = "sf")
  
  # 🧾 Subtitle with date
  subtitle_text <- if (!is.null(max_year) && !is.null(max_month)) {
    formatted_date <- format(as.Date(paste0(max_year, "-", max_month, "-01")), "%b %Y")
    paste0("Occurrences up to ", formatted_date)
  } else {
    NULL
  }
  
  # 📛 Italic species name
  title_text <- if (!is.null(species_name)) {
    paste0("<i>", paste(strsplit(species_name, " ")[[1]], collapse = " "), "</i>")
  } else {
    ""
  }
  
  # 📊 Base plot
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
      plot.subtitle = element_text(hjust = 0.5, size = 13, family = "mont"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = background_colour, colour = NA),
      plot.margin = margin(10, 20, 20, 20)
    )
  
  # 🧾 Optional caption footer
  if (show_caption) {
    full_plot <- cowplot::plot_grid(
      base_plot,
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
    full_plot <- base_plot
  }
  
  return(full_plot)
}





animate_occurrences_over_time <- function(
    data,
    species_name = NULL,
    format = "mp4",  # "gif" or "mp4"
    speed = 1,
    point_colour = "dodgerblue3",
    point_size = 2.8,
    old_point_colour = "grey30"
) {
  # Libraries (ideally already loaded globally)
  library(ggplot2)
  library(gganimate)
  library(dplyr)
  library(rnaturalearth)
  library(sf)
  library(stringr)
  library(png)
  library(grid)
  
  # Prepare data
  data <- data %>%
    rename(
      decimalLatitude = `Latitude (WGS84)`,
      decimalLongitude = `Longitude (WGS84)`,
      year = `Start date year`
    ) %>%
    filter(is.na(`Occurrence status`) | tolower(`Occurrence status`) != "absent") %>%
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(year)) %>%
    mutate(year = as.numeric(year)) %>%
    arrange(year) %>%
    mutate(point_id = row_number())
  
  uk <- rnaturalearth::ne_countries(scale = "medium", country = "United Kingdom", returnclass = "sf")
  
  # Compute running total
  frame_data <- data %>%
    count(year, name = "records") %>%
    arrange(year) %>%
    mutate(running_total = cumsum(records),
           frame_label = paste0("Year: ", year, " • Total: ", running_total))
  
  map_data <- left_join(data, frame_data[, c("year", "frame_label")], by = "year")
  
  # File paths
  file_base <- str_replace_all(species_name, " ", "_")
  anim1_path <- paste0(file_base, "_anim1.", format)
  anim2_path <- paste0(file_base, "_anim2.", format)
  final_output <- paste0(file_base, "_final.", format)
  file_list_txt <- paste0(file_base, "_filelist.txt")
  
  # Create year-by-year animation
  p1 <- ggplot() +
    geom_sf(data = uk, fill = "grey90", colour = "black") +
    annotation_custom(
      rasterGrob(png::readPNG("TRPP.png"), interpolate = TRUE),
      xmin = -15, xmax = Inf, ymin = 60, ymax = 61.5
    ) +
    shadow_mark(past = TRUE, future = FALSE, alpha = 1, size = point_size * 0.7, colour = old_point_colour) +
    geom_point(
      data = map_data,
      aes(x = decimalLongitude, y = decimalLatitude, group = point_id),
      colour = point_colour, alpha = 0.9, size = point_size
    ) +
    coord_sf(xlim = c(-11, 2), ylim = c(49.5, 61), expand = FALSE) +
    labs(
      title = paste0("Spread of ", species_name),
      subtitle = "{closest_state}",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 14, base_family = "mont") +
    theme(
      plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 28, hjust = 0.5),
      axis.text = element_text(size = 10),
      plot.title.position = "plot",
      plot.caption = element_text(hjust = 0.5, size = 20, margin = margin(t = 10))
    ) +
    labs(caption = "Data source: NBN Atlas") +
    transition_states(states = frame_label, transition_length = 0, state_length = 1) +
    view_follow(fixed_x = TRUE, fixed_y = TRUE)
  
  animate(p1, renderer = av_renderer(anim1_path), width = 900, height = 1400, fps = 4 * speed, res = 300)
  
  # Create final map frame
  final_data <- map_data
  final_data$frame_label <- paste0("Current total records: ", nrow(map_data))
  
  p2 <- ggplot() +
    geom_sf(data = uk, fill = "grey90", colour = "black") +
    annotation_custom(
      rasterGrob(png::readPNG("TRPP.png"), interpolate = TRUE),
      xmin = -15, xmax = Inf, ymin = 60, ymax = 61.5
    ) +
    geom_point(
      data = final_data,
      aes(x = decimalLongitude, y = decimalLatitude, group = point_id),
      colour = old_point_colour, alpha = 0.9, size = point_size
    ) +
    coord_sf(xlim = c(-11, 2), ylim = c(49.5, 61), expand = FALSE) +
    labs(
      title = paste0("Spread of ", species_name),
      subtitle = "{closest_state}",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 14, base_family = "mont") +
    theme(
      plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 28, hjust = 0.5),
      axis.text = element_text(size = 10),
      plot.title.position = "plot",
      plot.caption = element_text(hjust = 0.5, size = 20, margin = margin(t = 10))
    ) +
    labs(caption = "Data source: NBN Atlas") +
    transition_states(states = frame_label, transition_length = 0, state_length = 20) +  # <-- longer pause
    view_follow(fixed_x = TRUE, fixed_y = TRUE)
  
  animate(p2, renderer = av_renderer(anim2_path), width = 900, height = 1400, fps = 5 * speed, res = 300, nframes = 10)
  
  # Write the concat file
  writeLines(c(
    paste0("file '", anim2_path, "'"),
    paste0("file '", anim1_path, "'")
  ), file_list_txt)
  
  # Run ffmpeg to merge
  system2("ffmpeg", args = c(
    "-y", "-f", "concat", "-safe", "0",
    "-i", file_list_txt,
    "-c", "copy",
    final_output
  ))
  
  # Clean up
  unlink(c(anim1_path, anim2_path, file_list_txt))
  
  cat("✅ Successfully created:", final_output, "\n")
}

