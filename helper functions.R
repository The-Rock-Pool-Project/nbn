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
  
  # 🧠 Rename fields for consistency
  data <- data %>%
    rename(
      decimalLatitude = `Latitude (WGS84)`,
      decimalLongitude = `Longitude (WGS84)`,
      year = `Start date year`
    )
  
  # ✅ Filter valid presence data
  map_data <- data %>%
    filter(is.na(`Occurrence status`) | tolower(`Occurrence status`) != "absent") %>%
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(year)) %>%
    mutate(year = as.numeric(year)) %>%
    arrange(year) %>%
    mutate(point_id = row_number())
  
  # 🗺️ UK map
  uk <- rnaturalearth::ne_countries(scale = "medium", country = "United Kingdom", returnclass = "sf")
  
  # 📊 Compute running total
  frame_data <- map_data %>%
    count(year, name = "records") %>%
    arrange(year) %>%
    mutate(
      running_total = cumsum(records),
      frame_label = paste0("Year: ", year, "   •   Total records: ", running_total)
    )
  
  # Join labels back
  map_data <- left_join(map_data, frame_data[, c("year", "frame_label")], by = "year")
  
  # ⏸️ Optional pause at end
  if (pause_at_end) {
    final_label <- tail(unique(map_data$frame_label), 1)
    map_data <- bind_rows(
      map_data,
      map_data %>% filter(frame_label == final_label) %>% slice(rep(1:n(), 10))
    )
  }
  
  # 🧭 Build plot
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
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 14, base_family = "mont") +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 10),
      plot.margin = margin(20, 20, 40, 20)
    ) +
    transition_states(states = frame_label, transition_length = 1, state_length = 2) +
    ease_aes('linear')
  
  # 🎬 Animation settings
  nframes <- length(unique(map_data$frame_label)) * 10
  fps <- 10 * speed
  filename <- str_replace(filename, "\\.gif$|\\.mp4$", paste0(".", format))
  renderer <- if (format == "mp4") av_renderer(filename) else gifski_renderer()
  
  # 🔄 Animate
  anim <- animate(p, renderer = renderer, width = 900, height = 700, fps = fps, nframes = nframes)
  
  if (format == "gif") {
    anim_save(filename, animation = anim)
    
    # 🖼️ Add logo for GIFs
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
