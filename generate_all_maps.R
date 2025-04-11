# Load dependencies
library(dplyr)
library(readr)
library(stringr)
source("helper functions.R")

# Load your species list
species_list <- read_csv("UK Marine NNS.csv")

# Create output folders
dir.create("outputs/static", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/animated", recursive = TRUE, showWarnings = FALSE)

records_per_year <- tibble()  # Empty dataframe to collect summary stats

# Loop through each species
for (i in seq_len(nrow(species_list))) {
  sci_name <- species_list$Taxon_name[i]
  file_base <- str_replace_all(sci_name, " ", "_")
  
  cat("\n🔍 Processing:", sci_name, "\n")
  
  # Query NBN
  occ <- nbn_occ_dat(sci_name)
  
  if (is.null(occ)) {
    cat("⚠️ No data found, skipping...\n")
    next
  }
  
  # Summarise records per year
  year_summary <- occ %>%
    filter(!is.na(year)) %>%
    group_by(year) %>%
    summarise(n_records = n(), .groups = "drop") %>%
    mutate(species_name = sci_name)
  
  records_per_year <- bind_rows(records_per_year, year_summary)
  
  # STATIC MAP
  static_path <- file.path("outputs/static", paste0(file_base, ".png"))
  ragg::agg_png(static_path, width = 700, height = 1000, res = 150)
  print(plot_occurrences_map(occ, species_name = sci_name, max_year = max(occ$year, na.rm = TRUE), max_month = 12))
  dev.off()
  cat("✅ Static map saved to", static_path, "\n")
  
  # ANIMATED MAP - GIF
  gif_path <- file.path("outputs/animated", paste0(file_base, ".gif"))
  animate_occurrences_over_time(occ,
                                species_name = sci_name,
                                filename = gif_path,
                                format = "gif",
                                speed = 1,
                                point_colour = "#DA3737",
                                point_size = 4.5,
                                old_point_colour = "#0E6BFF")
  
  # ANIMATED MAP - MP4
  mp4_path <- file.path("outputs/animated", paste0(file_base, ".mp4"))
  animate_occurrences_over_time(occ,
                                species_name = sci_name,
                                filename = mp4_path,
                                format = "mp4",
                                speed = 1,
                                point_colour = "#DA3737",
                                point_size = 4.5,
                                old_point_colour = "#0E6BFF")
}


# Build species-year summary

# Start with full species list
species_base <- species_list %>%
  select(species_name = Taxon_name) %>%
  mutate(across(everything(), as.character))

# Pivot to wide format (species x year)
records_per_year <- records_per_year %>%
  mutate(year = as.integer(year))

records_wide <- records_per_year %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = n_records,
    values_fill = 0
  )

# Merge with species list (to include 0-record species)
summary_table <- left_join(species_base, records_wide, by = "species_name")
summary_table[is.na(summary_table)] <- 0

# First + last year with records
year_cols <- summary_table %>%
  select(-species_name) %>%
  select(where(is.numeric)) %>%
  names() %>%
  sort()

summary_table <- summary_table %>%
  rowwise() %>%
  mutate(
    First_year = min(as.numeric(year_cols)[c_across(all_of(year_cols)) > 0], na.rm = TRUE),
    Last_year = max(as.numeric(year_cols)[c_across(all_of(year_cols)) > 0], na.rm = TRUE),
    Total_records = sum(c_across(all_of(year_cols)))
  ) %>%
  ungroup() %>%
  relocate(Total_records, First_year, Last_year, .after = species_name) %>%
  select(species_name, Total_records, First_year, Last_year, all_of(year_cols))

# Save output
write_csv(summary_table, "outputs/records_per_species_year.csv")
cat("📊 Complete summary saved to outputs/records_per_species_year.csv\n")
