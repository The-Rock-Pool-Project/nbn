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
  
  occ <- subset(occ, `Occurrence status` == "present")
  
  if (nrow(occ) == 0) {
    cat("⚠️ No data found, skipping...\n")
    next
  }
  
  # Summarise records per year
  year_summary <- occ %>%
    filter(!is.na(`Start date year`)) %>%
    group_by(`Start date year`) %>%
    summarise(n_records = n(), .groups = "drop") %>%
    mutate(species_name = sci_name)
  
  records_per_year <- bind_rows(records_per_year, year_summary)
  
  #last record date
  date_obj <- as.Date(max(occ$`Start date`, na.rm = T))
  
  last_year <- as.numeric(format(date_obj, "%Y"))   # 2025
  last_month <- as.numeric(format(date_obj, "%m"))  # 3
  
  
  # STATIC MAP
  static_path <- file.path("outputs/static", paste0(file_base, ".png"))
  ragg::agg_png(static_path, width = 700, height = 1000, res = 150)
  print(plot_occurrences_map(occ, species_name = sci_name, max_year = last_year, max_month = last_month))
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


# ✅ 1. Ensure you're working with the correct object
# You define `records_per_year` and mutate it, but then pivot `records_per_year1` (possibly a typo)

# FIX: Ensure you're using the correct dataframe name throughout
records_per_year <- records_per_year %>%
  mutate(year = as.integer(`Start date year`))


records_wide <- records_per_year %>%
  mutate(year = as.integer(`Start date year`)) %>%
  select(species_name, year, n_records) %>%
  distinct() %>%  # 💡 ensure uniqueness
  tidyr::pivot_wider(
    names_from = year,
    values_from = n_records,
    values_fill = list(n_records = 0)
  )


# ✅ 4. Start with full species list
species_base <- species_list %>%
  select(species_name = Taxon_name) %>%
  mutate(across(everything(), as.character))

# ✅ 5. Merge with full species list
summary_table <- left_join(species_base, records_wide, by = "species_name")
summary_table[is.na(summary_table)] <- 0

# ✅ 6. Identify year columns
year_cols <- summary_table %>%
  select(-species_name) %>%
  select(where(is.numeric)) %>%
  names() %>%
  sort()

# ✅ 7. Add Total, First, Last year columns

all_res <- list()

for (i in species_list$Taxon_name) {
  
  sp_Dat <- subset(records_wide, species_name == i)
  
  if(nrow(sp_Dat) == 0){
    sp_Dat <- rep(0, length(year_cols))
    names(sp_Dat) <- year_cols
  }else{
    sp_Dat <- unlist(sp_Dat)[year_cols]  
  }
  
  
  total_records <- sum(as.numeric(sp_Dat))
  
  sp_Dat_pres <- sp_Dat[!sp_Dat == 0]
  
  first_year <- min(names(sp_Dat_pres))
  
  last_year <- max(names(sp_Dat_pres))
  
  all_res[[i]] <- c(i,total_records, first_year, last_year, sp_Dat)
  
  
}

allres_tab <- do.call("rbind", all_res)
allres_tab <- as.data.frame(allres_tab)
names(allres_tab)[1:4] <- c("species_name",	"Total_records",	"First_year",	"Last_year")

# Save output
write_csv(allres_tab, "outputs/records_per_species_year.csv")
cat("📊 Complete summary saved to outputs/records_per_species_year.csv\n")
