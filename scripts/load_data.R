library(data.table)
library(dplyr)
library(sf)

# If the formatted data tables were already loaded once, they can be loaded quickly,
# because they've been saved into .rds files.
# If the formatted data files weren't loaded, they weren't saved into .rds files,
# so we run the load_data.R script.

path1 <- "data/vpd_aggregated_per_day_and_stop_area.rds"
path2 <- "data/stop_area_list.rds"
path3 <- "data/stop_area_locations.rds"

if (file.exists(path1) && file.exists(path2) && file.exists(path3)) {
  
  vpd_aggregated <- readRDS(path1)
  stop_area_list <- readRDS(path2)
  stop_area_locations <- readRDS(path3)
  
  rm(path1)
  rm(path2)
  rm(path3)
  
} else {
  
  # ***********************************************************************
  # ****************** Load the "zone d'arrêt" locations ******************
  # ***********************************************************************
  
  stop_area_locations <- fread("data/zone-arret-locations.csv")
  spatial_data <- st_read("geom/PL_ZDL_R_07_01_2024.shp") # shapefile
  stop_area_locations <- merge(spatial_data, stop_area_locations, by.x = "id_refa", by.y = "zdaid", all.x = TRUE)
  
  # Keep the relevant columns
  stop_area_locations <- stop_area_locations[, c("zdapostalregion", "zdatown", "nom", "type_arret", "idrefa_lda", "geometry")]
  
  # Get rid of bus stations
  stop_area_locations <- stop_area_locations %>%
    filter(type_arret != "Arrêt de bus")
  
  rm(spatial_data)
  
  # ***********************************************************************
  # **** Load validation per day and stop point first semester of 2023 ****
  # ***********************************************************************
  
  vpd_s1_2023 <- fread("data/2023_S1_NB_FER.csv")
  
  names(vpd_s1_2023) <- c("JOUR", "CODE_STIF_TRNS", "CODE_STIF_RES", 
                          "CODE_STIF_ARRET", "LIBELLE_ARRET", "ID_REFA_LDA", 
                          "CATEGORIE_TITRE", "NB_VALD") # Fix column names
  
  vpd_s1_2023$JOUR <- as.Date(vpd_s1_2023$JOUR, format = "%Y-%m-%d")
  vpd_s1_2023$JOUR <- format(vpd_s1_2023$JOUR, "%d/%m/%Y")
  
  # ***********************************************************************
  # ****** Load validation per day and stop point from 2017 to 2022 *******
  # ***********************************************************************
  
  # Count how many files have to be loaded
  donnees <- read.csv("data/val_per_day_and_stop_point_2015-2022.csv", sep = ";") 
  donnees <- donnees %>%
    filter(as.numeric(annee) >= 2017)
  
  # Load data of all years
  for (i in seq(nrow(donnees))) {
    year <- donnees[i, "annee"]
    
    # Load data file of each semester 
    for (j in 1:2) {
      if (j == 1) {
        file_name <- file.path(paste0("data/data-rf-", year),  paste0(year,"_S1_NB_FER.txt"))
        df_name <- paste0("vpd_s1_", year)
      } else {
        file_name <- file.path(paste0("data/data-rf-", year),  paste0(year,"_S2_NB_FER.txt"))
        df_name <- paste0("vpd_s2_", year)
      }
      
      # Load data into the dynamically named object
      assign(df_name, fread(file_name))
      setnames(get(df_name), c("JOUR", "CODE_STIF_TRNS", "CODE_STIF_RES", 
                               "CODE_STIF_ARRET", "LIBELLE_ARRET", "ID_REFA_LDA", 
                               "CATEGORIE_TITRE", "NB_VALD")) # Fix column names
    }
  }
  
  # ***********************************************************************
  # ********* Combine all the data tables to make one data frame **********
  # ***********************************************************************
  
  table_names <- paste0("vpd_s", c("1_2017", "2_2017", "1_2018", "2_2018", 
                                   "1_2019", "2_2019", "1_2020", "2_2020", 
                                   "1_2021", "2_2021", "1_2022", "2_2022", 
                                   "1_2023"))
  
  vpd_combined <- rbindlist(lapply(table_names, get))
  
  for (table_name in table_names) {
    if (exists(table_name)) {
      rm(list = table_name)
    }
  }
  
  # ***********************************************************************
  # ************************ Clean the data frame *************************
  # ***********************************************************************
  
  # Formating columns into proper types
  vpd_combined$JOUR <- as.Date(vpd_combined$JOUR, format = "%d/%m/%Y")
  vpd_combined$CODE_STIF_TRNS <- as.numeric(vpd_combined$CODE_STIF_TRNS)
  vpd_combined$CODE_STIF_RES <- as.numeric(vpd_combined$CODE_STIF_RES)
  vpd_combined$CODE_STIF_ARRET <- as.numeric(vpd_combined$CODE_STIF_ARRET)
  vpd_combined$LIBELLE_ARRET <- as.character(vpd_combined$LIBELLE_ARRET)
  vpd_combined$ID_REFA_LDA <- as.character(vpd_combined$ID_REFA_LDA)
  # vpd_combined$CATEGORIE_TITRE <- as.character(vpd_combined$CATEGORIE_TITRE)
  vpd_combined$NB_VALD <- as.numeric(vpd_combined$NB_VALD)
  
  # Checking if there is any NA
  if (anyNA(vpd_combined)) {
    vpd_combined <- na.omit(vpd_combined) # Removing existing NA
  }
  
  # Aggregating per stop area and day
  vpd_aggregated <- vpd_combined %>%
    group_by(JOUR, LIBELLE_ARRET, ID_REFA_LDA) %>%
    summarize(NB_VALD = sum(NB_VALD)) %>%
    ungroup() %>%
    as.data.frame()
  
  # Get rid of non existing ID_REFA_LDA
  vpd_aggregated <- subset(vpd_aggregated, ID_REFA_LDA != "")
  vpd_aggregated <- subset(vpd_aggregated, ID_REFA_LDA != "0")
  vpd_aggregated <- subset(vpd_aggregated, ID_REFA_LDA != "?")
  
  # Get rid of useless location data
  stop_area_locations <- stop_area_locations[stop_area_locations$idrefa_lda %in% vpd_aggregated$ID_REFA_LDA, ]
  
  # Group the stations with the same ID
  stop_area_locations <- stop_area_locations %>%
    group_by(nom, idrefa_lda) %>%
    summarize(geometry = st_union(geometry))
  
  # Fixing projection
  stop_area_locations <- st_transform(stop_area_locations, crs = st_crs("+proj=longlat +datum=WGS84"))
  
  # Create latitude and longitude columns
  centroids <- st_centroid(stop_area_locations)
  stop_area_locations$latitude <- st_coordinates(centroids)[, "Y"]
  stop_area_locations$longitude <- st_coordinates(centroids)[, "X"]
  
  rm(centroids)
  
  # Put area locations names to uppercase
  stop_area_locations$nom <- toupper(gsub("[éè]", "E", stop_area_locations$nom))
  stop_area_locations$nom <- gsub("Ç", "C", stop_area_locations$nom)
  stop_area_locations$nom <- gsub(" - ", "-", stop_area_locations$nom)
  
  # Created to save time when loading the Stop Area List in the Shiny App
  stop_area_list <- unique(sort(vpd_aggregated[, "LIBELLE_ARRET"]))
  
  # Saving data tables
  saveRDS(vpd_aggregated, file = "data/vpd_aggregated_per_day_and_stop_area.rds")
  saveRDS(stop_area_list, file = "data/stop_area_list.rds")
  saveRDS(stop_area_locations, file = "data/stop_area_locations.rds")
  #saveRDS(vpd_combined, file = "data/vpd_combined.rds")
}
