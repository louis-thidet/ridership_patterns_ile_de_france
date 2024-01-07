library(httr)
library(dplyr)
source("scripts/functions.R")

# ******************************************************************************
# **** Data set 1: validation per day and stop point first semester of 2023 ****
# ******************************************************************************

GET("https://data.iledefrance-mobilites.fr/api/explore/v2.1/catalog/datasets/validations-reseau-ferre-nombre-validations-par-jour-1er-semestre/exports/csv", 
    write_disk("data/2023_S1_NB_FER.csv", overwrite = TRUE))

# ******************************************************************************
# ********* Data set 2: validation per day and stop point 2015 – 2022 **********
# ******************************************************************************

GET("https://data.iledefrance-mobilites.fr/api/explore/v2.1/catalog/datasets/histo-validations-reseau-ferre/exports/csv", 
    write_disk("data/val_per_day_and_stop_point_2015-2022.csv", overwrite = TRUE))

donnees <- read.csv("data/val_per_day_and_stop_point_2015-2022.csv", sep = ";")

donnees <- donnees %>%
  filter(as.numeric(annee) >= 2017)

# Function to download and extract data
download_and_extract_data <- function(link, year) {
  name <- paste0("data/data-rf-", year, ".zip") # Build file's name
  GET(link, write_disk(name, overwrite = TRUE))  # Download file
  unzip(zipfile = name, exdir = paste0("data/")) # Extract file's content
}

# Iterate the use of the function for each row
for (i in seq(nrow(donnees))) {
  link <- donnees[i, "reseau_ferre"]
  year <- donnees[i, "annee"]
  
  download_and_extract_data(link, year)
  
  # Fix eventual wrong file names (like for 2017)
  name_s1 <- file.path(paste0("data/data-rf-", year),  paste0(year,"S1_NB_FER.txt"))
  fixed_name_s1 <- file.path(paste0("data/data-rf-", year),  paste0(year,"_S1_NB_FER.txt"))
  
  name_s2 <- file.path(paste0("data/data-rf-", year),  paste0(year,"S2_NB_FER.txt"))
  fixed_name_s2 <- file.path(paste0("data/data-rf-", year),  paste0(year,"_S2_NB_FER.txt"))
  
  if (file.exists(name_s1)) {
    file.rename(name_s1, fixed_name_s1)
  }
  if (file.exists(name_s2)) {
    file.rename(name_s2, fixed_name_s2)
  }
}

# Remove the useless files

files_to_remove <- list.files("data/", pattern = "^data-rf-.*\\.zip$", full.names = TRUE)
file.remove(files_to_remove)

# ******************************************************************************
# ************** Data set 3:  stops i.e “Zone d’arrêt” locations ***************
# ******************************************************************************

GET("https://data.iledefrance-mobilites.fr/api/explore/v2.1/catalog/datasets/zones-d-arrets/exports/csv", 
    write_disk("data/zone-arret-locations.csv", overwrite = TRUE))

# Download spatial file:

GET("https://eu.ftp.opendatasoft.com/stif/Reflex/REF_ZdA.zip", 
    write_disk("geom/REF_ZdA.zip", overwrite = TRUE))

unzip(zipfile = "geom/REF_ZdA.zip", exdir = "geom/")

file.remove("geom/REF_ZdA.zip")
