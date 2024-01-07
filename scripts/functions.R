# **********************
# ******** app.R *******
# **********************

# Function to download and extract data
download_and_extract_data <- function(link, year) {
  name <- paste0("data/data-rf-", year, ".zip") # Build file's name
  GET(link, write_disk(name, overwrite = TRUE))  # Download file
  unzip(zipfile = name, exdir = paste0("data/")) # Extract file's content
}

# Retrieve holkiday periods dates for a year
get_holidays <- function(year) {

  if(year == 2017){
    holidays <- list(
      # No data loaded for 2016
      # christmas2016 = seq(as.Date("2016-12-17"), as.Date("2017-01-02"), by = "day"),
      winter2017 = seq(as.Date("2017-02-18"), as.Date("2017-03-06"), by = "day"),
      spring2017 = seq(as.Date("2017-04-15"), as.Date("2017-05-01"), by = "day"),
      ascension2017 = seq(as.Date("2017-05-24"), as.Date("2017-05-29"), by = "day"),
      great_holiday2017 = seq(as.Date("2017-07-08"), as.Date("2017-09-03"), by = "day"),
      all_saints_day2017 = seq(as.Date("2017-10-21"), as.Date("2017-11-06"), by = "day"),
      christmas2017 = seq(as.Date("2017-12-23"), as.Date("2018-01-08"), by = "day")
    )
  } else if(year == 2018){
    holidays <- list(
      christmas2017 = seq(as.Date("2017-12-23"), as.Date("2018-01-08"), by = "day"),
      winter2018 = seq(as.Date("2018-02-04"), as.Date("2018-02-20"), by = "day"),
      spring2018 = seq(as.Date("2018-04-01"), as.Date("2018-04-18"), by = "day"),
      ascension2018 = seq(as.Date("2018-05-10"), as.Date("2018-05-10"), by = "day"),
      great_holiday2018 = seq(as.Date("2018-07-07"), as.Date("2018-09-02"), by = "day"),
      all_saints_day2018 = seq(as.Date("2018-10-20"), as.Date("2018-11-05"), by = "day"),
      christmas2018 = seq(as.Date("2018-12-22"), as.Date("2019-01-07"), by = "day")
    )
  } else if (year == 2019) {
    holidays <- list(
      christmas2018 = seq(as.Date("2018-12-22"), as.Date("2019-01-07"), by = "day"),
      winter2019 = seq(as.Date("2019-02-23"), as.Date("2019-03-11"), by = "day"),
      spring2019 = seq(as.Date("2019-04-20"), as.Date("2019-05-06"), by = "day"),
      ascension2019 = seq(as.Date("2019-05-29"), as.Date("2019-06-03"), by = "day"),
      great_holiday2019 = seq(as.Date("2019-07-06"), as.Date("2019-09-01"), by = "day"),
      all_saints_day2019 = seq(as.Date("2019-10-19"), as.Date("2019-11-04"), by = "day"),
      christmas2019 = seq(as.Date("2019-12-21"), as.Date("2020-01-06"), by = "day")
    )
  } else if (year == 2020) {
    holidays <- list(
      christmas2019 = seq(as.Date("2019-12-21"), as.Date("2020-01-06"), by = "day"),
      winter2020 = seq(as.Date("2020-02-08"), as.Date("2020-02-24"), by = "day"),
      spring2020 = seq(as.Date("2020-04-04"), as.Date("2020-04-20"), by = "day"),
      ascension2020 = seq(as.Date("2020-05-20"), as.Date("2020-05-25"), by = "day"),
      great_holiday2020 = seq(as.Date("2020-07-04"), as.Date("2020-08-30"), by = "day"),
      all_saints_day2020 = seq(as.Date("2020-10-17"), as.Date("2020-11-02"), by = "day"),
      christmas2020 = seq(as.Date("2020-12-19"), as.Date("2021-01-03"), by = "day")
    )
  } else if (year == 2021) {
    holidays <- list(
      christmas2020 = seq(as.Date("2020-12-19"), as.Date("2021-01-03"), by = "day"),
      winter2021 = seq(as.Date("2021-02-13"), as.Date("2021-02-28"), by = "day"),
      spring2021 = seq(as.Date("2021-04-10"), as.Date("2021-04-26"), by = "day"),
      ascension2021 = seq(as.Date("2021-05-12"), as.Date("2021-05-17"), by = "day"),
      great_holiday2021 = seq(as.Date("2021-07-03"), as.Date("2021-08-29"), by = "day"),
      all_saints_day2021 = seq(as.Date("2021-10-23"), as.Date("2021-11-07"), by = "day"),
      christmas2021 = seq(as.Date("2021-12-18"), as.Date("2022-01-02"), by = "day")
    )
  } else if (year == 2022) {
    holidays <- list(
      christmas2021 = seq(as.Date("2021-12-18"), as.Date("2022-01-02"), by = "day"),
      winter2022 = seq(as.Date("2022-02-18"), as.Date("2022-03-06"), by = "day"),
      spring2022 = seq(as.Date("2022-04-15"), as.Date("2022-05-01"), by = "day"),
      ascension2022 = seq(as.Date("2022-05-25"), as.Date("2022-05-30"), by = "day"),
      great_holiday2022 = seq(as.Date("2022-07-09"), as.Date("2022-09-04"), by = "day"),
      all_saints_day2022 = seq(as.Date("2022-10-22"), as.Date("2022-11-07"), by = "day"),
      christmas2022 = seq(as.Date("2022-12-24"), as.Date("2023-01-09"), by = "day")
    )
  } else { # year == 2023
    
    holidays <- list(
      # No data available for second semester and 2024
      christmas2022 = seq(as.Date("2022-12-24"), as.Date("2023-01-08"), by = "day"),
      winter2023 = seq(as.Date("2023-02-18"), as.Date("2023-03-06"), by = "day"),
      spring2023 = seq(as.Date("2023-04-15"), as.Date("2023-04-30"), by = "day"),
      ascension2023 = seq(as.Date("2023-05-17"), as.Date("2023-05-22"), by = "day")
      #great_holiday2023 = seq(as.Date("2023-07-08"), as.Date("2023-09-03"), by = "day"),
      #all_saints_day2023 = seq(as.Date("2023-10-21"), as.Date("2023-11-06"), by = "day"),
      #christmas2023 = seq(as.Date("2023-12-23"), as.Date("2024-01-08"), by = "day")
    )
  }
}

# Convert holiday list to dataframe
convert_holidays <- function(holidays_list) {
  holidays_df <- data.frame(
    start_date = as.Date(character(0)),
    end_date = as.Date(character(0)),
    label = character(0),
    stringsAsFactors = FALSE
  )
  
  for (holiday_name in names(holidays_list)) {
    date_range <- holidays_list[[holiday_name]]
    start_date <- min(date_range)
    end_date <- max(date_range)
    label <- gsub("[0-9]", "", gsub("_", " ", holiday_name))
    
    holidays_df <- rbind(holidays_df, data.frame(start_date, end_date, label))
  }
  
  return(holidays_df)
}

# **********************
# ***** get_data.R *****
# **********************

# Function to read and encode images
read_and_encode <- function(file_path) {
  file_data <- readBin(file_path, "raw", file.info(file_path)$size)
  base64encode(file_data)
}