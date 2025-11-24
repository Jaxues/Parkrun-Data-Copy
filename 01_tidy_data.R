# This script returns a zipped csv file of the tidy Parkrun data, including imputation and week numbers

# getting cleaned dataset from 00_cleaned_dataset
source("00_cleaned_dataset.R")

# install packages if necessary
packages <- c("tidyverse", "stringr", "mice", "rvest", "lubridate")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# loading packages
library(tidyverse)
library(mice)
library(rvest)

# parkrun_tidy already exists from source()

# selecting and renaming variables as per sow
tidy_data <- parkrun_all |>
  select(
    runner_id, filenum, park, gender, age_median, club,
    time_seconds, pb_seconds, age_grade
  ) |>
  rename(
    id       = runner_id,
    parkrun  = park,
    sex      = gender,
    age      = age_median,
    time     = time_seconds
  ) |>
  mutate(
    parkrun = as.factor(parkrun),
    sex = as.factor(sex),
    club = as.factor(club),
    time = as.numeric(time),
    pb_seconds = as.numeric(pb_seconds),
    age_grade = as.numeric(age_grade),
    age = as.numeric(age)
  )


if (!file.exists("intermediate/parkrun_weeks.csv")) {
  print("Scraping local HTML files")

  # Getting parkrun URLS for pages to download

  urls <- list.files("dataset") %>%
    gsub("_", "/", .) %>%
    as.character() %>%
    paste0("www.parkrun.co.nz/", .)



  # Scraping week from parkrun html

  html_files <- list.files("parkrun_html") %>% paste0("parkrun_html/", .)

  base_date <- as.Date(dmy("1/1/2025"))

  week_tibble <- tibble(week = numeric())

  parkrun_id <- list.files("dataset") %>%
    str_extract("(\\d+$)") %>%
    as.integer() %>%
    tibble(filenum = .)



  # Code for scrapping all HTML pages stored locally

  for (file in html_files) {
    page_date <- read_html(file) %>%
      html_element(".Results") %>%
      html_element(".format-date") %>%
      html_text() %>%
      dmy() %>%
      as.Date()

    # Calculate week number for date of parkrun

    week <- (as.numeric(difftime(page_date, base_date)) %/% 7) + 1

    temp_tibble <- tibble(
      week = week
    )

    week_tibble <- bind_rows(week_tibble, temp_tibble)
  }

  week_tibble <- bind_cols(week_tibble, parkrun_id)

  # Define correct dimensions for tidy data

  tidy_data <- left_join(tidy_data, week_tibble, by = "filenum") %>%
    select(id, week, parkrun, sex, age, club, time, pb_seconds, age_grade)
  # Create csv of weeks to not have to run code after every restart

  write_csv(week_tibble, "intermediate/parkrun_weeks.csv")
}


if ("filenum" %in% colnames(tidy_data)) {
  # Get correct week number for parkruns without having HTML for pages
  # print("Using parkrun_week csv")
  # Read data known for parkrun id and week
  week_tibble <- read_csv("intermediate/parkrun_weeks.csv")
  # Joined data from filenum by parkrun id
  tidy_data <- left_join(tidy_data, week_tibble, by = "filenum") %>%
    select(id, week, parkrun, sex, age, club, time, pb_seconds, age_grade)
}

# Mice Imputation for missing values
set.seed(123)

meth <- make.method(tidy_data)
pred <- make.predictorMatrix(tidy_data)

# Do NOT impute id or club, and do NOT use them to predict others
meth["id"] <- ""
meth["club"] <- ""
pred["id", ] <- 0
pred["club", ] <- 0
pred[, "id"] <- 0
pred[, "club"] <- 0

# Identify numeric vars and set PMM for those (time, age, week, etc.)
num_vars <- names(tidy_data)[vapply(tidy_data, is.numeric, logical(1))]
meth[num_vars] <- "pmm"

imp <- mice(
  tidy_data,
  m = 5,
  maxit = 10,
  method = meth,
  predictorMatrix = pred,
  printFlag = FALSE
)

# get one completed dataset
tidy_data_imputed <- complete(imp, 1)

# to filter out the added variables for a data set with variables from SOW
tidy_data_final <- tidy_data_imputed |> select(
  id, week,
  parkrun, sex, age, club, time
)
if (!file.exists("final_parkrun_dataset.csv") || !file.exists("final_parkrun_dataset.zip")) {
  print("Creating final parkrun csv and zip")
  write_csv(tidy_data_final, "final_parkrun_dataset.csv")
  zip(zipfile = "final_parkrun_dataset.zip", files = "final_parkrun_dataset.csv")
}
