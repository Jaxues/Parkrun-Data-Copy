# This code returns a tibble for a csv file of all the variables extracted from the dataset folder for analysis and as a base for the clean csv zip file
# Local access of the parkrun_all tibble can be used for all the parkrun data extracted from the files

# install packages
packages <- c("tidyverse", "digest","stringr")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# loading packages
library(tidyverse)
library(digest) # for hashing names

# age group regex pattern
ag_code_rx <- "(?:J[MW](?:10|11-14|15-17)|S[MW](?:18-19|20-24|25-29|30-34)|V[MW](?:35-39|40-44|45-49|50-54|55-59|60-64|65-69|70-74|75-79|80-84|85-89))"

# extract file name into park and filename
parse_park_file <- function(file_path) {
  fn <- basename(file_path)
  park <- str_match(fn, "^(.*?)[_-]?results")[, 2]
  if (is.na(park)) park <- str_split(fn, "[-_]", simplify = TRUE)[, 1]
  park <- park |>
    str_replace_all("_", " ") |> # reformatting values
    str_to_lower()
  filenum <- str_match(fn, "([0-9]+)$")[, 2] |> as.integer()
  list(park = park, filenum = filenum) # make into a list
}

# compute integer median age
median_age_int <- function(band) {
  if (is.na(band)) {
    return(NA_integer_) # changing to NA integer value, if no age band present
  }
  if (str_detect(band, "-")) {
    nums <- as.numeric(str_split(band, "-", simplify = TRUE)) # if there is an age band, find the middle value
    return(as.integer(round(mean(nums))))
  } else {
    return(suppressWarnings(as.integer(band))) # otherwise, supress warnings
  }
}

data_dir <- "dataset" # dataset folder
files <- list.files(data_dir, full.names = TRUE) # get list of filenames

# lapply runs the function for every element in the file list
df_list <- lapply(files, function(path) {
  pfil <- parse_park_file(path)

  # read file and change variables to not include spaces
  raw <- read_csv(path, na = c("", "NA"), show_col_types = FALSE)
  if ("Age Group" %in% names(raw)) raw <- raw |> rename(age_group = `Age Group`)
  if (!"age_group" %in% names(raw)) raw <- raw |> mutate(age_group = NA_character_)

  # add elements from file name to tibble
  tibble(
    park = pfil$park,
    filenum = pfil$filenum,
    Position = suppressWarnings(as.integer(raw$Position)), # add position number to tibble

    # use regex to extract values from raw data (name, run count, gender, gender position)
    name = str_match(raw$parkrunner, "^(.*?)\\s*\\d+\\s*parkrun")[, 2] |> str_squish(),
    runs_count = str_match(raw$parkrunner, "(\\d+)\\s*parkrun")[, 2] |> as.integer(),
    gender = str_match(raw$Gender, "\\b(Male|Female)\\b")[, 2],
    gender_position = str_extract(raw$Gender, "\\d+") |> as.integer(),

    # set age categories to text and extract age group and age grade
    age_group_code = str_match(raw$age_group, ag_code_rx)[, 1],
    age_grade = str_match(raw$age_group, "(\\d{1,2}\\.\\d{2})\\s*%")[, 2] |> as.numeric(),
    category = case_when(
      str_detect(age_group_code, "^SW") ~ "Women Senior",
      str_detect(age_group_code, "^SM") ~ "Men Senior",
      str_detect(age_group_code, "^VW") ~ "Women Veteran",
      str_detect(age_group_code, "^VM") ~ "Men Veteran",
      str_detect(age_group_code, "^JW") ~ "Girls Junior",
      str_detect(age_group_code, "^JM") ~ "Boys Junior",
      TRUE ~ NA_character_
    ),
    # extract age category and use age median function to get median ages
    age_category = str_extract(age_group_code, "\\d{2}(?:-\\d{2})?"),
    age_median = vapply(age_category, median_age_int, integer(1)),
    club = if_else(!is.na(raw$Club) & raw$Club != "", raw$Club, NA_character_),

    # extract times (time, time in seconds, pb time and pb in seconds)
    official_time_raw = str_match(raw$Time, "(?<!\\d)\\d{1,2}:\\d{2}(?::\\d{2})?(?!\\d)")[, 1], # getting raw data with regex

    official_time = case_when(
      is.na(official_time_raw) ~ NA_character_, # dealing with NA
      str_count(official_time_raw, ":") == 2 ~ official_time_raw,
      as.integer(str_split_fixed(official_time_raw, ":", 2)[, 1]) >= 14 ~ # boolean for separating hrs and min times
        paste0("00:", official_time_raw),
      TRUE ~ paste0(official_time_raw, ":00") # converting format
    ),
    time_seconds = if_else(
      !is.na(official_time),
      as.integer(lubridate::period_to_seconds(lubridate::hms(official_time))), # calculating time to seconds
      NA_integer_
    ),


    # pb time: allow MM:SS or H:MM:SS, normalize, and seconds
    pb_time_raw = str_match(raw$Time, "(?i)PB\\s*(\\d{1,2}:\\d{2}(?::\\d{2})?)")[, 2], # extracting time
    pb_time = case_when(
      is.na(pb_time_raw) ~ NA_character_,
      str_detect(pb_time_raw, "^\\d{1,2}:\\d{2}$") ~ paste0("00:", pb_time_raw), # separating over and under 1hr times
      TRUE ~ pb_time_raw
    ),
    pb_seconds = if_else(
      !is.na(pb_time),
      as.integer(lubridate::period_to_seconds(lubridate::hms(pb_time))), # calculating time in seconds
      NA_integer_
    )
  )
})

# combine the df list into one tibble
parkrun_all <- bind_rows(df_list) |> select(-pb_time_raw, -official_time_raw)


####################################### HASHING NAMES ##########################################################

# using the md5 algorithm and a salt to annonymise names and add to the dataframe
algo <- "md5"
salt <- "data201" # keep constant across runs

parkrun_all <- parkrun_all |>
  mutate(
    # Normalize names for consistency
    .norm_name = str_squish(str_to_lower(name)),

    # Add stable runner_id
    runner_id = sapply(.norm_name, function(x) {
      if (is.na(x)) {
        return(NA_character_)
      }
      digest(paste0(salt, x), algo = algo, serialize = FALSE)
    })
  ) |>
  select(-.norm_name) |>
  relocate(runner_id, .before = name)

# write all results to file (as test)
write_csv(parkrun_all, "parkrun_all.csv")

# write.csv(parkrun_all, gzfile("parkrun_results.csv.gz"), row.names = FALSE) <- zipped
# 