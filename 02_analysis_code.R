# This script codes for visualisations to help answer each of Puma's 6 key questions

# To install packages if not already installed
packages <- c("tidyverse", "reshape2", "geosphere", "paletteer")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
# Load necessary packages
library(tidyverse)
library(reshape2)
library(geosphere)
library(paletteer)
pal <- paletteer_d("MetBrewer::Derain") # the palette used for visualisations

# Read the cleaned data
source("01_tidy_data.R")
df <- read_csv("final_parkrun_dataset.csv")
#-------------------------------------------------------------------------------
# Start of Analysis for Question 1

# Table mean attendance
counts_week <- df %>%
  select(week, parkrun) %>%
  count(week, parkrun) %>%
  group_by(parkrun) %>%
  summarise(mean_attendance = round(mean(n))) %>%
  arrange(desc(mean_attendance))


# Which Parkrun provides the best marketing opportunity with the largest number of people ? Parkrun with mean largest amount of people

q1_graph <- df %>%
  filter(!is.na(id)) %>% # Remove any rows with missing id
  count(parkrun) %>% # Count total runners per parkrun
  ggplot(aes(x = reorder(parkrun, -n), y = n)) +
  geom_col(fill = "#454A74FF") +
  geom_text(
    aes(label = n),
    vjust = -0.3, # move labels above bar tops
    size = 3.5 # adjust font size if needed
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Parkrun",
    y = "Total runners",
    title = "Total participants per Parkrun"
  ) +
  theme_minimal()

#-------------------------------------------------------------------------------
# Start of Analysis for Question 2
# Do new people come each week or is it always the same people?

# Drop rows without id, as we cannot track runners without an id
df <- df %>% filter(!is.na(id))

# Sort the data by week
df <- df %>% arrange(week)

# Keep track of runners who have already appeared in previous weeks
seen_ids <- c()

# Store weekly counts of new, returning, and total runners
results <- data.frame()

# Loop through each week to categorize runners
for (w in unique(df$week)) {
  week_ids <- unique(df$id[df$week == w]) # Get the unique runner ids for each week
  new <- setdiff(week_ids, seen_ids) # Count new runners
  returning <- intersect(week_ids, seen_ids) # Count returning runners

  # Save counts for each week into the 'results' data frame
  results <- rbind(results, data.frame(
    week = w,
    new_runners = length(new),
    returning_runners = length(returning),
    total_runners = length(week_ids)
  ))

  # Update list of seen runners with the corresponding week
  seen_ids <- union(seen_ids, week_ids)
}

# Calculate the proportion of new vs returning each week
results <- results %>%
  mutate(
    percent_new = new_runners / total_runners * 100,
    percent_returning = returning_runners / total_runners * 100
  )

# Visualise the trend and pivot data to long format for plotting
results_long <- results %>%
  tidyr::pivot_longer(
    cols = c("new_runners", "returning_runners"),
    names_to = "Type", values_to = "count"
  )

# Change the week variable into categorical
results_long$week <- factor(results_long$week)

# Plot 100% stacked bar chart of new vs returning runners each week
q2_graph <- ggplot(results_long, aes(x = week, y = count, fill = Type)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(
    values = c(
      # Manually set colors for new and returning runners
      "new_runners" = "#97C684FF",
      "returning_runners" = "#6F9969FF"
    ),
    labels = c("New Runners", "Returning Runners")
  ) +
  labs(
    title = "Proportion of New vs Returning Runners Each Week", # Main plot title
    x = "Week of the Year", # x-axis label
    y = "Proportion of Runners" # y-axis label
  ) +
  theme_minimal() +
  # Customise theme elements
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  )

#-------------------------------------------------------------------------------
# Start of Analysis for Question 3
# Do people move between Parkruns? Does this mean if you advertise at fewer places you eventually
# see people from other locations ?

# Prepare runner x parkrun attendance matrix
runner_parkrun <- df %>%
  select(id, parkrun) %>% # Select only runner id and parkrun
  distinct() %>% # Remove duplicates
  mutate(value = 1) %>% # Create a column to indicate attendance
  pivot_wider(
    names_from = parkrun, # Each unique parkrun becomes a column
    values_from = value, # Fill column with the attendance value
    values_fill = 0 # Fill missing values with 0 (did not attend)
  )

# Compute overlap matrix
parkrun_matrix <- as.matrix(runner_parkrun[, -1]) # remove id column
overlap_matrix <- t(parkrun_matrix) %*% parkrun_matrix # shared runners

# Convert overlap matrix to long format for ggplot
overlap_long <- melt(overlap_matrix)
colnames(overlap_long) <- c("from", "to", "weight")

# Remove self-loops
overlap_long <- overlap_long[overlap_long$from != overlap_long$to, ]

# Plot heatmap
q3_graph <- ggplot(overlap_long, aes(x = from, y = to, fill = weight)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#AAB5D5FF", high = "#454A74FF") +
  labs(
    title = "Heatmap of Runners Shared Between Parkruns",
    x = "Parkrun",
    y = "Parkrun",
    fill = "Shared Runners"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for readability
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

#-------------------------------------------------------------------------------
# Start of Analysis for Question 4
# If Puma want to target elite runners, where should they focus their efforts?

# elite defined as having a pb time and age grade in the top 5% of all results

# Finding top 5%
top10_pb_time_M <- tidy_data |>
  filter(sex == "Male") |>
  arrange(pb_seconds) |>
  head(720) # 5% pb time, males, highest = 1196
top10_pb_time_F <- tidy_data |>
  filter(sex == "Female") |>
  arrange(pb_seconds) |>
  head(720) # 5% pb time, females, highest = 1499
top10_age_grade <- tidy_data |>
  arrange(desc(age_grade)) |>
  head(720) # 5% age grade, lowest = 69

elite_runners <- tidy_data |>
  group_by(id, sex, parkrun) |>
  summarise(
    best_time = min(pb_seconds, na.rm = TRUE), # find the best time for each individual
    best_age_grade = max(age_grade, na.rm = TRUE), # find the age grade for each individual
  ) |>
  mutate(
    is_elite = (best_age_grade >= 69) | # defining 'is elite' by age grade and best time, top 5%
      (best_time <= 1196 & sex == "Male") &
        (best_time <= 1499 & sex == "Female")
  ) |>
  filter(is_elite == TRUE) # filter to include only elite runners

# Number of unique runners per parkrun
total_runners <- tidy_data |>
  group_by(parkrun) |>
  summarise(total_participants = n_distinct(id))

# Elite runners per parkrun
elite_runners_dist <- elite_runners |>
  group_by(parkrun) |>
  summarise(elite_total = n_distinct(id))

# Calculating elite runners as a percentage of runners per parkrun
elite_summary <- elite_runners_dist |>
  left_join(total_runners, by = "parkrun") |>
  mutate(
    elite_percent = (elite_total / total_participants) * 100
  ) |>
  arrange(desc(elite_percent))

# Bar chart to visualise total number of elite runners per parkrun
q4_graph_total <- ggplot(data = elite_summary, aes(x = reorder(parkrun, -elite_total), y = elite_total)) +
  geom_col(fill = "#EFC86EFF") +
  theme_minimal() +
  labs(
    x = "Christchurch Parkrun Locations",
    y = "Total number of elite runners",
    title = "Elite runners at Christchurch Parkrun locations"
  ) +
  theme(plot.title = element_text(hjust = 0.5),  axis.text.x = element_text(angle = 45, hjust = 1) )

# Bar chart to visualise percentage of elite runners per parkrun
q4_graph_proportion <- ggplot(data = elite_summary, aes(x = reorder(parkrun, -elite_percent), y = elite_percent)) +
  geom_col(fill = "#EFC86EFF") +
  theme_minimal() +
  labs(
    x = "Christchurch Parkrun Locations",
    y = "Percentage of Elite Runners (%)",
    title = "Percentage of Elite Runners at Christchurch Parkrun locations"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size=14),  axis.text.x = element_text(angle = 45, hjust = 1) )

#-------------------------------------------------------------------------------
# Start of Analysis for Question 5
# Puma believe more consistent runners have better potential as customers, how many runners are consistent and is consistency different across locations? Which runners consistently go to parkruns. Count for different runners amount of parkruns attended

# remove rows with missing id
df <- df %>% filter(!is.na(id))

# Count how many total runs each runner attended
consistency <- df %>%
  group_by(id) %>%
  summarise(run_count = n()) %>%
  ungroup()

consistency_counts <- consistency %>%
  count(run_count)

q5_consistency_plot <- ggplot(consistency_counts, aes(x = run_count, y = n)) +
  geom_col(fill = "#454A74FF", color = "black") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  scale_x_continuous(
    breaks = seq(min(consistency_counts$run_count),
      max(consistency_counts$run_count),
      by = 1
    ) # dont skip numbers
  ) +
  labs(
    title = "Distribution of Parkrun Attendance (Total Runs)",
    x = "Number of Runs Attended",
    y = "Number of Runners"
  ) +
  theme_minimal()

q5_consistency_plot
summary(consistency)


# amount of people who have attended more than 5 runs
more_than_5 <- consistency %>%
  filter(run_count >= 5) %>%
  summarise(count = n())

# still have more to do here, will do it when I have time

# ---

runner_location_counts <- df %>%
  group_by(parkrun, id) %>%
  summarise(run_count = n(), .groups = "drop")

location_consistency <- runner_location_counts %>%
  group_by(parkrun) %>%
  summarise(
    avg_runs = mean(run_count),
    median_runs = median(run_count),
    n_runners = n()
  )

location_consistency

# plot sorted by average runs
q5_location_consistency_plot <- ggplot(location_consistency, aes(x = reorder(parkrun, avg_runs), y = avg_runs)) +
  geom_col(fill = "#808FE1FF") +
  coord_flip() +
  labs(
    title = "Average Parkrun Attendance per Runner by Location",
    x = "Parkrun Location",
    y = "Average Number of Runs Attended"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.3, size = 12, face = "bold"))

q5_location_consistency_plot

location_consistent_counts <- df %>%
  distinct(parkrun) %>%
  left_join(
    runner_location_counts %>%
      filter(run_count >= 5) %>%
      count(parkrun, name = "consistent_runners"),
    by = "parkrun"
  ) %>%
  mutate(consistent_runners = replace_na(consistent_runners, 0))

q5_consistent_runner_counts_plot <- ggplot(location_consistent_counts, aes(x = reorder(parkrun, consistent_runners), y = consistent_runners)) +
  geom_col(fill = "#454A74FF") +
  coord_flip() +
  labs(
    title = "Consistent Runners (5+ Visits) per Parkrun",
    x = "Parkrun Location",
    y = "Number of Consistent Runners"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.3, size = 12, face = "bold"))

q5_consistent_runner_counts_plot

#-------------------------------------------------------------------------------
# Start of Analysis for Question 6

# Code for question 6
park_coordinates <- read_table("intermediate/park_lat_lng") %>%
  mutate(lat = round(as.numeric(lat), 2), lng = round(as.numeric(lng), 2))
# Calculates distances between locations in Meters based on long and lat
distances_matrix <- park_coordinates %>%
  select(lng, lat) %>%
  distm(., fun = distHaversine)

row.names(distances_matrix) <- park_coordinates$park
colnames(distances_matrix) <- park_coordinates$park
distances_betweenparks <- melt(distances_matrix) %>%
  filter(as.integer(value) != 0)
colnames(distances_betweenparks) <- c("from", "to", "distance")
# Dividing by 1000 to transform meters to kilometers for distance

joined_dist <- distances_betweenparks %>%
  # Remove distances between same location
  filter(distance != 0) %>%
  # Combine with overlap matrix from question 3
  left_join(., overlap_long, by = c("from", "to")) %>%
  # Convert Hervsine shortest distance Meters to km
  mutate(overlap = weight, distance = distance / 1000) %>%
  select("from", "to", "distance", "overlap")

non_log_graph <- ggplot(joined_dist, aes(distance, overlap)) +
  geom_point() +
  labs(title = "Distance vs Overlapping runners") +
  xlab("Distance (Km)") +
  ylab("Overlapping Runners")

# Fitting linear model based off overlap by distance
dist_overlap <- lm(log(overlap) ~ distance, data = joined_dist)

pred_dist <- ggplot(joined_dist, aes(distance, log(overlap))) +
  geom_point(aes(color = distance)) +
  geom_abline(
    slope = coef(dist_overlap)["distance"],
    intercept = coef(dist_overlap)[["(Intercept)"]]
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Distance vs log(Overlapping) runners", color = "") +
  xlab("Distance (Km)") +
  ylab("Overlapping Runners (log scaled)")
