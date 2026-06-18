#
#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
#
#
#
billboard %>%
  select(artist, track, date.entered, matches("^wk[1-4]$")) %>%
  print(n = 20)
#
#
#
# Summary of date.entered to show time period covered
summary(as.Date(billboard$date.entered))
#
#
#
# Histogram of wk1 with sample size in title
wk1_n <- sum(!is.na(billboard$wk1))
ggplot(billboard, aes(x = wk1)) +
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue", na.rm = TRUE) +
  labs(
    title = paste0("Histogram of wk1 (n = ", wk1_n, ")"),
    x = "wk1 ranking",
    y = "count"
  ) +
  theme_minimal()
#
#
#
  # Histogram of wk6 with sample size in title
  wk6_n <- sum(!is.na(billboard$wk6))
  ggplot(billboard, aes(x = wk6)) +
    geom_histogram(binwidth = 1, color = "black", fill = "salmon", na.rm = TRUE) +
    labs(
      title = paste0("Histogram of wk6 (n = ", wk6_n, ")"),
      x = "wk6 ranking",
      y = "count"
    ) +
    theme_minimal()
#
#
#
  # Table of missing and present counts for selected wk columns
  wk_cols <- c("wk1","wk4","wk10","wk20","wk40","wk76")
  counts <- tibble(column = wk_cols) %>%
    mutate(
      present = map_int(column, ~ sum(!is.na(billboard[[.x]]))),
      missing = map_int(column, ~ sum(is.na(billboard[[.x]]))),
      total = nrow(billboard)
    )
  counts %>% print()
#
#
#
# Detect songs that leave the chart and re-enter
billboard <- billboard %>%
  mutate(
    re_entered = apply(select(., starts_with("wk")), 1, function(row) {
      # Find indices of non-NA and NA values
      non_na_idx <- which(!is.na(row))
      
      # If fewer than 2 non-NA values, can't re-enter
      if (length(non_na_idx) < 2) return(FALSE)
      
      # Check if there's a gap: non-NA, then NA, then non-NA
      for (i in 1:(length(non_na_idx) - 1)) {
        gap_after <- non_na_idx[i] + 1
        gap_before <- non_na_idx[i + 1] - 1
        if (gap_after <= gap_before) {
          # There's a gap between two non-NA values
          return(TRUE)
        }
      }
      FALSE
    })
  )

# Table of re-entry counts
billboard %>%
  summarize(
    `Re-entered` = sum(re_entered),
    `Did not re-enter` = sum(!re_entered)
  ) %>%
  print()
#
#
#
# Compare wk1 and wk6 ranks
comparison <- billboard %>%
  mutate(
    rank_change = wk6 - wk1,
    status = case_when(
      is.na(wk6) & !is.na(wk1) ~ "Missing by wk6",
      is.na(wk1) | is.na(wk6) ~ "Missing data",
      wk6 < wk1 ~ "Improved",
      wk6 == wk1 ~ "Stayed same",
      wk6 > wk1 ~ "Got worse"
    )
  )

# Count by status
status_counts <- comparison %>%
  group_by(status) %>%
  summarize(count = n(), .groups = "drop")

status_counts %>% print()

# Median rank change for songs with both weeks
cat("\nMedian rank change (songs with both wk1 and wk6):\n")
comparison %>%
  filter(!is.na(rank_change)) %>%
  summarize(median_change = median(rank_change)) %>%
  print()
#
#
#
# Reshape billboard data to long format: week and rank columns
billboard_long <- billboard %>%
  select(artist, track, starts_with("wk")) %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    names_prefix = "wk",
    names_transform = list(week = as.numeric)
  ) %>%
  filter(!is.na(rank))

# Plot ranking over time for each song
ggplot(billboard_long, aes(x = week, y = rank, group = interaction(artist, track), color = artist)) +
  geom_line(alpha = 0.3, size = 0.5) +
  scale_y_reverse() +
  labs(
    title = "Billboard Song Rankings Over Time",
    x = "Week",
    y = "Rank (lower is better)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
#
#
#
# Create song-level summary
song_summary <- billboard_long %>%
  group_by(artist, track) %>%
  summarize(
    first_rank = first(rank),
    best_rank = min(rank),
    week_at_best = first(week[rank == min(rank)]),
    total_weeks = n(),
    .groups = "drop"
  )

song_summary %>% print(n = 20)

# Identify three notable songs
cat("\n=== NOTABLE SONGS ===\n")

# 1. Fastest to reach #1 (minimum week number for songs that hit rank 1)
fastest_to_one <- song_summary %>%
  filter(best_rank == 1) %>%
  arrange(week_at_best) %>%
  slice(1)
cat("\nFastest to reach #1:\n")
print(fastest_to_one)

# 2. Slowest to reach #1 (maximum week number for songs that hit rank 1)
slowest_to_one <- song_summary %>%
  filter(best_rank == 1) %>%
  arrange(desc(week_at_best)) %>%
  slice(1)
cat("\nSlowest to reach #1:\n")
print(slowest_to_one)

# 3. Top-10 song with longest chart run
longest_top10 <- song_summary %>%
  filter(best_rank <= 10) %>%
  arrange(desc(total_weeks)) %>%
  slice(1)
cat("\nTop-10 song with longest chart run:\n")
print(longest_top10)
#
#
#
#| cache: true
# Plot top-10 songs with three notable songs highlighted
# Get the three notable songs
fastest_song <- fastest_to_one %>% select(artist, track)
slowest_song <- slowest_to_one %>% select(artist, track)
longest_song <- longest_top10 %>% select(artist, track)

# Filter to top-10 songs and mark the notable ones
top10_long <- billboard_long %>%
  inner_join(
    song_summary %>% filter(best_rank <= 10) %>% select(artist, track),
    by = c("artist", "track")
  ) %>%
  mutate(
    song_type = case_when(
      artist == fastest_to_one$artist & track == fastest_to_one$track ~ "Fastest to #1",
      artist == slowest_to_one$artist & track == slowest_to_one$track ~ "Slowest to #1",
      artist == longest_top10$artist & track == longest_top10$track ~ "Longest top-10 run",
      TRUE ~ "Other top-10"
    )
  )

# Create plot with conditional coloring
ggplot(top10_long, aes(x = week, y = rank, group = interaction(artist, track), color = song_type)) +
  geom_line(
    data = filter(top10_long, song_type == "Other top-10"),
    aes(color = NULL),
    color = "gray80",
    size = 0.5,
    alpha = 0.6
  ) +
  geom_line(
    data = filter(top10_long, song_type != "Other top-10"),
    size = 1
  ) +
  scale_y_reverse() +
  labs(
    title = "Top-10 Songs with Notable Performers Highlighted",
    x = "Week",
    y = "Rank (lower is better)",
    color = "Song Type"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
#
#
#
# Read and print data/music.csv with readr
music <- readr::read_csv("data/music.csv")
print(music)
```
#
#
#
# Summary statistics for selected music fields
music %>%
  select(artist.familiarity, artist.hotttnesss, song.year, song.tempo) %>%
  summarize(across(everything(), list(
    min = ~ min(.x, na.rm = TRUE),
    q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
    median = ~ median(.x, na.rm = TRUE),
    q3 = ~ quantile(.x, 0.75, na.rm = TRUE),
    max = ~ max(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  print()
#
#
#
# Plot distribution of song.year excluding missing years
year_data <- music %>%
  filter(song.year != 0)

year_n <- nrow(year_data)

ggplot(year_data, aes(x = song.year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Song Release Years",
    subtitle = paste0("Number of songs used: ", year_n),
    x = "Song Year",
    y = "Count"
  ) +
  theme_minimal()
#
#
#
# Print a smaller, more readable subset of music columns
music %>%
  select(
    artist.name,
    artist.location,
    artist.latitude,
    artist.longitude,
    artist.terms,
    artist.familiarity,
    artist.hotttnesss,
    song.title,
    song.year
  ) %>%
  print(n = 20)

```{r}
# Count rows with placeholder values in selected music fields
placeholder_counts <- tibble(
  column = c(
    "artist.location",
    "release.name",
    "song.title",
    "song.year",
    "artist.familiarity",
    "artist.hotttnesss"
  ),
  placeholder_count = c(
    sum(is.na(music$artist.location) | music$artist.location == ""),
    sum(is.na(music$release.name) | music$release.name == ""),
    sum(is.na(music$song.title) | music$song.title == ""),
    sum(is.na(music$song.year) | music$song.year == 0),
    sum(is.na(music$artist.familiarity) | music$artist.familiarity == 0),
    sum(is.na(music$artist.hotttnesss) | music$artist.hotttnesss == 0)
  )
) %>%
  mutate(
    total_rows = nrow(music),
    percent = placeholder_count / total_rows * 100
  )

placeholder_counts %>%
  arrange(desc(placeholder_count)) %>%
  print()
#
#
#
#
#
# Count unique artists with usable versus placeholder coordinates
artist_coord_counts <- music %>%
  distinct(artist.name, artist.latitude, artist.longitude) %>%
  mutate(
    coord_status = case_when(
      artist.latitude == 0 & artist.longitude == 0 ~ "placeholder",
      TRUE ~ "usable"
    )
  ) %>%
  count(coord_status, name = "artist_count") %>%
  mutate(percent = artist_count / sum(artist_count) * 100)

artist_coord_counts %>%
  arrange(desc(artist_count)) %>%
  print()
#
#
#
# Map artists with usable coordinates on a world map
if (!requireNamespace("maps", quietly = TRUE)) {
  install.packages("maps")
}

usable_artists <- music %>%
  distinct(artist.name, artist.latitude, artist.longitude) %>%
  filter(!(artist.latitude == 0 & artist.longitude == 0))

world_map <- ggplot2::map_data("world")

ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray60",
    size = 0.2
  ) +
  geom_point(
    data = usable_artists,
    aes(x = artist.longitude, y = artist.latitude),
    color = "steelblue",
    alpha = 0.6,
    size = 2
  ) +
  coord_quickmap() +
  labs(
    title = "Artist Locations Based on Usable Coordinates",
    subtitle = "Only artists with nonzero latitude and longitude are shown; zero/zero coordinates are treated as placeholders",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()
#
#
#
#
#
