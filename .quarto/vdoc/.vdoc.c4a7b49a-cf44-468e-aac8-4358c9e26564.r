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
#
