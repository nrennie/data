library(tidyverse)
library(rvest)

reddit_url <- "https://www.reddit.com/r/Cooking/comments/1qxdjxy/i_timed_how_long_31_different_pasta_shapes_take/"

raw_html <- read_html(reddit_url)

clean_html <- raw_html |>
  html_elements("table") |>
  html_table()

pasta_data <- clean_html[[1]] |>
  separate_wider_delim(`box time`, delim = "-", names = c("box_min", "box_max")) |>
  mutate(across(starts_with("box"), ~parse_number(.x))) |>
  rename(actual = `actual al dente`) |>
  mutate(actual = if_else(pasta == "farfalle", NA, actual),
         difference = if_else(pasta == "farfalle", NA, difference)) |>
  mutate(
    actual_decimal = as.numeric(sub(':.*','',actual)) + as.numeric(sub('.*:','',actual))/60,
    difference_decimal = {
      sgn  <- ifelse(startsWith(difference, '-'), -1, 1)
      t    <- gsub('-', '', difference)
      mins <- as.numeric(sub(':.*', '', t))
      secs <- as.numeric(sub('.*:', '', t))
      sgn * (mins + secs / 60)
    }
  ) |>
  mutate(pasta = str_to_title(pasta))

write_csv(pasta_data, "reddit-al-dente-pasta/al_dente_pasta.csv")
