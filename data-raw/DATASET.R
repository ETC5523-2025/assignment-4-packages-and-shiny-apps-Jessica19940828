# Load library
library(readr)
library(dplyr)
library(usethis)
library(here)

# Read in raw data
fwi_data <- readr::read_table(
  here::here("data-raw", "fwi_data.txt"),
  col_names = c("year", "fwi_sm")
)

msr_data <- readr::read_table(
  here::here("data-raw", "msr_data.txt"),
  col_names = c("year", "msr_sm")
)

# Combine datasets
ausbushfire_data_raw <- dplyr::full_join(fwi_data, msr_data, by = "year")

# Filter our year 1978
ausbushfire_data <- ausbushfire_data_raw %>%
  dplyr::filter(year >= 1979)

# Save clean data into 'data/' folder
usethis::use_data(ausbushfire_data, overwrite = TRUE)
