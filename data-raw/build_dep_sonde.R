## code to prepare `dep_sonde` dataset.
library(dplyr)

library(readr)

dep_sonde <- read_csv('data-raw/sonde_data.csv',
                       col_types = cols(
                         site_name = col_character(),
                         site = col_character(),
                         dt = col_date(format = ""),
                         month = col_character(),
                         year = col_double(),
                         time = col_time(format = ""),
                         hour = col_double(),
                         depth = col_double(),
                         temp = col_double(),
                         salinity = col_double(),
                         ph = col_double(),
                         pctsat = col_double(),
                         do = col_double(),
                         chl_a_sonde = col_double(),
                         turbidity = col_double(),
                         turbidity_cens = col_logical())) %>%
  rename(sample_date = dt) %>%

  # The full data set contains records for several years and sites.  We
  # select one, arbitraril;y.
  filter(site == 'FR09', year == 2018) %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  select(-site_name, -year, -hour)

usethis::use_data(dep_sonde, overwrite = TRUE)

