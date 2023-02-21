library(tidyverse)

sda_sectors <- c("aviation", "cement", "steel")

activity_units <- r2dii.data::abcd_demo %>%
  dplyr::distinct(
    .data$sector, .data$technology, .data$production_unit, .data$emission_factor_unit
  ) %>%
  dplyr::rename(activity_unit = "production_unit") %>%
  dplyr::mutate(
    activity_unit = dplyr::if_else(
      .data$sector %in% sda_sectors,
      .data$emission_factor_unit,
      .data$activity_unit
    )
  ) %>%
  dplyr::select(-"emission_factor_unit")

usethis::use_data(activity_units, overwrite = TRUE)
