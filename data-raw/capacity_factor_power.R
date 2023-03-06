library(tidyverse)
# based on IEA WEO 2022, table B.4a: capacity factors for the stated polcies
# scenario (SPS) in 2021, which describes the status quo. Values for EU.
capacity_factor_power_raw <- readr::read_csv("path/to/weo2022_extended_metrics.csv")

capacity_factor_power <- capacity_factor_power_raw %>%
  janitor::clean_names() %>%
  dplyr::filter(
    scenario == "Stated Policies Scenario",
    year == 2021,
    flow %in% c("Electricity generation", "Electrical capacity")
  ) %>%
  # metrics to common scale
  dplyr::mutate(
    value = dplyr::if_else(flow == "Electricity generation", .data$value * 1000, .data$value),
    unit = dplyr::if_else(flow == "Electricity generation", "GWh", .data$unit)
  ) %>%
  # capacity to potential max generation
  dplyr::mutate(
    value = dplyr::if_else(flow == "Electrical capacity", .data$value * 365.25 * 24, .data$value),
    unit = dplyr::if_else(flow == "Electrical capacity", "GWh", .data$unit),
    flow = dplyr::if_else(flow == "Max electricity generation", "GWh", .data$flow)
  ) %>%
  # create categories matching PACTA technologies
  dplyr::filter(
    !.data$product %in% c("Total", "Renewables", "Battery storage", "Hydrogen and H2-based fuels"),
    !grepl("Fossil fuels", .data$product)
  ) %>%
  dplyr::mutate(
    technology = dplyr::case_when(
      .data$product %in% c("Coal: unabated", "Coal: with CCUS") ~ "coalcap",
      .data$product %in% c("Natural gas: unabated", "Natural gas: with CCUS") ~ "gascap",
      .data$product %in% c("Oil") ~ "oilcap",
      .data$product %in% c("Hydro") ~ "hydrocap",
      .data$product %in% c("Nuclear") ~ "nuclearcap",
      .data$product %in% c(
        "Solar PV", "Wind", "Modern bioenergy and renewable waste", "Bioenergy: with CCUS", "Concentrating solar power", "Geothermal", "Marine"
      ) ~ "renewablescap",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(-"category") %>%
  tidyr::pivot_wider(names_from = "flow", values_from = "value") %>%
  janitor::clean_names() %>%
  dplyr::summarise(
    generation = sum(.data$electricity_generation, na.rm = TRUE),
    capacity = sum(.data$electrical_capacity, na.rm = TRUE),
    .by = c("publication", "scenario", "region", "year", "technology", "unit")
  ) %>%
  # calculate capacity factors
  dplyr::mutate(
    capacity_factor = .data$generation / .data$capacity
  ) %>%
  # beautify output
  dplyr::rename(scenario_source = "publication") %>%
  dplyr::mutate(
    scenario_source = dplyr::if_else(.data$scenario_source == "World Energy Outlook 2022", "weo_2022", .data$scenario_source),
    scenario = dplyr::if_else(.data$scenario == "Stated Policies Scenario", "sps", .data$scenario),
    region = dplyr::if_else(.data$region == "World", "global", .data$region)
  ) %>%
  dplyr::select(c("scenario_source", "scenario", "region", "year", "technology", "capacity_factor"))

usethis::use_data(capacity_factor_power, overwrite = TRUE)
