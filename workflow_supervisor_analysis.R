# load packages----
devtools::load_all()

library(dplyr)
library(r2dii.analysis)
library(r2dii.data)
library(r2dii.match)
library(r2dii.plot)
library(readxl)
library(rlang)
library(tidyr)
library(vroom)

# set parameters----
scenario_source_input <- "weo_2021"
scenario_select <- "nze_2050"
region_select <- c("global", "european union")
region_isos_select <- r2dii.data::region_isos %>%
  dplyr::filter(
    .data$source == .env$scenario_source_input,
    .data$region %in% .env$region_select
  )

# set directories----

# input_path_scenario <- file.path("/path/to/input/directory/scenario/file.csv")
# input_directory_abcd <- file.path("/path/to/input/directory/abcd/")
# input_directory_raw <- file.path("/path/to/input/directory/raw/")
# input_directory_matched <- file.path("/path/to/input/directory/matched/")
# output_directory_p4b_standard <- file.path("/path/to/input/directory/output/standard")
# output_directory_p4b_aggregated <- file.path("/path/to/input/directory/aggregated")

# load input data----
scenario_input_tms <- read.csv(input_path_scenario_tms)
scenario_input_sda <- read.csv(input_path_scenario_sda)

# abcd <- abcd_test_data
abcd <- readr::read_csv(file.path(input_directory_abcd, "abcd_test_data.csv"))
# replace potential NA values with 0 in production
abcd["production"][is.na(abcd["production"])] <- 0

# loanbook <- loanbook_test_data
loanbook <- purrr::map_dfr(list.files(input_directory_raw, full.names = T), .f = vroom::vroom, id = "bank_id")
loanbook <- loanbook %>%
  dplyr::mutate(bank_id = gsub(pattern = paste0(input_directory_raw, "/"), replacement = "", x = .data$bank_id)) %>%
  dplyr::mutate(bank_id = gsub(pattern = ".csv", replacement = "", x = .data$bank_id))

# match and prioritize loan book----
unique_loanbooks_raw <- unique(loanbook$bank_id)

matched_loanbook <- NULL

for (i in unique_loanbooks_raw) {
  loanbook_i <- loanbook %>%
    dplyr::filter(.data$bank_id == i)

  matched_i <- match_name(loanbook_i, abcd) %>%
    prioritize()

  names_i <- unique(matched_i$name_abcd)

  matched_loanbook <- matched_loanbook %>%
    dplyr::bind_rows(matched_i)
}

# matched_loanbook %>%
#   readr::write_csv(file.path(input_directory_matched, "matched_prio_all_banks.csv"))

# add loan book with corporate economy benchmark
# TODO: we need to replace the loan size with a value that reflects the size
# of the company in the real economy
loanbook_corporate_benchmark <- abcd %>%
  create_benchmark_loanbook()

# matching the benchmark loan book separately, because it is not needed for the
# generation of standard PACTA output
matched_benchmark <- match_name(loanbook_corporate_benchmark, abcd) %>%
  prioritize()

# matched_benchmark %>%
#   readr::write_csv(file.path(input_directory_matched, "matched_prio_benchmark.csv"))

# get sector split for energy companies----
start_year <- scenario_input_tms %>%
  dplyr::filter(
    scenario_source == scenario_source_input,
    scenario == scenario_select,
    region == "global"
  ) %>%
  dplyr::distinct(.data$year) %>%
  dplyr::pull(.data$year) %>%
  min()

primary_energy_efficiency <- get("primary_energy_efficiency")

capacity_factors <- get("capacity_factor_power") %>%
  dplyr::select(c("technology", "capacity_factor")) %>%
  dplyr::distinct()

unit_conversion <- get("unit_conversion")

sector_split_energy_companies <- abcd %>%
  get_energy_sector_split(
    start_year = start_year,
    primary_energy_efficiency = primary_energy_efficiency,
    capacity_factor_power = capacity_factors,
    unit_conversion = unit_conversion
  )
# TODO match with loan book

# generate all P4B outputs----
unique_loanbooks_matched <- unique(matched_loanbook$bank_id)

## generate SDA outputs----
results_sda_total <- NULL

for (i in unique_loanbooks_matched) {
  matched_i <- matched_loanbook %>%
    dplyr::filter(.data$bank_id == i) %>%
    dplyr::select(-"bank_id")

  results_sda_i <- matched_i %>%
    target_sda(
      abcd = abcd,
      co2_intensity_scenario = scenario_input_sda,
      region_isos = region_isos_select
    ) %>%
    dplyr::mutate(bank_id = .env$i)

  results_sda_total <- results_sda_total %>%
    dplyr::bind_rows(results_sda_i)
}

# results_sda_total %>%
#   readr::write_csv(file.path(output_directory_p4b_standard, "sda_results_all_banks.csv"))


## generate TMS outputs----

results_tms_total <- NULL

for (i in unique_loanbooks_matched) {
  matched_i <- matched_loanbook %>%
    dplyr::filter(.data$bank_id == i) %>%
    dplyr::select(-"bank_id")

  results_tms_i <- matched_i %>%
    target_market_share(
      abcd = abcd,
      scenario = scenario_input_tms,
      region_isos = region_isos_select
    ) %>%
    dplyr::mutate(bank_id = .env$i)

  results_tms_total <- results_tms_total %>%
    dplyr::bind_rows(results_tms_i)
}

# results_tms_total %>%
#   readr::write_csv(file.path(output_directory_p4b_standard, "tms_results_all_banks.csv"))

# generate P4B plots----

# results_tms_total <- readr::read_csv(file.path(output_directory_p4b_standard, "tms_results_all_banks.csv"), col_types = readr::cols())
# results_sda_total <- readr::read_csv(file.path(output_directory_p4b_standard, "sda_results_all_banks.csv"), col_types = readr::cols())
# matched_loanbook <- readr::read_csv("file.path(input_directory_matched, "matched_prio_all_banks.csv"), col_types = readr::cols())

## retrieve set of unique banks to loop over----
unique_banks_tms <- unique(results_tms_total$bank_id)
unique_banks_sda <- unique(results_sda_total$bank_id)

## run automatic result generation ----------
# TODO: parameterize inputs
# TODO: get all available sectors and produce outputs for them all)
for (tms_i in unique_banks_tms) {
  generate_individual_outputs(
    data = results_tms_total,
    matched_loanbook = matched_loanbook,
    output_directory = output_directory_p4b_standard,
    target_type = "tms",
    bank_id = tms_i,
    scenario_source = scenario_source_input,
    target_scenario = glue::glue("target_{scenario_select}"),
    region = "global",
    sector = "power"
  )
}

# TODO: get all available sectors and produce outputs for them all)
for (sda_i in unique_banks_sda) {
  generate_individual_outputs(
    data = results_sda_total,
    matched_loanbook = matched_loanbook,
    output_directory = output_directory_p4b_standard,
    target_type = "sda",
    bank_id = sda_i,
    scenario_source = scenario_source_input,
    target_scenario = glue::glue("target_{scenario_select}"),
    region = "global",
    sector = "steel"
  )
}


# aggregate P4B alignment----

# TODO: add aggregation for SDA sectors

## set specifications----

# for the calculation of the aggregate company score, we do not force companies
# to enter a new market to build out hydro power or nuclear power, as this may
# not be feasible for political and/or geographic reasons.
# in the power sector, only renewables continues to follow the SMSP logic
green_or_brown_aggregate_score <- r2dii.data::green_or_brown %>%
  dplyr::mutate(
    green_or_brown = dplyr::if_else(
      .data$technology %in% c("hydrocap", "nuclearcap"),
      "brown",
      .data$green_or_brown
    )
  )

# define if technologies should be treated as build out or phase down in the
# aggregation
technology_direction <- scenario_input_tms %>%
  dplyr::filter(.data$year %in% c(2021, 2026)) %>%
  dplyr::distinct(.data$scenario_source, .data$scenario, .data$sector, .data$technology, .data$region) %>%
  dplyr::inner_join(r2dii.data::green_or_brown, by = c("sector", "technology")) %>%
  dplyr::mutate(
    directional_dummy = dplyr::if_else(.data$green_or_brown == "green", 1, -1)
  ) %>%
  dplyr::select(-"green_or_brown")

# add benchmark loan book for aggregation
matched_total <- matched_loanbook %>%
  dplyr::bind_rows(matched_benchmark)

## prepare TMS company level P4B results for aggregation----
tms_result_for_aggregation <- NULL

unique_banks_tms_aggregation <- c(unique_banks_tms, unique(matched_benchmark$bank_id))

for (i in unique_banks_tms_aggregation) {
  tryCatch(
    {
      tms_result_for_aggregation_i <- target_market_share(
        data = matched_total %>%
          dplyr::filter(.data$bank_id == i) %>%
          dplyr::select(-"bank_id"),
        abcd = abcd,
        scenario = scenario_input_tms,
        region_isos = region_isos_select,
        by_company = TRUE,
        weight_production = FALSE,
        green_or_brown = green_or_brown_aggregate_score
      )

      tms_result_for_aggregation_i <- tms_result_for_aggregation_i %>%
        dplyr::mutate(bank_id = .env$i)

      tms_result_for_aggregation <- tms_result_for_aggregation %>%
        dplyr::bind_rows(tms_result_for_aggregation_i)

    },
    error = function(e) {
      log_text <- glue::glue("{Sys.time()} - bank: {i} Problem in preparing data for aggregation. Skipping! \n")
      write(log_text, file = file.path(output_directory_p4b_aggregated, "error_messages.txt"), append = TRUE)
    }
  )
}

## aggregate TMS P4B results to company level alignment score----
# calculate aggregation for the loan book

tms_company_technology_deviation <- tms_result_for_aggregation %>%
  calculate_company_tech_deviation(
    technology_direction = technology_direction,
    scenario_trajectory = scenario_input_tms,
    green_or_brown = green_or_brown_aggregate_score,
    scenario_source = scenario_source_input,
    scenario = scenario_select
    # bridge_tech = "gascap"
  )

tms_company_technology_deviation %>%
  readr::write_csv(file.path(output_directory_p4b_aggregated, "tms_company_technology_deviation.csv"))

tms_aggregated <- tms_company_technology_deviation %>%
  calculate_company_aggregate_score_tms(
    scenario_source = scenario_source_input,
    scenario = scenario_select,
    level = "net"
  )

tms_aggregated %>%
  readr::write_csv(file.path(output_directory_p4b_aggregated, "tms_aggregated_company.csv"))

tms_aggregated_buildout_phaseout <- tms_company_technology_deviation %>%
  calculate_company_aggregate_score_tms(
    scenario_source = scenario_source_input,
    scenario = scenario_select,
    level = "bo_po"
  )

tms_aggregated_buildout_phaseout %>%
  readr::write_csv(file.path(output_directory_p4b_aggregated, "tms_aggregated_company_buildout_phaseout.csv"))

## prepare SDA company level P4B results for aggregation----
sda_result_for_aggregation <- NULL

unique_banks_sda_aggregation <- c(unique_banks_sda, unique(matched_benchmark$bank_id))

for (i in unique_banks_sda_aggregation) {
  tryCatch(
    {
      sda_result_for_aggregation_i <- target_sda(
        data = matched_total %>%
          dplyr::filter(.data$bank_id == i) %>%
          dplyr::select(-"bank_id"),
        abcd = abcd,
        co2_intensity_scenario = scenario_input_sda,
        by_company = TRUE,
        region_isos = region_isos_select
      )

      sda_result_for_aggregation_i <- sda_result_for_aggregation_i %>%
        dplyr::mutate(bank_id = .env$i)

      sda_result_for_aggregation <- sda_result_for_aggregation %>%
        dplyr::bind_rows(sda_result_for_aggregation_i)

    },
    error = function(e) {
      log_text <- glue::glue("{Sys.time()} - bank: {i} Problem in preparing data for aggregation. Skipping! \n")
      write(log_text, file = file.path(output_directory_p4b_aggregated, "error_messages.txt"), append = TRUE)
    }
  )
}

## aggregate SDA P4B results to company level alignment score----
# calculate aggregation for the loan book
sda_aggregated <- sda_result_for_aggregation %>%
  calculate_company_aggregate_score_sda(
    scenario_emission_intensities = scenario_input_sda,
    scenario_source = scenario_source_input,
    scenario = scenario_select
  )

sda_aggregated %>%
  readr::write_csv(file.path(output_directory_p4b_aggregated, "sda_aggregated_company.csv"))


## calculate sector and loan book level aggregate alignment based on company exposures in loan book----

# the company level aggregate scores are then joined with the matched loan book
# to derive some high level summary statistics on the loan book level
companies_aggregated <- tms_aggregated %>%
  dplyr::bind_rows(sda_aggregated)

# show exposures (n companies and loan size) by alignment with given scenario
# TODO: correctly aggregate to bo_po level

# net
aggregate_exposure_loanbook <- companies_aggregated %>%
  calculate_loanbook_exposure_scores(
    matched = matched_total,
    level = "net"
  )

aggregate_exposure_loanbook %>%
  readr::write_csv(file.path(output_directory_p4b_aggregated, "aggregate_exposure_loanbook.csv"))

# buildout / phaseout
aggregate_exposure_loanbook_bopo <- tms_aggregated_buildout_phaseout %>%
  calculate_loanbook_exposure_scores(
    matched = matched_total,
    level = "bo_po"
  )

aggregate_exposure_loanbook_bopo %>%
  readr::write_csv(file.path(output_directory_p4b_aggregated, "aggregate_exposure_loanbook_bopo.csv"))

# Plot sankey plot of financial flows scenario alignment

data_sankey <- prep_sankey(
  tms_aggregated,
  sda_aggregated,
  matched_loanbook,
  region_tms = "global",
  region_sda = "global",
  year = 2026,
  middle_node = "name_abcd"
  )

plot_sankey(data_sankey, save_png_to = output_directory_p4b_aggregated)
