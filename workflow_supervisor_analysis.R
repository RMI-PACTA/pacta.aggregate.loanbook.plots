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

# set directories----

# path_to_regions_geco_2022 <- file.path("/path/to/input/directory/scenario/region_geco2022.csv")
# path_to_regions_weo_2022 <- file.path("/path/to/input/directory/scenario/region_weo2022.csv")
# input_path_scenario_tms <- file.path("/path/to/input/directory/scenario/tms_file.csv")
# input_path_scenario_sda <- file.path("/path/to/input/directory/scenario/sda_file.csv")
# input_directory_abcd <- file.path("/path/to/input/directory/abcd/")
# input_directory_raw <- file.path("/path/to/input/directory/raw/")
# input_directory_matched <- file.path("/path/to/input/directory/matched/")
# output_directory_p4b_standard <- file.path("/path/to/input/directory/output/standard")
# output_directory_p4b_aggregated <- file.path("/path/to/input/directory/aggregated")

# set parameters----
scenario_source_input <- "weo_2022"
scenario_select <- "nze_2050"
region_select <- "global"
# region_select <- "european union"

# r2dii.data is not updated yet, so we manually update the region_isos data to
# cover the 2022 scenarios
regions_geco_2022 <- readr::read_csv(path_to_regions_geco_2022)
regions_weo_2022 <- readr::read_csv(path_to_regions_weo_2022)

region_isos_updated <- r2dii.data::region_isos %>%
  rbind(regions_geco_2022) %>%
  rbind(regions_weo_2022)

region_isos_select <- region_isos_updated %>%
  dplyr::filter(
    .data$source == .env$scenario_source_input,
    .data$region %in% .env$region_select
  )

start_year <- 2022


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

  matched_loanbook <- matched_loanbook %>%
    dplyr::bind_rows(matched_i)
}

# matched_loanbook %>%
#   readr::write_csv(file.path(input_directory_matched, "matched_prio_all_banks.csv"))

# add loan book with corporate economy benchmark----
# benchmark_region can be selected based on r2dii.data::region_isos
benchmark_regions <- c("global", "european union")

matched_benchmark <- NULL

# matching the benchmark loan book separately, because it is not needed for the
# generation of standard PACTA output
for (i in benchmark_regions) {
  loanbook_corporate_benchmark_i <- abcd %>%
    create_benchmark_loanbook(
      scenario_source = scenario_source_input,
      start_year = start_year,
      region_isos = region_isos_updated,
      benchmark_region = i
    )

  matched_benchmark_i <- match_name(loanbook_corporate_benchmark_i, abcd) %>%
    prioritize()

  matched_benchmark <- matched_benchmark %>%
    dplyr::bind_rows(matched_benchmark_i)
}

# matched_benchmark %>%
#   readr::write_csv(file.path(input_directory_matched, "matched_prio_benchmark.csv"))


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
  dplyr::filter(.data$year %in% c(2022, 2027)) %>%
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

for (i in unique_banks_tms) {
  tryCatch(
    {
      tms_result_for_aggregation_i <- target_market_share(
        data = matched_loanbook %>%
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

tms_result_for_aggregation_benchmark <- NULL

unique_benchmarks_tms <- unique(matched_benchmark$bank_id)

for (i in unique_benchmarks_tms) {
  tryCatch(
    {
      benchmark_region_i <- gsub("benchmark_corporate_economy_", "", i)

      allowed_countries_i <- region_isos_updated %>%
        dplyr::filter(
          .data$source == .env$scenario_source_input,
          .data$region == .env$benchmark_region_i,
        ) %>%
        dplyr::pull(.data$isos) %>%
        toupper()

      abcd_benchmark_region_i <- abcd %>%
        dplyr::filter(.data$plant_location %in% .env$allowed_countries_i)

      tms_result_for_aggregation_benchmark_i <- target_market_share(
        data = matched_benchmark %>%
          dplyr::filter(.data$bank_id == i) %>%
          dplyr::select(-"bank_id"),
        abcd = abcd_benchmark_region_i,
        scenario = scenario_input_tms,
        region_isos = region_isos_select,
        by_company = TRUE,
        weight_production = FALSE,
        green_or_brown = green_or_brown_aggregate_score
      )

      tms_result_for_aggregation_benchmark_i <- tms_result_for_aggregation_benchmark_i %>%
        dplyr::mutate(bank_id = .env$i)

      tms_result_for_aggregation_benchmark <- tms_result_for_aggregation_benchmark %>%
        dplyr::bind_rows(tms_result_for_aggregation_benchmark_i)

    },
    error = function(e) {
      log_text <- glue::glue("{Sys.time()} - bank: {i} Problem in preparing data for benchmark aggregation. Skipping! \n")
      write(log_text, file = file.path(output_directory_p4b_aggregated, "error_messages.txt"), append = TRUE)
    }
  )
}

# bind the TMS results from the loan book and benchmark PACTA runs for further
# aggregation
tms_result_for_aggregation <- tms_result_for_aggregation %>%
  dplyr::bind_rows(tms_result_for_aggregation_benchmark)

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

for (i in unique_banks_sda) {
  tryCatch(
    {
      sda_result_for_aggregation_i <- target_sda(
        data = matched_loanbook %>%
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

sda_result_for_aggregation_benchmark <- NULL

unique_benchmarks_sda <- unique(matched_benchmark$bank_id)

for (i in unique_benchmarks_sda) {
  tryCatch(
    {
      benchmark_region_i <- gsub("benchmark_corporate_economy_", "", i)

      allowed_countries_i <- region_isos_updated %>%
        dplyr::filter(
          .data$source == .env$scenario_source_input,
          .data$region == .env$benchmark_region_i,
        ) %>%
        dplyr::pull(.data$isos) %>%
        toupper()

      abcd_benchmark_region_i <- abcd %>%
        dplyr::filter(.data$plant_location %in% .env$allowed_countries_i)

      sda_result_for_aggregation_benchmark_i <- target_sda(
        data = matched_benchmark %>%
          dplyr::filter(.data$bank_id == i) %>%
          dplyr::select(-"bank_id"),
        abcd = abcd_benchmark_region_i,
        co2_intensity_scenario = scenario_input_sda,
        by_company = TRUE,
        region_isos = region_isos_select
      )

      sda_result_for_aggregation_benchmark_i <- sda_result_for_aggregation_benchmark_i %>%
        dplyr::mutate(bank_id = .env$i)

      sda_result_for_aggregation_benchmark <- sda_result_for_aggregation_benchmark %>%
        dplyr::bind_rows(sda_result_for_aggregation_benchmark_i)

    },
    error = function(e) {
      log_text <- glue::glue("{Sys.time()} - bank: {i} Problem in preparing data for benchmark aggregation. Skipping! \n")
      write(log_text, file = file.path(output_directory_p4b_aggregated, "error_messages.txt"), append = TRUE)
    }
  )
}

# bind the SDA results from the loan book and benchmark PACTA runs for further
# aggregation
sda_result_for_aggregation <- sda_result_for_aggregation %>%
  dplyr::bind_rows(sda_result_for_aggregation_benchmark)

## aggregate SDA P4B results to company level alignment score----
# calculate aggregation for the loan book
# temporary fix for the scenario name issue in geco_2021, relates to https://github.com/RMI-PACTA/r2dii.analysis/issues/425
if (scenario_source_input == "geco_2021" & scenario_select == "1.5c") {scenario_select_sda <- "1.5c-unif"} else {scenario_select_sda <- scenario_select}

sda_aggregated <- sda_result_for_aggregation %>%
  calculate_company_aggregate_score_sda(
    scenario_emission_intensities = scenario_input_sda,
    scenario_source = scenario_source_input,
    scenario = scenario_select_sda
  )

sda_aggregated %>%
  readr::write_csv(file.path(output_directory_p4b_aggregated, "sda_aggregated_company.csv"))


## calculate sector and loan book level aggregate alignment based on company exposures in loan book----

# the company level aggregate scores are then joined with the matched loan book
# to derive some high level summary statistics on the loan book level
companies_aggregated <- tms_aggregated %>%
  dplyr::bind_rows(sda_aggregated)

# show exposures (n companies and loan size) by alignment with given scenario

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

# bespoke plots for supervisory analysis----

# Plot sankey plot of financial flows scenario alignment - examples

if (!is.null(tms_aggregated)) {
data_sankey_tms <- prep_sankey(
  tms_aggregated,
  matched_loanbook,
  region = "global",
  year = 2027,
  middle_node = "sector"
  )
} else {
  data_sankey_tms <- NULL
}

if (!is.null(sda_aggregated)) {
  data_sankey_sda <- prep_sankey(
  sda_aggregated,
  matched_loanbook,
  region = "global",
  year = 2027,
  middle_node = "sector"
  )
} else {
  data_sankey_sda <- NULL
}

data_sankey <- rbind(data_sankey_tms, data_sankey_sda)

plot_sankey(data_sankey, save_png_to = output_directory_p4b_aggregated, png_name = "sankey_sector.png")

if (!is.null(tms_aggregated)) {
data_sankey_tms2 <- prep_sankey(
  tms_aggregated,
  matched_loanbook,
  region = "global",
  year = 2027,
  middle_node = "name_abcd",
  middle_node2 = "sector"
  )
} else {
  data_sankey_tms2 <- NULL
}

if (!is.null(sda_aggregated)) {
  data_sankey_sda2 <- prep_sankey(
  sda_aggregated,
  matched_loanbook,
  region = "global",
  year = 2027,
  middle_node = "name_abcd",
  middle_node2 = "sector"
  )
} else {
  data_sankey_sda2 <- NULL
}

data_sankey2 <- rbind(data_sankey_tms2, data_sankey_sda2)

plot_sankey(data_sankey2, save_png_to = output_directory_p4b_aggregated, png_name = "sankey_company_sector.png")

# Plot timeline of evolution of portfolio-weighted alignment over time - examples

# build-out / phase-out for power
sector_timeline <- "power"
region_timeline <- "global"
data_timeline <- prep_timeline(
  aggregate_exposure_loanbook_bopo,
  sector = sector_timeline,
  region = region_timeline,
  bank_ids_to_plot = "bank1")
plot_timeline(
  data_timeline,
  sector = sector_timeline,
  scenario_source = scenario_source_input,
  scenario = scenario_select,
  region = region_timeline
  )
ggsave(
  filename = "timeline_bopo_power.png",
  path = output_directory_p4b_aggregated,
  width = 8,
  height = 5
  )

# net score for cement
sector_timeline <- "cement"
region_timeline <- "global"
data_timeline <- prep_timeline(
  aggregate_exposure_loanbook,
  sector = sector_timeline,
  region = region_timeline,
  bank_ids_to_plot = "bank1")
plot_timeline(
  data_timeline,
  sector = sector_timeline,
  scenario_source = scenario_source_input,
  scenario = scenario_select,
  region = region_timeline
  )

ggsave(
  filename = "timeline_cement.png",
  path = output_directory_p4b_aggregated,
  width = 7,
  height = 5
  )

# Plot scatterplot of alignment scores - examples

# company level, excluding outliers
year_scatter <- 2027
sector_scatter <- "power"
region_scatter <- "global"
data_level1 <- "company"
data_scatter <- prep_scatter(
  tms_aggregated_buildout_phaseout,
  tms_aggregated,
  year = year_scatter,
  sector = sector_scatter,
  region = region_scatter,
  bank_ids_to_plot = "bank1",
  data_level = data_level1
  )
plot_scatter(
  data_scatter,
  data_level = data_level1,
  year = year_scatter,
  sector = sector_scatter,
  region = region_scatter,
  scenario_source = scenario_source_input,
  scenario = scenario_select,
  cap_outliers = 2,
  floor_outliers = -2
  )

# bank level
data_level2 <- "bank"
data_scatter2 <- prep_scatter(
  aggregate_exposure_loanbook_bopo,
  aggregate_exposure_loanbook,
  year = year_scatter,
  sector = sector_scatter,
  region = region_scatter,
  data_level = data_level2
  )
plot_scatter(
  data_scatter2,
  data_level = data_level2,
  year = year_scatter,
  sector = sector_scatter,
  region = region_scatter,
  scenario_source = scenario_source_input,
  scenario = scenario_select
  )
