#' Return raw loan book containing the corporate economy benchmark
#'
#' @param data data frame containing the asset-based company data (ABCD) in
#'   PACTA for Banks format.
#' @param scenario_source character vector of length 1. This is used to subset
#'   the allowed regions as defined in `r2dii.data::region_isos`. It is
#'   recommended to simply use the scenario_sourc_input used throughout the
#'   workflow.
#' @param start_year character vector of length 1. Defines the initial year of
#'   the analysis. The company weights will be picked based on the production
#'   capacity in the `start_year`.
#' @param region_isos data frame containing the regional mapping for scenarios
#'   to country iso codes, following the format of `r2dii.data::region_isos`
#' @param benchmark_region character vector of length 1. Select the region based
#'   on which the benchmark loan book should be created. Only companies with
#'   production within the selceted region will be kept.
#'
#' @return NULL
#' @export
create_benchmark_loanbook <- function(data,
                                      scenario_source,
                                      start_year,
                                      region_isos,
                                      benchmark_region) {

  # validate inputs
  validate_input_create_benchmark_loanbook(
    data = data,
    scenario_source = scenario_source,
    start_year = start_year,
    region_isos = region_isos,
    benchmark_region = benchmark_region
  )

  # get NACE sector codes to use in raw loan book of corporate benchmark
  benchmark_sectors <- r2dii.data::nace_classification %>%
    dplyr::filter(.data$borderline == FALSE) %>%
    dplyr::group_by(.data$sector) %>%
    dplyr::slice_max(.data$code_level, n = 1) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select("sector", "code")

  # get corporates from ABCD to use in benchmark raw loan book
  benchmark_companies <- data %>%
    dplyr::filter(.data$is_ultimate_owner) %>%
    dplyr::inner_join(
      benchmark_sectors,
      by = "sector"
    ) %>%
    dplyr::distinct(.data$company_id, .data$name_company, .data$lei, .data$code)

  # keep only the companies that have production in the provided region
  benchmark_countries <- region_isos %>%
    dplyr::filter(
      .data$source == .env$scenario_source,
      .data$region == .env$benchmark_region,
    ) %>%
    dplyr::pull(.data$isos) %>%
    toupper()

  benchmark_companies <- data %>%
    dplyr::filter(
      .data$is_ultimate_owner,
      .data$plant_location %in% .env$benchmark_countries,
      .data$year == .env$start_year
    ) %>%
    dplyr::inner_join(
      benchmark_sectors,
      by = "sector"
    ) %>%
    dplyr::summarise(
      production_sector = sum(.data$production, na.rm = TRUE),
      .by = c("company_id", "name_company", "lei", "code")
    )

  # create raw loan book of corporate benchmark according to standard PACTA for
  # Banks raw loan book data structure
  loanbook_benchmark <- tibble::tibble(
    id_direct_loantaker = paste0("C", benchmark_companies$company_id),
    name_direct_loantaker = benchmark_companies$name_company,
    id_intermediate_parent_1 = NA_character_,
    name_intermediate_parent_1 = NA_character_,
    id_ultimate_parent = paste0("UP", benchmark_companies$company_id),
    name_ultimate_parent = benchmark_companies$name_company,
    loan_size_outstanding = benchmark_companies$production_sector,
    loan_size_outstanding_currency = "USD",
    loan_size_credit_limit = benchmark_companies$production_sector,
    loan_size_credit_limit_currency = "USD",
    sector_classification_system = "NACE",
    sector_classification_input_type = "Code",
    sector_classification_direct_loantaker = as.numeric(benchmark_companies$code),
    fi_type = "Loan",
    flag_project_finance_loan = "No",
    name_project = NA_character_,
    lei_direct_loantaker = benchmark_companies$lei,
    isin_direct_loantaker = NA_character_,
    group_id = paste0("benchmark_corporate_economy_", benchmark_region)
  ) %>%
    tibble::rowid_to_column() %>%
    dplyr::mutate(id_loan = paste0("L", .data$rowid)) %>%
    dplyr::select(-"rowid")

  return(loanbook_benchmark)
}

validate_input_create_benchmark_loanbook <- function(data,
                                                     scenario_source,
                                                     start_year,
                                                     region_isos,
                                                     benchmark_region) {
  # validate input arguments
  validate_input_args_create_benchmark_loanbook(
    scenario_source = scenario_source,
    start_year = start_year,
    benchmark_region = benchmark_region
  )

  # validate input data sets
  validate_input_data_create_benchmark_loanbook(
    data = data,
    region_isos = region_isos
  )

  # consistency checks
  if (!scenario_source %in% unique(region_isos$source)) {
    stop(
      paste0(
        "input value of `scenario_source` not found in `region_isos$source`. You provided ",
        scenario_source,". Available values are: ",
        toString(unique(region_isos$source))
      )
    )
  }
  available_regions <- region_isos %>%
    dplyr::filter(.data$source == .env$scenario_source)

  if (!benchmark_region %in% unique(available_regions$region)) {
    stop(
      paste0(
        "input value of `benchmark_region` not found in `region_isos$region` for the given `scenario_source`. You provided ",
        benchmark_region,". Available values are: ",
        toString(unique(available_regions$region)), ". For the selected scenario_source: ",
        scenario_source, ", the following input region cannot be found: ",
        setdiff(benchmark_region, unique(available_regions$region)),
        ". Please check the regions data for updates or select another benchmark region."
      )
    )
  }
  if (!start_year %in% unique(data$year)) {
    stop(
      paste0(
        "input value of `start_year` not found in `data$year`. You provided ",
        start_year,". Available values are: ", toString(unique(data$year))
      )
    )
  }

  invisible()
}

validate_input_args_create_benchmark_loanbook <- function(scenario_source,
                                                          start_year,
                                                          benchmark_region) {
  if (!length(scenario_source) == 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
  if (!inherits(scenario_source, "character")) {
    stop("Argument scenario_source must be of class character. Please check your input.")
  }
  if (!length(start_year) == 1) {
    stop("Argument start_year must be of length 1. Please check your input.")
  }
  if (!inherits(start_year, "numeric")) {
    stop("Argument start_year must be of class character. Please check your input.")
  }
  if (!length(benchmark_region) > 0) {
    stop("Argument benchmark_region must have at least one entry, you provided 0. Please check your input.")
  }
  if (!inherits(benchmark_region, "character")) {
    stop("Argument benchmark_region must be of class character. Please check your input.")
  }

  invisible()
}

validate_input_data_create_benchmark_loanbook <- function(data,
                                                          region_isos) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns <- c(
      "company_id", "name_company", "lei", "is_ultimate_owner", "sector",
      "technology", "plant_location", "year", "production", "production_unit",
      "emission_factor", "emission_factor_unit", "ald_timestamp"
    )
  )

  validate_data_has_expected_cols(
    data = region_isos,
    expected_columns = c(
      "region", "isos", "source"
    )
  )

  invisible()
}

