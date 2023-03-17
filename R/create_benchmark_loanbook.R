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
#' @param benchmark_region character vector of length 1. Select the region based
#'   on which the benchmark loan book should be created. Only companies with
#'   production within the selceted region will be kept.
#'
#' @return NULL
#' @export
create_benchmark_loanbook <- function(data,
                                      scenario_source,
                                      start_year,
                                      benchmark_region) {

  # check input data
  validate_data_has_expected_cols(
    data = data,
    expected_columns <- c(
      "company_id", "name_company", "lei", "is_ultimate_owner", "sector",
      "technology", "plant_location", "year", "production", "production_unit",
      "emission_factor", "emission_factor_unit", "ald_timestamp"
    )
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
  benchmark_countries <- r2dii.data::region_isos %>%
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
    bank_id = paste0("benchmark_corporate_economy_", benchmark_region)
  ) %>%
    tibble::rowid_to_column() %>%
    dplyr::mutate(id_loan = paste0("L", .data$rowid)) %>%
    dplyr::select(-"rowid")

  return(loanbook_benchmark)
}
