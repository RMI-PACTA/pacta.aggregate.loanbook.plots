#' Return raw loan book containing the corporate economy benchmark
#'
#' @param data data frame containing the asset-based company data (ABCD) in
#'   PACTA for Banks format.
#'
#' @return NULL
#' @export
create_benchmark_loanbook <- function(data) {

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

  # create raw loan book of corporate benchmark according to standard PACTA for
  # Banks raw loan book data structure
  loanbook_benchmark <- tibble::tibble(
    id_direct_loantaker = paste0("C", benchmark_companies$company_id),
    name_direct_loantaker = benchmark_companies$name_company,
    id_intermediate_parent_1 = NA_character_,
    name_intermediate_parent_1 = NA_character_,
    id_ultimate_parent = paste0("UP", benchmark_companies$company_id),
    name_ultimate_parent = benchmark_companies$name_company,
    loan_size_outstanding = 100000,
    loan_size_outstanding_currency = "USD",
    loan_size_credit_limit = 100000,
    loan_size_credit_limit_currency = "USD",
    sector_classification_system = "NACE",
    sector_classification_input_type = "Code",
    sector_classification_direct_loantaker = as.numeric(benchmark_companies$code),
    fi_type = "Loan",
    flag_project_finance_loan = "No",
    name_project = NA_character_,
    lei_direct_loantaker = benchmark_companies$lei,
    isin_direct_loantaker = NA_character_,
    bank_id = "benchmark_corporate_economy"
  ) %>%
    tibble::rowid_to_column() %>%
    dplyr::mutate(id_loan = paste0("L", .data$rowid)) %>%
    dplyr::select(-"rowid")

  return(loanbook_benchmark)
}
