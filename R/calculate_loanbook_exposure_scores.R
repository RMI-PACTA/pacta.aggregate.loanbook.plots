#' Return loan book level aggregation of company scores by exposure
#'
#' @param data data.frame. Holds output pf company indicators
#' @param matched data.frame. Holds matched and priortised loan book
#' @param aggregate Logical. Indicates whether the indicators should be
#'   calculated for an aggregate of all loan books by different banks in
#'   `matched` or if they should be calculated individually, based on their
#'   `bank_id`. If only one loan book is included, use the default
#'   aggregate == TRUE.
#'
#' @return NULL
#' @export
calculate_loanbook_exposure_scores <- function(data,
                                               matched,
                                               aggregate = TRUE) {
  if (!is.logical(aggregate)) {
    stop("Function argument aggregate must be either TRUE or FALSE!")
  }

  if (!"bank_id" %in% names(matched) & !aggregate) {
    stop("The input macthed data set does not contain bank identifiers, which
         are needed to process multiple loan books on an individual level")
  } else if (!"bank_id" %in% names(matched) & aggregate) {
    matched <- matched %>%
      dplyr::mutate(bank_id = unique(data$bank_id))
  }

  matched <- matched %>%
    dplyr::select(
      c("bank_id", "id_loan", "loan_size_outstanding", "loan_size_outstanding_currency", "name_abcd", "sector")
    ) %>%
    dplyr::group_by(
      .data$bank_id, .data$loan_size_outstanding_currency, .data$name_abcd, .data$sector
    ) %>%
    dplyr::summarise(
      loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$bank_id, .data$loan_size_outstanding_currency
    ) %>%
    dplyr::mutate(
      exposure_weight = .data$loan_size_outstanding / sum(.data$loan_size_outstanding, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()


  aggregate_exposure_company <- matched %>%
    dplyr::inner_join(
      data,
      by = c("bank_id", "name_abcd", "sector")
    )

  total_aggregate_exposure_loanbook <- aggregate_exposure_company %>%
    dplyr::group_by(.data$bank_id, .data$region, .data$scenario, .data$year, .data$direction) %>%
    dplyr::mutate(
      n_companies = dplyr::n(),
      sum_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      companies_aligned = dplyr::if_else(.data$score >= 0, TRUE, FALSE),
      exposure_companies_aligned = dplyr::if_else(.data$score >= 0, .data$loan_size_outstanding, 0)
    ) %>%
    dplyr::group_by(
      .data$bank_id, .data$n_companies, .data$sum_loan_size_outstanding,
      .data$scenario, .data$region, .data$year, .data$direction
    ) %>%
    dplyr::summarise(
      n_companies_aligned = sum(.data$companies_aligned, na.rm = TRUE),
      sum_exposure_companies_aligned = sum(.data$exposure_companies_aligned, na.rm = TRUE),
      exposure_weighted_net_alignment = stats::weighted.mean(.data$score, w = .data$exposure_weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      share_companies_aligned = .data$n_companies_aligned / .data$n_companies,
      share_exposure_aligned = .data$sum_exposure_companies_aligned / .data$sum_loan_size_outstanding
    ) %>%
    dplyr::mutate(sector = "total")

  sector_aggregate_exposure_loanbook <- aggregate_exposure_company %>%
    dplyr::group_by(.data$bank_id, .data$region, .data$scenario, .data$sector, .data$year, .data$direction) %>%
    dplyr::mutate(
      n_companies = dplyr::n(),
      sum_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      companies_aligned = dplyr::if_else(.data$score >= 0, TRUE, FALSE),
      exposure_companies_aligned = dplyr::if_else(.data$score >= 0, .data$loan_size_outstanding, 0)
    ) %>%
    dplyr::group_by(
      .data$bank_id, .data$n_companies, .data$sum_loan_size_outstanding,
      .data$scenario, .data$region, .data$sector, .data$year, .data$direction
    ) %>%
    dplyr::summarise(
      n_companies_aligned = sum(.data$companies_aligned, na.rm = TRUE),
      sum_exposure_companies_aligned = sum(.data$exposure_companies_aligned, na.rm = TRUE),
      exposure_weighted_net_alignment = stats::weighted.mean(.data$score, w = .data$exposure_weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      share_companies_aligned = .data$n_companies_aligned / .data$n_companies,
      share_exposure_aligned = .data$sum_exposure_companies_aligned / .data$sum_loan_size_outstanding
    )

  out <- sector_aggregate_exposure_loanbook %>%
    dplyr::bind_rows(total_aggregate_exposure_loanbook) %>%
    dplyr::relocate(
      c(
        "bank_id", "scenario", "region", "sector", "year", "direction",
        "n_companies", "n_companies_aligned", "share_companies_aligned",
        "sum_loan_size_outstanding", "sum_exposure_companies_aligned",
        "share_exposure_aligned", "exposure_weighted_net_alignment"
      )
    ) %>%
    dplyr::arrange(.data$bank_id, .data$scenario, .data$region, .data$sector, .data$year)

  return(out)
}

validate_input_data_calculate_loanbook_exposure_scores <- function(data,
                                                                   matched) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns <- c(
      "bank_id", "name_abcd", "sector", "region", "scenario_source", "scenario",
      "year", "score"
    )
  )

  validate_data_has_expected_cols(
    data = matched,
    expected_columns <- c(
      "bank_id", "id_loan", "loan_size_outstanding",
      "loan_size_outstanding_currency", "name_abcd", "sector"
    )
  )

  invisible()
}
