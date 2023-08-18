#' Return loan book level aggregation of company alignment metrics by exposure
#'
#' @param data data.frame. Holds output pf company indicators
#' @param matched data.frame. Holds matched and prioritised loan book
#'
#' @return NULL
#' @export
aggregate_alignment_loanbook_exposure <- function(data,
                                                  matched) {
  # validate input data sets
  validate_input_data_aggregate_alignment_loanbook_exposure(
    data = data,
    matched = matched
  )

  matched <- matched %>%
    dplyr::select(
      dplyr::all_of(
        c("group_id", "id_loan", "loan_size_outstanding", "loan_size_outstanding_currency", "name_abcd", "sector")
      )
    ) %>%
    dplyr::summarise(
      loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
      .by = c("group_id", "loan_size_outstanding_currency", "name_abcd", "sector")
    ) %>%
    dplyr::mutate(
      exposure_weight = .data$loan_size_outstanding / sum(.data$loan_size_outstanding, na.rm = TRUE),
      .by = c("group_id", "loan_size_outstanding_currency")
    )

  aggregate_exposure_company <- data %>%
    dplyr::inner_join(
      matched,
      by = c("group_id", "name_abcd", "sector")
    )

  sector_aggregate_exposure_loanbook_summary <- aggregate_exposure_company %>%
    dplyr::mutate(
      n_companies = dplyr::n(),
      sum_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
      .by = c("group_id", "region", "scenario", "sector", "year", "direction")
    ) %>%
    dplyr::mutate(
      companies_aligned = dplyr::if_else(.data$alignment_metric >= 0, TRUE, FALSE),
      exposure_companies_aligned = dplyr::if_else(.data$alignment_metric >= 0, .data$loan_size_outstanding, 0)
    ) %>%
    dplyr::summarise(
      n_companies_aligned = sum(.data$companies_aligned, na.rm = TRUE),
      sum_exposure_companies_aligned = sum(.data$exposure_companies_aligned, na.rm = TRUE),
      .by = c(
        "group_id", "n_companies", "sum_loan_size_outstanding",
        "scenario", "region", "sector", "year", "direction"
      )
    ) %>%
    dplyr::mutate(
      share_companies_aligned = .data$n_companies_aligned / .data$n_companies,
      share_exposure_aligned = .data$sum_exposure_companies_aligned / .data$sum_loan_size_outstanding
    )

  # if a company only has technologies going in one direction in a sector with
  # high carbon and low carbon technologies, add an empty entry for the other
  # direction to ensure the aggregation is correct
  if (all(unique(aggregate_exposure_company$direction) %in% c("buildout", "phaseout"))) {
    aggregate_exposure_company <- aggregate_exposure_company %>%
      dplyr::mutate(
        n_directions = dplyr::n(),
        .by = c(
          "group_id", "name_abcd", "sector", "activity_unit", "region",
          "scenario_source", "scenario", "year", "loan_size_outstanding_currency"
        )
      )

    single_direction <- aggregate_exposure_company %>%
      dplyr::filter(
        n_directions == 1,
        .data$direction %in% c("buildout", "phaseout"),
        .data$sector %in% c("automotive", "hdv", "power")
      )

    opposite_direction <- single_direction %>%
      dplyr::mutate(
        direction = dplyr::if_else(
          .data$direction == "buildout",
          "phaseout",
          "buildout"
        ),
        total_deviation = 0,
        alignment_metric = 0
      )

    aggregate_exposure_company <- aggregate_exposure_company %>%
      bind_rows(opposite_direction) %>%
      dplyr::select(-"n_directions")
  }

  sector_aggregate_exposure_loanbook_alignment <- aggregate_exposure_company %>%
    dplyr::summarise(
      exposure_weighted_net_alignment = stats::weighted.mean(.data$alignment_metric, w = .data$exposure_weight, na.rm = TRUE),
      .by = c(
        "group_id", "scenario", "region", "sector", "year", "direction"
      )
    )

  out <- sector_aggregate_exposure_loanbook_summary %>%
    dplyr::inner_join(
      sector_aggregate_exposure_loanbook_alignment,
      by = c("group_id", "scenario", "region", "sector", "year", "direction")
    ) %>%
    dplyr::relocate(
      c(
        "group_id", "scenario", "region", "sector", "year", "direction",
        "n_companies", "n_companies_aligned", "share_companies_aligned",
        "sum_loan_size_outstanding", "sum_exposure_companies_aligned",
        "share_exposure_aligned", "exposure_weighted_net_alignment"
      )
    ) %>%
    dplyr::arrange(.data$group_id, .data$scenario, .data$region, .data$sector, .data$year)

  return(out)
}

validate_input_data_aggregate_alignment_loanbook_exposure <- function(data,
                                                                      matched) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns <- c(
      "group_id", "name_abcd", "sector", "activity_unit", "region",
      "scenario_source", "scenario", "year", "direction", "alignment_metric"
    )
  )

  validate_data_has_expected_cols(
    data = matched,
    expected_columns <- c(
      "group_id", "id_loan", "loan_size_outstanding",
      "loan_size_outstanding_currency", "name_abcd", "sector"
    )
  )

  invisible()
}
