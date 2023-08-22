#' Return loan book level aggregation of company alignment metrics by exposure
#'
#' @param data data.frame. Holds output pf company indicators
#' @param matched data.frame. Holds matched and prioritised loan book
#' @param level Character. Vector that indicates if the aggregate alignment
#'   metric should be returned based on the net technology deviations (`net`) or
#'   disaggregated into buildout and phaseout technologies (`bo_po`).
#'
#' @return NULL
#' @export
aggregate_alignment_loanbook_exposure <- function(data,
                                                  matched,
                                                  level = c("net", "bo_po")) {
  group_vars <- c("group_id", "scenario", "region", "sector", "year", "direction")
  level <- rlang::arg_match(level)

  # validate input data sets
  validate_input_data_aggregate_alignment_loanbook_exposure(
    data = data,
    matched = matched,
    group_vars = group_vars
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
      .by = dplyr::all_of(group_vars)
    ) %>%
    dplyr::mutate(
      companies_aligned = dplyr::if_else(.data$alignment_metric >= 0, TRUE, FALSE)
    ) %>%
    dplyr::summarise(
      n_companies_aligned = sum(.data$companies_aligned, na.rm = TRUE),
      .by = dplyr::all_of(c(group_vars, "n_companies"))
    ) %>%
    dplyr::mutate(
      share_companies_aligned = .data$n_companies_aligned / .data$n_companies
    )

  # aggregate exposure of aligned companies can only be calculated reasonably
  # for the net level, not the buildout/phaseout level since we cannot assume
  # the loan is split based on output units.
  if (level == "net") {
    sector_aggregate_exposure_loanbook_summary_value <- aggregate_exposure_company %>%
      dplyr::mutate(
        sum_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
        .by = group_vars
      ) %>%
      dplyr::mutate(
        exposure_companies_aligned = dplyr::if_else(.data$alignment_metric >= 0, .data$loan_size_outstanding, 0)
      ) %>%
      dplyr::summarise(
        sum_exposure_companies_aligned = sum(.data$exposure_companies_aligned, na.rm = TRUE),
        .by = dplyr::all_of(c(group_vars, "sum_loan_size_outstanding"))
      ) %>%
      dplyr::mutate(
        share_exposure_aligned = .data$sum_exposure_companies_aligned / .data$sum_loan_size_outstanding
      )

    sector_aggregate_exposure_loanbook_summary <- sector_aggregate_exposure_loanbook_summary %>%
      dplyr::inner_join(
        sector_aggregate_exposure_loanbook_summary_value,
        by = group_vars
      )
  }

  # if a company only has technologies going in one direction in a sector with
  # high carbon and low carbon technologies, add an empty entry for the other
  # direction to ensure the aggregation is correct
  if (level == "bo_po") {
    aggregate_exposure_company <- aggregate_exposure_company %>%
      dplyr::mutate(
        n_directions = dplyr::n(),
        .by = dplyr::all_of(
          c(
            group_vars[!group_vars == "direction"], "name_abcd", "sector",
            "activity_unit", "loan_size_outstanding_currency"
          )
        )
      )

    single_direction <- aggregate_exposure_company %>%
      dplyr::filter(
        .data$n_directions == 1,
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
      .by = dplyr::all_of(group_vars)
    )

  out <- sector_aggregate_exposure_loanbook_summary %>%
    dplyr::inner_join(
      sector_aggregate_exposure_loanbook_alignment,
      by = group_vars
    ) %>%
    dplyr::relocate(
      c(
        group_vars, "n_companies", "n_companies_aligned",
        "share_companies_aligned", "exposure_weighted_net_alignment"
      )
    ) %>%
    dplyr::arrange(.data$group_id, .data$scenario, .data$region, .data$sector, .data$year)

  return(out)
}

validate_input_data_aggregate_alignment_loanbook_exposure <- function(data,
                                                                      matched,
                                                                      group_vars) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns <- c(
      group_vars, "name_abcd", "activity_unit", "scenario_source", "alignment_metric"
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
