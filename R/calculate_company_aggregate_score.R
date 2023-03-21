#' Return company level technology deviations for TMS sectors. To be used as
#' input into calculation of company level aggregate scores for production
#' trajectory sectors.
#'
#' @param data data.frame. Holds the PACTA for Banks TMS results. Must have been
#'   calculated according to the green/brown logic of the CA100+ calculation
#'   and must return unweighted company level TMSR results.
#' @param technology_direction data frame that indicates which technologies are
#'   to be considered phase down technologies versus build out technologies
#' @param scenario_trajectory data frame containing the scenario file with
#'   information on yearly tmsr and smsp changes by scenario and region.
#' @param green_or_brown data frame. Indicates which technologies are to use
#'   tmsr versus smsp
#' @param scenario_source Character. Vector that indicates which scenario_source
#'   to use for reference in the calculation of the scores. Currently, the only
#'   supported value is `"geco_2021"`.
#' @param scenario Character. Vector that indicates which scenario to calculate
#'   the score for. Must be a scenario available from `scenario_source`.
#' @param bridge_tech Character. Vector that indicates if a technology is
#'   considered a bridge technology. I.e. if the scenario requires a temporary
#'   build out despite the need for a long term phase down. If so, the alignment
#'   score can be treated differently than for other technologies. Currently,
#'   this is only supported for gas fired power generation (`"gascap"`). Default
#'   is `NULL` which means that no special treatment is applied.
#' @param aggregate Logical. Indicates whether the indicators should be
#'   calculated for an aggregate of all loan books by different banks in `data`
#'   or if they should be calculated individually, based on their
#'   `bank_id`. If only one loan book is included, use the default
#'   aggregate == TRUE.
#'
#' @return NULL
#' @export
calculate_company_tech_deviation <- function(data,
                                             technology_direction,
                                             scenario_trajectory,
                                             green_or_brown,
                                             scenario_source = "geco_2021",
                                             scenario = "1.5c",
                                             bridge_tech = NULL,
                                             aggregate = TRUE) {

  # validate input values
  validate_input_args_calculate_company_tech_deviation(
    scenario_source = scenario_source,
    scenario = scenario
  )

  # validate input data sets
  validate_input_data_calculate_company_tech_deviation(
    data = data,
    technology_direction = technology_direction,
    scenario_trajectory = scenario_trajectory,
    green_or_brown = green_or_brown
  )

  start_year <- min(data$year, na.rm = TRUE)
  target_scenario <- paste0("target_", scenario)
  bridge_tech <- bridge_tech %||% "skip"

  technology_direction <- technology_direction %>%
    dplyr::filter(
      .data$scenario_source == .env$scenario_source,
      grepl(pattern = .env$scenario, x = .data$scenario)
    ) %>%
    dplyr::select(c("sector", "technology", "region", "directional_dummy"))

  if (length(bridge_tech) > 1) {
    stop("Function argument bridge_tech must be a character vector of length 1")
  }

  if (!is.logical(aggregate)) {
    stop("Function argument aggregate must be either TRUE or FALSE!")
  }

  if (!"bank_id" %in% names(data) & !aggregate) {
    stop("The input data set does not contain bank identifiers, which are needed
         to process multiple loan books on an individual level")
  } else if (!"bank_id" %in% names(data) & aggregate) {
    data <- data %>%
      dplyr::mutate(bank_id = "bank")
  }

  data <- data %>%
    dplyr::filter(.data$scenario_source == .env$scenario_source)

  data <- data %>%
    dplyr::select(-c("technology_share", "scope", "percentage_of_initial_production_by_scope")) %>%
    dplyr::filter(.data$metric %in% c("projected", paste0("target_", .env$scenario))) %>%
    dplyr::filter(dplyr::between(.data$year, left = .env$start_year, right = .env$start_year + 5)) %>%
    tidyr::pivot_wider(
      names_from = "metric",
      values_from = "production"
    )

  # add directional dummy
  data <- data %>%
    dplyr::inner_join(technology_direction, by = c("sector", "technology", "region"))

  # remove rows if both projected and target values are 0
  data_to_remove_no_plans_no_target_tech <- data %>%
    dplyr::group_by(
      .data$bank_id, .data$name_abcd, .data$region, .data$scenario_source,
      .data$sector, .data$technology, .data$year
    ) %>%
    dplyr::rename(target = !!rlang::sym(target_scenario)) %>%
    dplyr::summarise(
      projected = sum(.data$projected, na.rm = TRUE),
      target = sum(.data$target, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      .data$projected == 0,
      .data$target == 0
    )

  data_to_remove_no_target_in_sector <- data %>%
    dplyr::group_by(.data$bank_id, .data$name_abcd, .data$region, .data$scenario_source, .data$sector, .data$year) %>%
    dplyr::rename(target = !!rlang::sym(target_scenario)) %>%
    dplyr::summarise(
      target = sum(.data$target, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$target == 0)

  data <- data %>%
    dplyr::anti_join(
      data_to_remove_no_plans_no_target_tech,
      by = c("bank_id", "name_abcd", "region", "scenario_source", "sector", "technology", "year")
    )

  data <- data %>%
    dplyr::anti_join(
      data_to_remove_no_target_in_sector,
      by = c("bank_id", "name_abcd", "region", "scenario_source", "sector", "year")
    )

  # calculate total deviation per technology
  data <- data %>%
    dplyr::mutate(
      total_tech_deviation = (.data$projected - !!rlang::sym(target_scenario)) * .data$directional_dummy
    )

  # if gas_cap is a bridge tech, both sides of the scenario are treated as misaligned
  if (bridge_tech == "gascap") {
    data_cap <- data %>%
      dplyr::filter(.data$technology == "gascap")

    data_cap <- data_cap %>%
      dplyr::mutate(
        total_tech_deviation = dplyr::case_when(
          .data$projected < !!rlang::sym(target_scenario) ~ .data$projected - !!rlang::sym(target_scenario),
          .data$projected > !!rlang::sym(target_scenario) ~ (.data$projected - !!rlang::sym(target_scenario)) * -1,
          TRUE ~ 0
        )
      )

      data <- data %>%
        dplyr::filter(.data$technology != .env$bridge_tech) %>%
        dplyr::bind_rows(data_cap)
  }

  # add direction
  data <- data %>%
    dplyr::mutate(direction = dplyr::if_else(.data$directional_dummy == 1, "buildout", "phaseout")) %>%
    dplyr::select(-"directional_dummy")

  # add activity unit
  data <- data %>%
    dplyr::inner_join(
      activity_units,
      by = c("sector", "technology")
    )

  # add technology share by direction
  data <- data %>%
    dplyr::mutate(
      prod_sector = sum(.data$projected, na.rm = TRUE),
      .by = c("sector", "year", "region", "scenario_source", "name_abcd", "bank_id", "activity_unit")
    ) %>%
    dplyr::mutate(
      technology_share_by_direction = sum(.data$projected, na.rm = TRUE) / .data$prod_sector,
      .by = c("sector", "year", "region", "scenario_source", "name_abcd", "bank_id", "direction", "activity_unit")
    ) %>%
    dplyr::select(-"prod_sector")

  return(data)
}


#' Return company level scenario scores for the main sector of each company with
#' opton to disaggregate by buidlout / phaseout.
#'
#' @param data data.frame. Holds company-technology deviations based on PACTA
#'   for Banks TMS results. Must have been calculated according to the
#'   green/brown logic of the CA100+ calculation.
#' @param scenario_source Character. Vector that indicates which scenario_source
#'   to use for reference in the calculation of the scores. Currently, the only
#'   supported value is `"geco_2021"`.
#' @param scenario Character. Vector that indicates which scenario to calculate
#'   the score for. Must be a scenario available from `scenario_source`.
#' @param level Character. Vector that indicates if the aggreagte score should
#'   be returned based on the net technology deviations (`net`) or disaggregated
#'   into buildout and phaseout technologies (`bo_po`).
#' @param aggregate Logical. Indicates whether the indicators should be
#'   calculated for an aggregate of all loan books by different banks in `data`
#'   or if they should be calculated individually, based on their
#'   `bank_id`. If only one loan book is included, use the default
#'   aggregate == TRUE.
#'
#' @return NULL
#' @export
calculate_company_aggregate_score_tms <- function(data,
                                                  scenario_source = "geco_2021",
                                                  scenario = "1.5c",
                                                  level = c("net", "bo_po"),
                                                  aggregate = TRUE) {

  # validate input values
  validate_input_args_calculate_company_aggregate_score_tms(
    scenario_source = scenario_source,
    scenario = scenario
  )

  # validate input data set
  validate_input_data_calculate_company_aggregate_score_tms(
    data = data,
    scenario = scenario
  )

  start_year <- min(data$year, na.rm = TRUE)
  target_scenario <- paste0("target_", scenario)
  level <- match.arg(level)


  if (level == "bo_po") {
    # calculate buildout and phaseout sector score
    data <- data %>%
      dplyr::mutate(
        net_absolute_scenario_value = sum(!!rlang::sym(target_scenario), na.rm = TRUE),
        .by = c("bank_id", "name_abcd", "scenario_source", "region", "sector", "activity_unit", "year")
      ) %>%
      dplyr::summarise(
        total_deviation = sum(.data$total_tech_deviation, na.rm = TRUE),
        absolute_scenario_value = sum(!!rlang::sym(target_scenario), na.rm = TRUE),
        .by = c("bank_id", "name_abcd", "scenario_source", "region", "sector", "activity_unit", "year", "net_absolute_scenario_value", "direction", "technology_share_by_direction")
      ) %>%
      dplyr::mutate(
        score = .data$total_deviation / .data$net_absolute_scenario_value,
        scenario = .env$scenario
      ) %>%
      dplyr::select(
        c(
          "bank_id", "name_abcd", "sector", "activity_unit", "region",
          "scenario_source", "scenario", "year", "direction", "total_deviation",
          "technology_share_by_direction", "score"
        )
      )

  } else if (level == "net") {
    # calculate net sector score
    data <- data %>%
      dplyr::summarise(
        total_deviation = sum(.data$total_tech_deviation, na.rm = TRUE),
        net_absolute_scenario_value = sum(!!rlang::sym(target_scenario), na.rm = TRUE),
        .by = c("bank_id", "name_abcd", "scenario_source", "region", "sector", "activity_unit", "year")
      ) %>%
      dplyr::mutate(
        score = .data$total_deviation / .data$net_absolute_scenario_value,
        scenario = .env$scenario,
        direction = .env$level
      ) %>%
      dplyr::select(
        c(
          "bank_id", "name_abcd", "sector", "activity_unit", "region",
          "scenario_source", "scenario", "year", "direction", "total_deviation",
          "score"
        )
      )
  }

  data <- data %>%
    dplyr::arrange(.data$bank_id, .data$sector, .data$name_abcd, .data$region, .data$year)

  return(data)
}


#' Return company level scenario scores for the main sector of each company
#'
#' @param data data.frame. Holds the PACTA for Banks SDA results on company level.
#' @param scenario_emission_intensities data frame containing the scenario file with
#'   information on yearly emission intensity levels.
#' @param scenario_source Character. Vector that indicates which scenario_source
#'   to use for reference in the calculation of the scores. Currently, the only
#'   supported value is `"geco_2021"`.
#' @param scenario Character. Vector that indicates which scenario to calculate
#'   the score for. Must be a scenario available from `scenario_source`.
#' @param aggregate Logical. Indicates whether the indicators should be
#'   calculated for an aggregate of all loan books by different banks in `data`
#'   or if they should be calculated individually, based on their
#'   `bank_id`. If only one loan book is included, use the default
#'   aggregate == TRUE.
#'
#' @return NULL
#' @export
calculate_company_aggregate_score_sda <- function(data,
                                                  scenario_emission_intensities,
                                                  scenario_source = "geco_2021",
                                                  scenario = "1.5c",
                                                  aggregate = TRUE) {
  # validate input values
  validate_input_args_calculate_company_aggregate_score_sda(
    scenario_source = scenario_source,
    scenario = scenario
  )

  # validate input data set
  validate_input_data_calculate_company_aggregate_score_sda(
    data = data,
    scenario_emission_intensities = scenario_emission_intensities
  )

  start_year <- min(data$year, na.rm = TRUE)
  target_scenario <- paste0("target_", scenario)

  if (!is.logical(aggregate)) {
    stop("Function argument aggregate must be either TRUE or FALSE!")
  }

  if (!"bank_id" %in% names(data) & !aggregate) {
    stop("The input data set does not contain bank identifiers, which are needed
         to process multiple loan books on an individual level")
  } else if (!"bank_id" %in% names(data) & aggregate) {
    data <- data %>%
      dplyr::mutate(bank_id = "bank")
  }

  data <- data %>%
    dplyr::filter(.data$scenario_source == .env$scenario_source)

  data <- data %>%
    group_by(
      .data$bank_id, .data$name_abcd, .data$emission_factor_metric, .data$year, .data$region,
      .data$scenario_source
    ) %>%
    dplyr::filter(.data$name_abcd != "market") %>%
    dplyr::filter(.data$emission_factor_metric %in% c("projected", paste0("target_", .env$scenario))) %>%
    dplyr::filter(dplyr::between(.data$year, left = .env$start_year, right = .env$start_year + 5)) %>%
    tidyr::pivot_wider(
      names_from = "emission_factor_metric",
      values_from = "emission_factor_value"
    ) %>%
    dplyr::ungroup()

  # calculate sector score
  data <- data %>%
    dplyr::group_by(
      .data$bank_id, .data$name_abcd, .data$scenario_source, .data$region, .data$sector, .data$year
    ) %>%
    dplyr::mutate(
      total_deviation = (.data$projected - !!rlang::sym(target_scenario)) * -1
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      direction = "net",
      score = .data$total_deviation / !!rlang::sym(target_scenario)
    )

  activity_units_sector <- activity_units %>%
    dplyr::distinct(.data$sector, .data$activity_unit)

  data <- data %>%
    dplyr::mutate(scenario = .env$scenario) %>%
    dplyr::inner_join(activity_units_sector, by = "sector") %>%
    dplyr::select(
      c(
        "bank_id", "name_abcd", "sector", "activity_unit", "region",
        "scenario_source", "scenario", "year", "direction", "total_deviation",
        "score"
      )
    ) %>%
    dplyr::arrange(.data$bank_id, .data$sector, .data$name_abcd, .data$region, .data$year)

  return(data)
}

validate_input_args_calculate_company_tech_deviation <- function(scenario_source,
                                                                 scenario) {
  if (!length(scenario_source) == 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
  if (!inherits(scenario_source, "character")) {
    stop("Argument scenario_source must be of class character. Please check your input.")
  }
  if (!length(scenario) == 1) {
    stop("Argument scenario must be of length 1. Please check your input.")
  }
  if (!inherits(scenario, "character")) {
    stop("Argument scenario must be of length 1. Please check your input.")
  }

  invisible()
}

validate_input_data_calculate_company_tech_deviation <- function(data,
                                                                 technology_direction,
                                                                 scenario_trajectory,
                                                                 green_or_brown) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "sector", "technology", "year", "region", "scenario_source", "name_abcd",
      "metric", "production", "technology_share", "scope",
      "percentage_of_initial_production_by_scope", "bank_id"
    )
  )

  validate_data_has_expected_cols(
    data = technology_direction,
    expected_columns = c(
      "scenario_source", "scenario", "sector", "technology", "region",
      "directional_dummy"
    )
  )

  validate_data_has_expected_cols(
    data = scenario_trajectory,
    expected_columns = c(
      "scenario_source", "scenario", "sector", "technology", "region", "year",
      "tmsr", "smsp"
    )
  )

  validate_data_has_expected_cols(
    data = green_or_brown,
    expected_columns = c(
      "sector", "technology", "green_or_brown"
    )
  )

  invisible()
}

validate_input_args_calculate_company_aggregate_score_tms <- function(scenario_source,
                                                                      scenario) {
  if (!length(scenario_source) == 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
  if (!inherits(scenario_source, "character")) {
    stop("Argument scenario_source must be of class character. Please check your input.")
  }
  if (!length(scenario) == 1) {
    stop("Argument scenario must be of length 1. Please check your input.")
  }
  if (!inherits(scenario, "character")) {
    stop("Argument scenario must be of length 1. Please check your input.")
  }

  invisible()
}

validate_input_data_calculate_company_aggregate_score_tms <- function(data,
                                                                      scenario) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "sector", "technology", "year", "region", "scenario_source", "name_abcd",
      "bank_id", "projected", paste0("target_", scenario), "direction",
      "total_tech_deviation", "activity_unit", "technology_share_by_direction"
    )
  )

  invisible()
}

validate_input_args_calculate_company_aggregate_score_sda <- function(scenario_source,
                                                                      scenario) {
  if (!length(scenario_source) == 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
  if (!inherits(scenario_source, "character")) {
    stop("Argument scenario_source must be of class character. Please check your input.")
  }
  if (!length(scenario) == 1) {
    stop("Argument scenario must be of length 1. Please check your input.")
  }
  if (!inherits(scenario, "character")) {
    stop("Argument scenario must be of length 1. Please check your input.")
  }

  invisible()
}


validate_input_data_calculate_company_aggregate_score_sda <- function(data,
                                                                      scenario_emission_intensities) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns <- c(
      "sector", "year", "region", "scenario_source", "name_abcd",
      "emission_factor_metric", "emission_factor_value", "bank_id"
    )
  )

  validate_data_has_expected_cols(
    data = scenario_emission_intensities,
    expected_columns <- c(
      "scenario_source", "scenario", "sector",  "region", "year",
      "emission_factor", "emission_factor_unit"
    )
  )

  invisible()
}

