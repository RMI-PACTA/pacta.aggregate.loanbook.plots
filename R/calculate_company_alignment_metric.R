#' Return company level technology deviations for TMS sectors. To be used as
#' input into calculation of company level aggregate alignment metrics for
#' production trajectory sectors.
#'
#' @param data data.frame. Holds the PACTA for Banks TMS results. Must have been
#'   calculated according to the green/brown logic of the CA100+ calculation
#'   and must return unweighted company level TMSR results.
#' @param technology_direction data frame that indicates which technologies are
#'   to be considered phase down technologies versus build out technologies
#' @param scenario_source Character. Vector that indicates which scenario_source
#'   to use for reference in the calculation of the alignment metrics. Currently,
#'   the only supported value is `"geco_2021"`.
#' @param scenario Character. Vector that indicates which scenario to calculate
#'   the alignment metric for. Must be a scenario available from `scenario_source`.
#' @param bridge_tech Character. Vector that indicates if a technology is
#'   considered a bridge technology. I.e. if the scenario requires a temporary
#'   build out despite the need for a long term phase down. If so, the alignment
#'   metric can be treated differently than for other technologies. Currently,
#'   the only allowed values are (`"none", "gascap"`). Default is `"none"` which
#'   means that no special calculations are applied to any technology.
#'
#' @return NULL
#' @export
calculate_company_tech_deviation <- function(data,
                                             technology_direction,
                                             scenario_source = "geco_2021",
                                             scenario = "1.5c",
                                             bridge_tech = c("none", "gascap")) {
  bridge_tech <- rlang::arg_match(bridge_tech)

  # validate inputs
  validate_input_calculate_company_tech_deviation(
    data = data,
    technology_direction = technology_direction,
    scenario_source = scenario_source,
    scenario = scenario,
    bridge_tech = bridge_tech
  )

  # prep and wrangle data for calculation of company tech deviations
  target_scenario <- paste0("target_", scenario)

  data <- data %>%
    prep_company_alignment_aggregation(
      technology_direction = technology_direction,
      scenario_source = scenario_source,
      scenario = scenario
    )

  # remove rows with inadequate values
  data <- data %>%
    remove_rows_with_inadequate_values(target_scenario = target_scenario)

  # calculate total deviation per technology
  data <- data %>%
    add_total_tech_deviation(target_scenario = target_scenario)

  # add technology direction
  data <- data %>%
    add_tech_direction()

  # add activity unit
  data <- data %>%
    dplyr::inner_join(
      activity_units,
      by = c("sector", "technology")
    )

  # if gas_cap is a bridge tech, both sides of the scenario are treated as misaligned
  if (identical(bridge_tech, "gascap")) {
    data <- data %>%
      apply_bridge_technology_cap(bridge_tech = bridge_tech)
  }

  return(data)
}

prep_company_alignment_aggregation <- function(data,
                                               technology_direction,
                                               scenario_source,
                                               scenario) {
  start_year <- min(data$year, na.rm = TRUE)
  # standard forward looking PACTA time frame
  time_frame <- 5

  technology_direction <- technology_direction %>%
    dplyr::filter(
      .data$scenario_source == .env$scenario_source,
      grepl(pattern = .env$scenario, x = .data$scenario)
    ) %>%
    dplyr::select(c("sector", "technology", "region", "directional_dummy"))

  data <- data %>%
    dplyr::filter(.data$scenario_source == .env$scenario_source)

  data <- data %>%
    dplyr::select(-c("technology_share", "scope", "percentage_of_initial_production_by_scope")) %>%
    dplyr::filter(.data$metric %in% c("projected", paste0("target_", .env$scenario))) %>%
    dplyr::filter(dplyr::between(.data$year, left = .env$start_year, right = .env$start_year + .env$time_frame)) %>%
    tidyr::pivot_wider(
      names_from = "metric",
      values_from = "production"
    )

  # add directional dummy
  data <- data %>%
    dplyr::inner_join(technology_direction, by = c("sector", "technology", "region"))

  return(data)
}

remove_rows_with_inadequate_values <- function(data,
                                               target_scenario) {
  data <- data %>%
    # remove rows if both projected and target values are 0 for a technology
    remove_tech_no_plans_no_target(target_scenario = target_scenario) %>%
    # remove rows if target values are 0 for a sector
    remove_sector_no_target(target_scenario = target_scenario)

  return(data)
}

remove_tech_no_plans_no_target <- function(data,
                                           target_scenario) {
  data_to_remove <- data %>%
    dplyr::group_by(
      .data$group_id, .data$name_abcd, .data$region, .data$scenario_source,
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

  data <- data %>%
    dplyr::anti_join(
      data_to_remove,
      by = c("group_id", "name_abcd", "region", "scenario_source", "sector", "technology", "year")
    )

  return(data)
}

remove_sector_no_target <- function(data,
                                    target_scenario) {
  data_to_remove_no_target_in_sector <- data %>%
    dplyr::group_by(
      .data$group_id, .data$name_abcd, .data$region, .data$scenario_source,
      .data$sector, .data$year
    ) %>%
    dplyr::rename(target = !!rlang::sym(target_scenario)) %>%
    dplyr::summarise(
      target = sum(.data$target, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$target == 0)

  data <- data %>%
    dplyr::anti_join(
      data_to_remove_no_target_in_sector,
      by = c("group_id", "name_abcd", "region", "scenario_source", "sector", "year")
    )

  return(data)
}

add_total_tech_deviation <- function(data,
                                     target_scenario) {
  data <- data %>%
    dplyr::mutate(
      total_tech_deviation = (.data$projected - !!rlang::sym(target_scenario)) * .data$directional_dummy
    )

  return(data)
}

add_tech_direction <- function(data) {
  data <- data %>%
    dplyr::mutate(direction = dplyr::if_else(.data$directional_dummy == 1, "buildout", "phaseout")) %>%
    dplyr::select(-"directional_dummy")

  return(data)
}

apply_bridge_technology_cap <- function(data,
                                        bridge_tech) {
  data_cap <- data %>%
    dplyr::filter(.data$technology == .env$bridge_tech)

  # any deviation from the scenario-based value is considered misaligned under
  # the 2-sided cap
  data_cap <- data_cap %>%
    dplyr::mutate(total_tech_deviation = abs(.data$total_tech_deviation) * -1)

  data <- data %>%
    dplyr::filter(.data$technology != .env$bridge_tech) %>%
    dplyr::bind_rows(data_cap)

  return(data)
}

#' Return company level sector alignment metric for each company with
#' option to disaggregate by buildout / phaseout.
#'
#' @param data data.frame. Holds company-technology deviations based on PACTA
#'   for Banks TMS results. Must have been calculated according to the
#'   green/brown logic of the CA100+ calculation.
#' @param scenario_source Character. Vector that indicates which scenario_source
#'   to use for reference in the calculation of the alignment metrics. Currently,
#'   the only supported value is `"geco_2021"`.
#' @param scenario Character. Vector that indicates which scenario to calculate
#'   the alignment metric for. Must be a scenario available from `scenario_source`.
#' @param level Character. Vector that indicates if the aggreagte alignment
#'   metric should be returned based on the net technology deviations (`net`) or
#'   disaggregated into buildout and phaseout technologies (`bo_po`).
#'
#' @return NULL
#' @export
calculate_company_aggregate_alignment_tms <- function(data,
                                                      scenario_source = "geco_2021",
                                                      scenario = "1.5c",
                                                      level = c("net", "bo_po")) {
  # validate inputs
  validate_input_calculate_company_aggregate_alignment_tms(
    data = data,
    scenario_source = scenario_source,
    scenario = scenario
  )

  start_year <- min(data$year, na.rm = TRUE)
  target_scenario <- paste0("target_", scenario)
  level <- match.arg(level)

  # add tech share by direction
  data <- data %>%
    add_technology_share_by_direction(level = level)

  # calculate alignment metric
  data <- data %>%
    add_net_absolute_scenario_value(target_scenario = target_scenario) %>%
    add_total_deviation() %>%
    calculate_company_alignment_metric(scenario = scenario)  %>%
    dplyr::arrange(
      .data$group_id,
      .data$sector,
      .data$name_abcd,
      .data$region,
      .data$year
    )

  return(data)
}

add_technology_share_by_direction <- function(data,
                                              level = c("net", "bo_po")) {
  if (level == "bo_po") {
    # when applying the buildout/phaseout calculation, the technology share is
    # split within a sector, based on the production share in buildout and
    # phaseout technologies
    data <- data %>%
      dplyr::mutate(
        prod_sector = sum(.data$projected, na.rm = TRUE),
        .by = c("sector", "year", "region", "scenario_source", "name_abcd", "group_id", "activity_unit")
      ) %>%
      dplyr::mutate(
        technology_share_by_direction = sum(.data$projected, na.rm = TRUE) / .data$prod_sector,
        .by = c("sector", "year", "region", "scenario_source", "name_abcd", "group_id", "direction", "activity_unit")
      ) %>%
      dplyr::select(-"prod_sector")
  } else if (level == "net") {
    # when applying the net calculation, there is only one direction, hence the
    # share includes all technologies and is always 1.
    data <- data %>%
      dplyr::mutate(
        direction = .env$level,
        technology_share_by_direction = 1
      )
  } else {
    stop("Invalid input provided for argument: level.")
  }

  return(data)
}

add_net_absolute_scenario_value <- function(data,
                                            target_scenario) {
  data <- data %>%
    dplyr::mutate(
      net_absolute_scenario_value = sum(!!rlang::sym(target_scenario), na.rm = TRUE),
      .by = c(
        "group_id", "name_abcd", "scenario_source", "region", "sector",
        "activity_unit", "year"
      )
    )

  return(data)
}

add_total_deviation <- function(data) {
  data <- data %>%
    dplyr::summarise(
      total_deviation = sum(.data$total_tech_deviation, na.rm = TRUE),
      .by = c(
        "group_id", "name_abcd", "scenario_source", "region", "sector",
        "activity_unit", "year", "net_absolute_scenario_value", "direction",
        "technology_share_by_direction"
      )
    )

  return(data)
}

calculate_company_alignment_metric <- function(data,
                                               scenario) {
  data <- data %>%
    dplyr::mutate(
      alignment_metric = .data$total_deviation / .data$net_absolute_scenario_value,
      scenario = .env$scenario
    ) %>%
    dplyr::select(
      c(
        "group_id", "name_abcd", "sector", "activity_unit", "region",
        "scenario_source", "scenario", "year", "direction", "total_deviation",
        "technology_share_by_direction", "alignment_metric"
      )
    )

  return(data)
}


#' Return company level sector alignment metric for each company
#'
#' @param data data.frame. Holds the PACTA for Banks SDA results on company level.
#' @param scenario_source Character. Vector that indicates which scenario_source
#'   to use for reference in the calculation of the alignment metrics. Currently,
#'   the only supported value is `"geco_2021"`.
#' @param scenario Character. Vector that indicates which scenario to calculate
#'   the alignment metric for. Must be a scenario available from `scenario_source`.
#'
#' @return NULL
#' @export
calculate_company_aggregate_alignment_sda <- function(data,
                                                      scenario_source = "geco_2021",
                                                      scenario = "1.5c") {
  # validate inputs
  validate_input_calculate_company_aggregate_alignment_sda(
    data = data,
    scenario_source = scenario_source,
    scenario = scenario
  )

  # params
  start_year <- min(data$year, na.rm = TRUE)
  # standard forward looking PACTA time frame
  time_frame <- 5
  target_scenario <- paste0("target_", scenario)
  # since sda sectors are not split into technologies, the level is always: "net"
  level <- "net"

  # prep and wrangle
  data <- data %>%
    prep_and_wrangle_aggregate_alignment_sda(
      scenario_source = scenario_source,
      target_scenario = target_scenario,
      start_year = start_year,
      time_frame = time_frame
    )

  # add activity units to data
  activity_units_sector <- activity_units %>%
    dplyr::distinct(.data$sector, .data$activity_unit)

  data <- data %>%
    dplyr::inner_join(activity_units_sector, by = "sector")

  # add tech share by direction
  data <- data %>%
    add_technology_share_by_direction(level = level)

  # calculate alignment metric
  data <- data %>%
    add_net_absolute_scenario_value(target_scenario = target_scenario) %>%
    add_total_deviation_sda() %>%
    calculate_company_alignment_metric(scenario = scenario)  %>%
    dplyr::arrange(
      .data$group_id,
      .data$sector,
      .data$name_abcd,
      .data$region,
      .data$year
    )

  return(data)
}

prep_and_wrangle_aggregate_alignment_sda <- function(data,
                                                     scenario_source,
                                                     target_scenario,
                                                     start_year,
                                                     time_frame) {
  data <- data %>%
    dplyr::filter(.data$scenario_source == .env$scenario_source) %>%
    dplyr::filter(.data$name_abcd != "market") %>%
    dplyr::filter(.data$emission_factor_metric %in% c("projected", .env$target_scenario)) %>%
    dplyr::filter(
      dplyr::between(
        .data$year,
        left = .env$start_year,
        right = .env$start_year + .env$time_frame)
    ) %>%
    tidyr::pivot_wider(
      names_from = "emission_factor_metric",
      values_from = "emission_factor_value"
    )

  return(data)
}

add_total_deviation_sda <- function(data) {
  data <- data %>%
    dplyr::mutate(
      total_deviation = (.data$projected - .data$net_absolute_scenario_value) * -1,
      .by = c(
        "group_id", "name_abcd", "scenario_source", "region", "sector", "activity_unit", "year"
      )
    )

  return(data)
}

validate_input_calculate_company_tech_deviation <- function(data,
                                                            technology_direction,
                                                            scenario_source,
                                                            scenario,
                                                            bridge_tech) {
  # validate input values
  validate_input_args_calculate_company_tech_deviation(
    scenario_source = scenario_source,
    scenario = scenario,
    bridge_tech = bridge_tech
  )

  # validate input data sets
  validate_input_data_calculate_company_tech_deviation(
    data = data,
    technology_direction = technology_direction
  )

  # consistency checks
  if (!scenario_source %in% unique(data$scenario_source)) {
    stop(
      paste0(
        "input value of `scenario_source` not found in `data`. You provided ",
        scenario_source,". Available values are: ",
        toString(unique(data$scenario_source))
      )
    )
  }
  if (!any(grepl(pattern = scenario, x = unique(data$metric)))) {
    stop(
      paste0(
        "input value of `scenario` not matched to any sub string in
        `data$metric`. You provided ", scenario_source,". Available values are: ",
        data %>%
          dplyr::filter(grepl("target_", .data$metric)) %>%
          dplyr::pull(.data$metric) %>%
          unique() %>%
          gsub(pattern = "target_", replacement = "") %>%
          toString()
      )
    )
  }

  if (!scenario_source %in% unique(technology_direction$scenario_source)) {
    stop(
      paste0(
        "input value of `scenario_source` not found in `technology_direction`
        dataset. You provided ", scenario_source,". Available values are: ",
        toString(unique(technology_direction$scenario_source))
      )
    )
  }
  if (!scenario %in% unique(technology_direction$scenario)) {
    stop(
      paste0(
        "input value of `scenario` not found in `technology_direction`
        dataset. You provided ", scenario,". Available values are: ",
        toString(unique(technology_direction$scenario))
      )
    )
  }

  invisible()
}

validate_input_args_calculate_company_tech_deviation <- function(scenario_source,
                                                                 scenario,
                                                                 bridge_tech) {
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
    stop("Argument scenario must be of class character. Please check your input.")
  }
  if (!length(bridge_tech) == 1) {
    stop("Argument bridge_tech must be of length 1. Please check your input.")
  }
  if (!inherits(bridge_tech, "character")) {
    stop("Argument bridge_tech must be of class character. Please check your input.")
  }

  invisible()
}

validate_input_data_calculate_company_tech_deviation <- function(data,
                                                                 technology_direction) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "sector", "technology", "year", "region", "scenario_source", "name_abcd",
      "metric", "production", "technology_share", "scope",
      "percentage_of_initial_production_by_scope", "group_id"
    )
  )

  validate_data_has_expected_cols(
    data = technology_direction,
    expected_columns = c(
      "scenario_source", "scenario", "sector", "technology", "region",
      "directional_dummy"
    )
  )

  invisible()
}

validate_input_calculate_company_aggregate_alignment_tms <- function(data,
                                                                     scenario_source,
                                                                     scenario) {
  # validate input values
  validate_input_args_calculate_company_aggregate_alignment_tms(
    scenario_source = scenario_source,
    scenario = scenario
  )

  # validate input data set
  validate_input_data_calculate_company_aggregate_alignment_tms(
    data = data,
    scenario = scenario
  )

  # consistency checks
  check_consistency_calculate_company_aggregate_alignment_tms(
    data = data,
    scenario_source = scenario_source
  )

  invisible()
}

validate_input_args_calculate_company_aggregate_alignment_tms <- function(scenario_source,
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

validate_input_data_calculate_company_aggregate_alignment_tms <- function(data,
                                                                          scenario) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "sector", "technology", "year", "region", "scenario_source", "name_abcd",
      "group_id", "projected", paste0("target_", scenario), "direction",
      "total_tech_deviation", "activity_unit"
    )
  )

  invisible()
}

check_consistency_calculate_company_aggregate_alignment_tms <- function(data,
                                                                        scenario_source) {
  if (!scenario_source %in% unique(data$scenario_source)) {
    stop(
      paste0(
        "input value of `scenario_source` not found in `data$scenario_source`. You provided ",
        scenario_source,". Available values are: ", toString(unique(data$scenario_source))
      )
    )
  }

  invisible()
}

validate_input_calculate_company_aggregate_alignment_sda <- function(data,
                                                                     scenario_source,
                                                                     scenario) {
  # validate input values
  validate_input_args_calculate_company_aggregate_alignment_sda(
    scenario_source = scenario_source,
    scenario = scenario
  )

  # validate input data set
  validate_input_data_calculate_company_aggregate_alignment_sda(
    data = data
  )

  # consistency checks
  check_consistency_calculate_company_aggregate_alignment_sda(
    data = data,
    scenario_source = scenario_source,
    scenario = scenario
  )

  invisible()
}

validate_input_args_calculate_company_aggregate_alignment_sda <- function(scenario_source,
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


validate_input_data_calculate_company_aggregate_alignment_sda <- function(data) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns <- c(
      "sector", "year", "region", "scenario_source", "name_abcd",
      "emission_factor_metric", "emission_factor_value", "group_id"
    )
  )

  invisible()
}

check_consistency_calculate_company_aggregate_alignment_sda <- function(data,
                                                                        scenario_source,
                                                                        scenario) {
  if (!scenario_source %in% unique(data$scenario_source)) {
    stop(
      paste0(
        "input value of `scenario_source` not found in `data$scenario_source`. You provided: ",
        scenario_source,". Available values are: ", toString(unique(data$scenario_source))
      )
    )
  }
  available_scenarios <- data %>%
    dplyr::filter(grepl("target_", .data$emission_factor_metric)) %>%
    dplyr::mutate(emission_factor_metric = gsub("target_", "", .data$emission_factor_metric)) %>%
    dplyr::pull(.data$emission_factor_metric) %>%
    unique()
  if (!scenario %in% available_scenarios) {
    stop(
      paste0(
        "input value of `scenario` not found in `data$emission_factor_metric`. You provided ",
        scenario,". Available values are: ", toString(available_scenarios)
      )
    )
  }

  invisible()
}
