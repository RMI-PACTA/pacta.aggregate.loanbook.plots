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

  # TODO: generalize!
  if (bridge_tech == "gascap") {
    # for now power sector only

    # calculate progress towards t10
    scenario_t10 <- scenario_trajectory %>%
      dplyr::filter(
        .data$scenario_source == .env$scenario_source,
        grepl(.env$scenario, .data$scenario),
        .data$sector == "power",
        .data$year == .env$start_year + 10
      )

    data_scen_t10 <- data %>%
      dplyr::filter(.data$year == .env$start_year) %>%
      dplyr::select(-"year") %>%
      dplyr::group_by(.data$sector, .data$region, .data$scenario_source, .data$name_abcd, .data$bank_id) %>%
      dplyr::mutate(projected_sector = sum(.data$projected, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::inner_join(scenario_t10, by = c("sector", "technology", "region", "scenario_source")) %>%
      dplyr::inner_join(green_or_brown, by = c("sector", "technology")) %>%
      dplyr::mutate(
        !!target_scenario := dplyr::if_else(
          .data$green_or_brown == "green",
          .data$projected + (.data$projected_sector * .data$smsp),
          .data$projected * .data$tmsr
        )
      ) %>%
      dplyr::rename(scen_t10 = !!rlang::sym(target_scenario)) %>%
      dplyr::select(-c("green_or_brown", "tmsr", "smsp", "projected"))

    data_scen_t10 <- data_scen_t10 %>%
      dplyr::inner_join(
        data,
        by = c("sector", "technology", "region", "scenario_source", "name_abcd", "bank_id", "directional_dummy")
      ) %>%
      dplyr::mutate(
        add_scen_t5_t10 = abs(.data$scen_t10 - !!rlang::sym(target_scenario)),
        deviation_prod_t5_scen_t10 = (.data$projected - .data$scen_t10) * .data$directional_dummy,
        phaseout_deviation = dplyr::if_else(.data$directional_dummy == -1, .data$deviation_prod_t5_scen_t10, 0),
        buildout_deviation = dplyr::if_else(.data$directional_dummy == 1, .data$deviation_prod_t5_scen_t10, 0)
      )

    # calculate allowance
    data_t10_allowance <- data_scen_t10 %>%
      dplyr::filter(.data$technology != .env$bridge_tech) %>%
      dplyr::group_by(
        .data$sector, .data$region, .data$scenario_source, .data$year, .data$name_abcd, .data$bank_id
      ) %>%
      dplyr::summarise(
        total_phaseout_deviation = sum(.data$phaseout_deviation, na.rm = TRUE),
        total_buildout_deviation = sum(.data$buildout_deviation, na.rm = TRUE),
        total_net_deviation = sum(.data$deviation_prod_t5_scen_t10, na.rm = TRUE),
        t5_t10_scenario_buildout = sum(.data$add_scen_t5_t10, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        allowance = dplyr::if_else(
          .data$total_buildout_deviation >= 0,
          max(.data$total_phaseout_deviation / .data$t5_t10_scenario_buildout, 0),
          0
        )
      )
  }

  # calculate total deviation per technology
  data <- data %>%
    dplyr::mutate(
      total_tech_deviation = (.data$projected - !!rlang::sym(target_scenario)) * .data$directional_dummy
    )

  if (exists("data_t10_allowance") & exists("data_scen_t10")) {
    # update total_tech_deviation
    data_t10_allowance <- data_t10_allowance %>%
      dplyr::select(c("bank_id", "name_abcd", "sector", "region", "scenario_source", "year", "allowance")) %>%
      dplyr::mutate(technology = .env$bridge_tech)

    data_scen_t10 <- data_scen_t10 %>%
      dplyr::filter(.data$technology == .env$bridge_tech) %>%
      dplyr::select(
        c(
          "sector", "technology", "region", "scenario_source", "scenario", "name_abcd",
          "bank_id", "projected", !!target_scenario, "scen_t10", "directional_dummy", "year"
        )
      ) %>%
      dplyr::inner_join(
        data_t10_allowance,
        by = c("bank_id", "name_abcd", "sector", "technology", "region", "scenario_source", "year")
      ) %>%
      dplyr::mutate(
        scen_t5_allowance = dplyr::if_else(
          .data$allowance <= 0,
          !!rlang::sym(target_scenario),
          !!rlang::sym(target_scenario) + (.data$scen_t10 - !!rlang::sym(target_scenario)) * .data$allowance
        )
      ) %>%
      dplyr::mutate(
        total_tech_deviation = dplyr::case_when(
          .data$projected < !!rlang::sym(target_scenario) ~ .data$projected - !!rlang::sym(target_scenario),
          .data$projected > .data$scen_t5_allowance ~ (.data$projected - .data$scen_t5_allowance) * -1,
          TRUE ~ 0
        )
      ) %>%
      dplyr::select(-!!rlang::sym(target_scenario)) %>%
      dplyr::rename(!!target_scenario := "scen_t5_allowance") %>%
      dplyr::select(
        c(
          "sector", "technology", "year", "region", "scenario_source", "name_abcd", "bank_id", "projected", !!target_scenario, "directional_dummy", "total_tech_deviation"
        )
      )

    data <- data %>%
      dplyr::filter(.data$technology != .env$bridge_tech) %>%
      dplyr::bind_rows(data_scen_t10)
  }

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
  start_year <- min(data$year, na.rm = TRUE)
  target_scenario <- paste0("target_", scenario)
  level <- match.arg(level)

  if (level == "bo_po") {
    # calculate buildout and phaseout sector score
    data <- data %>%
      dplyr::mutate(
        net_absolute_scenario_value = sum(!!rlang::sym(target_scenario), na.rm = TRUE),
        .by = c("bank_id", "name_abcd", "scenario_source", "region", "sector", "year")
      ) %>%
      dplyr::summarise(
        total_deviation = sum(.data$total_tech_deviation, na.rm = TRUE),
        absolute_scenario_value = sum(!!rlang::sym(target_scenario), na.rm = TRUE),
        .by = c("bank_id", "name_abcd", "scenario_source", "region", "sector", "year", "net_absolute_scenario_value", "directional_dummy")
      ) %>%
      dplyr::mutate(
        score1 = .data$total_deviation / .data$absolute_scenario_value,
        score2 = .data$total_deviation / .data$net_absolute_scenario_value,
        direction = dplyr::if_else(.data$directional_dummy == 1, "buildout", "phaseout")
      ) %>%
      dplyr::select(-"directional_dummy")

    data <- data %>%
      dplyr::mutate(scenario = .env$scenario) %>%
      dplyr::select(c("bank_id", "name_abcd", "sector", "region", "scenario_source", "scenario", "year", "direction", "score1", "score2")) %>%
      dplyr::arrange(.data$bank_id, .data$sector, .data$name_abcd, .data$region, .data$year)

  } else if (level == "net") {
    # calculate net sector score
    data <- data %>%
      dplyr::group_by(
        .data$bank_id, .data$name_abcd, .data$scenario_source, .data$region, .data$sector, .data$year
      ) %>%
      dplyr::summarise(
        total_net_deviation = sum(.data$total_tech_deviation, na.rm = TRUE),
        net_absolute_scenario_value = sum(!!rlang::sym(target_scenario), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        score = .data$total_net_deviation / .data$net_absolute_scenario_value
      )

    data <- data %>%
      dplyr::mutate(scenario = .env$scenario) %>%
      dplyr::select(c("bank_id", "name_abcd", "sector", "region", "scenario_source", "scenario", "year", "score")) %>%
      dplyr::arrange(.data$bank_id, .data$sector, .data$name_abcd, .data$region, .data$year)
  }

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

  # TODO: keep corporate_economy, taking nto account that it needs to done
  # either separately or treated like a scenario that gets a value for each company

  data <- data %>%
    group_by(
      .data$bank_id, .data$name_abcd, .data$emission_factor_metric, .data$year, .data$region,
      .data$scenario_source # , .data$technology
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
      emission_intensity_deviation = (.data$projected - !!rlang::sym(target_scenario)) * -1
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      score = .data$emission_intensity_deviation / !!rlang::sym(target_scenario)
    )

  data <- data %>%
    dplyr::mutate(scenario = .env$scenario) %>%
    dplyr::select(c("bank_id", "name_abcd", "sector", "region", "scenario_source", "scenario", "score", "year")) %>%
    dplyr::arrange(.data$bank_id, .data$sector, .data$name_abcd, .data$region, .data$year)

  return(data)
}
