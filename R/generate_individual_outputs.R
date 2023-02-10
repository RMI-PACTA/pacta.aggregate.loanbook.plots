#' Return data and plots
#'
#' @param data data.frame. Holds the PACTA for Banks results of a set of banks.
#'   Must include a column `bank_id` to select individual banks. Can be
#'   either a target market share calculation or an SDA calculation.
#' @param matched_loanbook data.frame. Holds the matched loan books of a
#'   set of banks. Must include a column `bank_id` to select individual
#'   banks.
#' @param output_directory Character. Path to directory where the ouput csv and png
#'   files should be stored.
#' @param target_type Character. Must be either `tms` or
#'   `sda`.
#' @param bank_id Character. Must be of length 1.
#' @param scenario_source Character. Must be available in the scenario file used
#'   for the analysis.
#' @param target_scenario Character. Must be of length 1.
#' @param region Character. Must be of length 1.
#' @param sector Character. Must be of length 1.
#'
#' @return NULL
#' @export
generate_individual_outputs <- function(data,
                                        matched_loanbook,
                                        output_directory,
                                        target_type = c("tms", "sda"),
                                        bank_id,
                                        scenario_source,
                                        target_scenario,
                                        region = "global",
                                        sector) {

  # match input values
  target_type <- match.arg(target_type)

  # validate input values
  validate_input_args_generate_individual_outputs(
    output_directory = output_directory,
    bank_id = bank_id,
    scenario_source = scenario_source,
    target_scenario = target_scenario,
    region = region,
    sector = sector
  )

  #validate input data
  validate_input_data_generate_individual_outputs(
    data = data,
    matched_loanbook = matched_loanbook,
    target_type = target_type
  )

  # create sub directory for the selected institute
  dir.create(file.path(output_directory, bank_id), showWarnings = FALSE)

  # set and derive some parameters
  start_year <- min(data$year, na.rm = TRUE)
  time_horizon <- 5

  data <- data %>%
    dplyr::filter(
      bank_id == .env$bank_id,
      scenario_source == .env$scenario_source,
      region == .env$region,
      sector %in% .env$sector
    )

  matched_loanbook <- matched_loanbook %>%
    dplyr::filter(
      bank_id == .env$bank_id,
      sector %in% .env$sector
    )

  if (target_type == "tms") {
    # plot tech mix for given sector
    data_techmix <- data %>%
      dplyr::filter(
        .data$metric %in% c("projected", "corporate_economy", .env$target_scenario),
        dplyr::between(.data$year, .env$start_year, .env$start_year + .env$time_horizon)
      )

    plot_techmix <- data_techmix %>%
      r2dii.plot::plot_techmix(
        span_5yr = TRUE,
        convert_label = r2dii.plot::recode_metric_techmix,
        convert_tech_label = r2dii.plot::spell_out_technology
      ) +
      ggplot2::labs(
        title = glue::glue("Technology Mix: {tools::toTitleCase(sector)}"),
        subtitle = glue::glue("Institution ID: {bank_id}")
      )

    # export tech mix
    data_techmix %>%
      readr::write_csv(
        file = file.path(
          output_directory,
          bank_id,
          glue::glue("data_tech_mix_{sector}.csv")
        )
      )

    ggplot2::ggsave(
      filename = glue::glue("plot_tech_mix_{sector}.png"),
      plot = plot_techmix,
      device = "png",
      path = file.path(output_directory, bank_id)
    )

    # plot trajectory charts for all available techs in given sector
    technologies_in_sector <- r2dii.data::green_or_brown %>%
      dplyr::filter(.data$sector == .env$sector) %>%
      dplyr::pull(.data$technology)

    technologies_to_plot <- data %>%
      dplyr::filter(
        .data$metric == .env$target_scenario,
        .data$technology %in% .env$technologies_in_sector
      ) %>%
      dplyr::distinct(.data$technology) %>%
      dplyr::arrange(.data$technology) %>%
      dplyr::pull()

    for (i in 1:length(technologies_to_plot)) {
      tryCatch(
        {
          data_trajectory <- data %>%
            dplyr::filter(
              .data$technology == .env$technologies_to_plot[i],
              dplyr::between(.data$year, .env$start_year, .env$start_year + .env$time_horizon)
            )

          plot_trajectory <- data_trajectory %>%
            r2dii.plot::plot_trajectory(
              span_5yr = TRUE,
              convert_label = r2dii.plot::recode_metric_trajectory,
              center_y = TRUE,
              value_col = "percentage_of_initial_production_by_scope",
              perc_y_scale = TRUE
            ) +
            ggplot2::labs(
              title = glue::glue("Valume Trajectory: {tools::toTitleCase(technologies_to_plot[i])}"),
              subtitle = glue::glue("Institution ID: {bank_id}")
            ) +
            ggplot2::xlab("Year") +
            ggplot2::ylab("Value")

          # export trajectory chart
          data_trajectory %>%
            readr::write_csv(
              file = file.path(
                output_directory,
                bank_id,
                glue::glue("data_trajectory_{sector}_{technologies_to_plot[i]}.csv")
              )
            )

          ggplot2::ggsave(
            filename = glue::glue("plot_trajectory_{sector}_{technologies_to_plot[i]}.png"),
            plot = plot_trajectory,
            device = "png",
            path = file.path(output_directory, bank_id)
          )
        },
        error = function(e) {
          log_text <- glue::glue("{Sys.time()} - bank: {bank_id} Problem in plotting trajectory chart for: {sector} {i} \n")
          write(log_text, file = file.path(output_directory, "error_messages.txt"), append = TRUE)
        }
      )
    }
  } else {
    # plot convergence chart for given sector
    plot_emission_intensity <- data %>%
      r2dii.plot::plot_emission_intensity(
        span_5yr = FALSE,
        convert_label = r2dii.plot::to_title
      ) +
      ggplot2::labs(
        title = glue::glue("Emission Intensity - Convergence Approach: {tools::toTitleCase(sector)}"),
        subtitle = glue::glue("Institution ID: {bank_id}")
      ) +
      ggplot2::xlab("Year") +
      ggplot2::ylab("Emission Factor Value")

    # export convergence chart
    data %>%
      readr::write_csv(
        file = file.path(
          output_directory,
          bank_id,
          glue::glue("data_emission_intensity_{sector}.csv")
        )
      )

    ggplot2::ggsave(
      filename = glue::glue("plot_emission_intensity_{sector}.png"),
      plot = plot_emission_intensity,
      device = "png",
      path = file.path(output_directory, bank_id)
    )
  }
  companies_included <- matched_loanbook %>%
    dplyr::select(
      "bank_id", "name_abcd", "sector_abcd", "loan_size_outstanding",
      "loan_size_outstanding_currency", "loan_size_credit_limit",
      "loan_size_credit_limit_currency"
    )

  companies_included %>%
    readr::write_csv(
      file = file.path(
        output_directory,
        bank_id,
        glue::glue("companies_included_{sector}.csv")
      )
    )
}

validate_input_args_generate_individual_outputs <- function(output_directory,
                                                            bank_id,
                                                            scenario_source,
                                                            target_scenario,
                                                            region,
                                                            sector) {
  if (!length(output_directory) == 1) {
    stop("Argument output_directory must be of length 1. Please check your input.")
  }
  if (!inherits(output_directory, "character")) {
    stop("Argument output_directory must be of class character. Please check your input.")
  }
  if (!length(bank_id) == 1) {
    stop("Argument bank_id must be of length 1. Please check your input.")
  }
  if (!length(scenario_source) == 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
  if (!inherits(scenario_source, "character")) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
  if (!length(target_scenario) == 1) {
    stop("Argument target_scenario must be of length 1. Please check your input.")
  }
  if (!inherits(target_scenario, "character")) {
    stop("Argument target_scenario must be of length 1. Please check your input.")
  }
  if (!length(region) == 1) {
    stop("Argument region must be of length 1. Please check your input.")
  }
  if (!inherits(region, "character")) {
    stop("Argument region must be of length 1. Please check your input.")
  }
  if (!length(sector) == 1) {
    stop("Argument sector must be of length 1. Please check your input.")
  }
  if (!inherits(sector, "character")) {
    stop("Argument sector must be of length 1. Please check your input.")
  }

  invisible()
}

validate_input_data_generate_individual_outputs <- function(data,
                                                            matched_loanbook,
                                                            target_type) {
  if (target_type == "sda") {
    validate_data_has_expected_cols(
      data = data,
      expected_columns = c(
        "sector", "year", "region", "scenario_source", "emission_factor_metric",
        "emission_factor_value", "bank_id"
      )
    )
  } else if (target_type == "tms") {
    validate_data_has_expected_cols(
      data = data,
      expected_columns = c(
        "sector", "technology", "year", "region", "scenario_source", "metric",
        "production", "technology_share", "scope",
        "percentage_of_initial_production_by_scope", "bank_id"
      )
    )
  }

  validate_data_has_expected_cols(
    data = matched_loanbook,
    expected_columns = c(
      "bank_id", "name_abcd", "sector", "sector_abcd", "loan_size_outstanding",
      "loan_size_outstanding_currency", "loan_size_credit_limit",
      "loan_size_credit_limit_currency"
    )
  )

  invisible()
}
