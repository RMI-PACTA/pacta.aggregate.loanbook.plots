#' Plot alignment timeline
#'
#' @param data data.frame Should have the same format as output of
#'   `prep_timeline()` and contain columns: 'direction', 'year',
#'   'exposure_weighted_net_alignment', 'bank_id'.
#' @param sector Character. Sector name to be used in the plot title.
#' @param scenario_source Character. Scenario source to be used in the plot
#'   caption.
#' @param scenario Character. Scenario name to be used in the plot caption.
#' @param region Character. Region to be used in the plot caption.
#' @param title Character. Custom title if different than default.
#' @param subtitle Character. Custom subtitle if different than default.
#' @param alignment_limits Numeric vector of size 2. Limits to be applied to
#'   alignment values for colouring. By default maximum absolute value of
#'   'exposure_weighted_net_alignment' is used.
#'
#' @return object of type "ggplot"
#' @export
#'
#' @examples
#' #TODO
plot_timeline <- function(
    data,
    sector = NULL,
    scenario_source = NULL,
    scenario = NULL,
    region = NULL,
    title = NULL,
    subtitle = NULL,
    alignment_limits = NULL
    ) {

  caption <- ""
  if (!is.null(scenario_source) | !is.null(scenario) | !is.null(region)) {
    if (!is.null(scenario)) {
      caption <- glue("Scenario: {beautify_scenario_label(scenario)}\n", .trim = FALSE)
    }
    if (!is.null(scenario_source)) {
      caption <- glue("{caption}Scenario source: {beautify_scenario_label(scenario_source)}\n", .trim = FALSE)
    }
    if(!is.null(region)) {
      caption <- glue("{caption}Region: {r2dii.plot::to_title(region)}", .trim = FALSE)
    }
  } else {
    rlang::warn("No information to display in caption provided. Please provide scenario_source and/or scenario and/or region if you want them to be included in the graph", frequency = "once")
  }

  if (is.null(title)) {
    if (!is.null(sector)) {
      title <- glue("Aggregated Alignment over Time in the {r2dii.plot::to_title(sector)} Sector")
    } else {
      title <- "Aggregated Alignment over Time"
    }
  }

  if (is.null(subtitle)) {
    if (all(unique(data$direction) == "net")) {
      subtitle <- "Each dot is a yearly alignment value which is calculated as an exposure-weighted\npercentage deviation from a scenario-based value. Colour intensity indicates\nhow aligned (green) or misaligned (red) the value is."
    } else {
      subtitle <- "Each dot is a yearly alignment value which is calculated as an exposure-weighted percentage deviation from\n a scenario-based value. Build-out alignment is calcuated based on low-carbon technologies required to be\nbuilt out by the scenario. Phase-out alignment is calculated based on high-carbon technologies required to\nbe phased-out by the scenario. Colour intensity indicates how aligned (green) or misaligned (red) the value is."
    }
  }

  if (is.null(alignment_limits)) {
    max_value <- max(abs(data$exposure_weighted_net_alignment), na.rm = TRUE)
    alignment_limits <- c(-max_value, max_value)
  }

  check_timeline(data, alignment_limits)

  p <- ggplot(
      data,
      aes(
        x = .data$year,
        y = .data$exposure_weighted_net_alignment,
        colour = .data$exposure_weighted_net_alignment
        )
    ) +
    geom_hline(yintercept = 0, colour = "white") +
    geom_line() +
    geom_point() +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(
      name = "Exposure-weighted deviation from scenario value",
      labels = scales::percent
      ) +
    scale_colour_gradient2(
      low = "#e10000",
      mid = "white",
      high = "#3d8c40",
      midpoint = 0,
      limits = alignment_limits,
      labels = scales::percent
    ) +
    facet_grid(bank_id~direction, labeller = as_labeller(format_facet_labels)) +
    r2dii.plot::theme_2dii() +
    theme(
      panel.background = element_rect(fill = "#6c6c6c")
      ) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
      )
  p
}

check_timeline <- function(data, alignment_limits) {
  r2dii.plot:::abort_if_missing_names(data, c("direction", "year",
   "exposure_weighted_net_alignment", "bank_id"))
  if ((length(alignment_limits) != 2) | (!is.numeric(alignment_limits))){
    rlang::abort("'alignment_limits' must be a numeric vector of size 2.")
  }
}

format_facet_labels <- function(str) {
  str_out <- case_when(
    grepl("(?i)buildout", str) ~ "Build-out",
    grepl("(?i)phaseout", str) ~ "Phase-out",
    TRUE ~ r2dii.plot::to_title(str)
  )
  str_out
}
