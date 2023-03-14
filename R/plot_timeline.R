#' Plot alignment timeline
#'
#' @param data data.frame Should have the same format as output of
#'   `prep_timeline()` and contain columns: 'direction', 'year',
#'   'exposure_weighted_net_alignment', 'bank_id'.
#' @param sector Character. Sector name to be used in the plot title.
#' @param scenario_source Character. Scenario source to be used in the plot
#'   caption.
#' @param scenario Character. Scenario name to be used in the plot caption.
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
    title = NULL,
    subtitle = NULL,
    alignment_limits = NULL
    ) {

  if (!is.null(scenario_source) & !is.null(scenario)) {
    caption <- glue("Scenario: {beautify_scenario_label(scenario)}\nScenario source: {beautify_scenario_label(scenario_source)}")
  } else {
    caption <- ""
    rlang::warn("No information to display in caption provided. Please provide scenario_source, scenario and year if you want them to be included in the graph", frequency = "once")
  }

  if (is.null(title)) {
    if (!is.null(sector)) {
      title <- glue("Aggregate Alignment Scores over Time in the {r2dii.plot::to_title(sector)} Sector")
    } else {
      title <- "Aggregate Alignment Scores over Time"
    }
  }

  if (is.null(subtitle)) {
    if (all(unique(data$direction) == "net")) {
      subtitle <- "Each dot is a yearly weighted alignment. Colour intensity indicates how positive (green)\nor negative (red) is the alignment value."
    } else {
      subtitle <- "Each dot is a yearly weighted alignment. Buildout alignment is calcuated based on low-carbon technologies\nrequired to be built out by the scenario. Phaseout alignment is calculated based on high-carbon technologies\nwhich should be phased-out according to the scenario. Colour intensity indicates how positive (green)\nor negative (red) is the alignment value."
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
        x = year,
        y = exposure_weighted_net_alignment,
        colour = exposure_weighted_net_alignment
        )
    ) +
    geom_hline(yintercept = 0, colour = "white") +
    geom_line() +
    geom_point() +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Exposure weighted alignment") +
    scale_colour_gradient2(
      low = "#e10000",
      mid = "white",
      high = "#3d8c40",
      midpoint = 0,
      limits = alignment_limits
    ) +
    facet_grid(bank_id~direction, labeller = as_labeller(r2dii.plot::to_title)) +
    r2dii.plot::theme_2dii() +
    theme(
      panel.background = element_rect(fill = "#d9d9d9")
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
