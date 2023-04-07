#' Plot alignment scatterplot
#'
#' @param data data.frame. Should have the same format as output of
#'   `prep_scatter_animated()` and contain columns: 'name', 'buildout',
#'   phaseout', 'net' and 'year'.
#' @param data_level Character. Level of the plotted data. Can be 'bank' or
#'   'company'.
#' @param sector Character. Sector name to be used in the plot title.
#' @param scenario_source Character. Scenario source to be used in the plot
#'   caption.
#' @param scenario Character. Scenario name to be used in the plot caption.
#' @param region Character. Region to be used in the plot caption.
#' @param title Character. Custom title if different than default.
#' @param subtitle Character. Custom subtitle if different than default.
#' @param alignment_limit Numeric. Limit to be applied to the x- and y-axis
#'   scales and to alignment values for colouring. By default the maximum
#'   absolute alignment value of is used.
#' @param cap_outliers Numeric. Cap which should be applied to the alignment
#'   values in the data. Values bigger than cap are plotted on the border of the
#'   plot.
#' @param floor_outliers Numeric. Floor which should be applied to the alignment
#'   values in the data. Values smaller than floor are plotted on the border of
#'   the plot.
#'
#' @return object of type "plotly"
#' @export
#'
#' @examples
#' #TODO
plot_scatter_animated <- function(
    data,
    data_level = c("company", "bank"),
    sector = NULL,
    scenario_source = NULL,
    scenario = NULL,
    region = NULL,
    title = NULL,
    subtitle = NULL,
    alignment_limit = NULL,
    cap_outliers = NULL,
    floor_outliers = NULL
    ) {
  arg_match(data_level)

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
      title <- glue("Build-out vs. Phase-out Alignment \nin the {r2dii.plot::to_title(sector)} Sector")
    } else {
      title <- "Build-out vs. Phase-out Alignment"
    }
  }

  if (data_level == "company") {
    title <- paste0(title, " per Company")
    if (is.null(subtitle)) {
      subtitle <- "Each dot is a company. The companies in the top right quadrant are both building out\n low-carbon technologies and phasing out high-carbon technologies at rates\ngreater or equal to those required by the scenario."
    }
  } else {
    title <- paste0(title, " per Bank")
    if (is.null(subtitle)) {
      subtitle <- "Each dot is a bank. The banks in the top right quadrant are exposed to companies\nwhich on aggregate level are both building out low-carbon technologies and phasing out\nhigh-carbon technologies at rates greater or equal to those required by the scenario."
    }
  }

  check_plot_scatter_animated(data, alignment_limit, cap_outliers, floor_outliers)

  if(!is.null(floor_outliers)) {
    data <- data %>%
      mutate(
        buildout = if_else(.data$buildout <= .env$floor_outliers, .env$floor_outliers, .data$buildout),
        phaseout = if_else(.data$phaseout <= .env$floor_outliers, .env$floor_outliers, .data$phaseout),
        net = if_else(.data$net <= .env$floor_outliers, .env$floor_outliers, .data$net)
      )
    subtitle <- glue("{subtitle}\nThe outliers are displayed on the borders of the plot.", .trim = FALSE)
  }
  if (!is.null(cap_outliers)) {
    data <- data %>%
      mutate(
        buildout = if_else(.data$buildout >= .env$cap_outliers, .env$cap_outliers, .data$buildout),
        phaseout = if_else(.data$phaseout >= .env$cap_outliers, .env$cap_outliers, .data$phaseout),
        net = if_else(.data$net >= .env$cap_outliers, .env$cap_outliers, .data$net)
      )
    if (is.null(floor_outliers)) {
      subtitle <- glue("{subtitle}\nThe outliers are displayed on the borders of the plot.", .trim = FALSE)
    }
  }

  title <- glue("<b>{title}</b>\n\n<sup>{subtitle}</sup>")

  if (is.null(alignment_limit)) {
    alignment_limit <- max(abs(c(data$buildout, data$phaseout, data$net)), na.rm = TRUE)
  }

  p <- plotly::plot_ly(
      x = ~buildout,
      y = ~phaseout,
      frame = ~year,
      showlegend = F,
      color = ~net,
      colors = colorRamp(c("#e10000", "#FFFFFF", "#3d8c40")),
      symbol = ~datapoint,
      symbols = c("circle", "circle-open"),
      width = 600,
      height = 800
    ) %>%
    plotly::add_markers(
      name = "company",
      data = data,
      text = ~name,
      marker = list(
        autocolorscale = F,
        cmin = -alignment_limit,
        cmid = 0,
        cmax = alignment_limit
      )
    ) %>%
    plotly::add_annotations(
      text = "Aligned buildout,\nAligned phaseout",
      x = 0.99,
      xref = "paper",
      y = 0.99,
      yref = "paper",
      showarrow = F,
      align = "right",
      font = list(color = "#c0c0c0")
    ) %>%
    plotly::add_annotations(
      text = "Misaligned buildout,\nAligned phaseout",
      x = 0.49,
      xref = "paper",
      xanchor = "right",
      y = 0.99,
      yref = "paper",
      showarrow = F,
      align = "right",
      font = list(color = "#c0c0c0")
    ) %>%
    plotly::add_annotations(
      text = "Misaligned buildout,\nMisaligned phaseout",
      x = 0.01,
      xref = "paper",
      y = 0.01,
      yref = "paper",
      showarrow = F,
      align = "left",
      font = list(color = "#c0c0c0")
    ) %>%
    plotly::add_annotations(
      text = "Aligned buildout,\nMisaligned phaseout",
      x = 0.51,
      xref = "paper",
      xanchor = "left",
      y = 0.01,
      yref = "paper",
      showarrow = F,
      align = "left",
      font = list(color = "#c0c0c0")
    ) %>%
    plotly::add_annotations(
      text = caption,
      x = 1,
      xanchor = "right",
      align = "right",
      xref = "paper",
      y = -0.6,
      yref = "paper",
      showarrow = F
    ) %>%
    plotly::add_annotations(
      text = "0% net deviation from scenario",
      x = 0.99,
      xanchor = "right",
      align = "right",
      xref = "paper",
      y = 0.43,
      yanchor = "top",
      yref = "paper",
      showarrow = F,
      font = list(color = "#ffffff"),
      textangle = 45
    ) %>%
    plotly::colorbar(
      limits = c(-alignment_limit, alignment_limit),
      title = "Net\ndeviation",
      tickformat = ",.0%"
      ) %>%
    plotly::layout(
      title = list(
        text = title,
        font = list(color = "#000"),
        pad = list(b = 20),
        yanchor = "top",
        yref = "container",
        y = 0.95
        ),
      xaxis = list(
        title = list(
          text = "Deviation from scenario value\nfor low-carbon technologies build-out",
          font = list(color = "#000")
          ),
        range = c(-alignment_limit, alignment_limit),
        color = "#c0c0c0",
        tickfont = list(color = "#000"),
        showgrid = FALSE,
        tickformat = ",.0%"
        ),
      yaxis = list(
        title = list(
          text = "Deviation from scenario value\nfor high-carbon technologies phase-out",
          font = list(color = "#000")
          ),
        range = c(-alignment_limit, alignment_limit),
        color = "#c0c0c0",
        tickfont = list(color = "#000"),
        showgrid = FALSE,
        tickformat = ",.0%"
        ),
      plot_bgcolor = "#6c6c6c",
      autosize = F,
      margin = list(l = 0, r = 0, t = 155, b = 250),
      shapes = list(
        list(
          type = "line",
          x0 = -alignment_limit,
          y0 = alignment_limit,
          x1 = alignment_limit,
          y1 = -alignment_limit,
          layer = "below",
          line = list(color = "#ffffff", width = 1)
        )
      )
    )

  p$x$layout$sliders[[1]]$pad$t <- 80
  p$x$layout$updatemenus[[1]]$pad$t <- 80

  p
}

check_plot_scatter_animated <- function(
    data,
    alignment_limit,
    cap_outliers,
    floor_outliers
    ) {
  r2dii.plot:::abort_if_missing_names(data, c("name", "buildout",
   "phaseout", "net", "year"))
  if (!is.null(alignment_limit)) {
    if ((length(alignment_limit) != 1) | (!is.numeric(alignment_limit))){
      rlang::abort("'alignment_limit' must be a numeric value.")
    }
  }
  if (!is.null(cap_outliers)) {
    if ((length(cap_outliers) != 1) | (!is.numeric(cap_outliers))){
     rlang::abort("'cap_outliers' must be a numeric value.")
    }
  }
  if (!is.null(floor_outliers)) {
    if ((length(floor_outliers) != 1) | (!is.numeric(floor_outliers))){
      rlang::abort("'floor_outliers' must be a numeric value.")
    }
  }
}
