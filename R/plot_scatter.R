#' Plot alignment scatterplot
#'
#' @param data data.frame. Should have the same format as output of
#'   `prep_scatter()` and contain columns: 'name', 'buildout', phaseout', 'net'.
#' @param sector Character. Sector name to be used in the plot title.
#' @param scenario_source Character. Scenario source to be used in the plot
#'   caption.
#' @param scenario Character. Scenario name to be used in the plot caption.
#' @param year Integer. Year of the analysis to be used in the plot caption.
#' @param region Character. Region to be used in the plot caption.
#' @param title Character. Custom title if different than default.
#' @param subtitle Character. Custom subtitle if different than default.
#' @param alignment_limit Numeric. Limit to be applied to the x- and y-axis
#'   scales and to alignment values for colouring. By default the maximum
#'   absolute alignment value of is used.
#' @param data_level Character. Level of the plotted data. Can be 'bank' or
#'   'company'.
#' @param cap_outliers Numeric. Cap which should be applied to the alignment
#'   values in the data. Values bigger than cap are plotted on the border of the
#'   plot.
#' @param floor_outliers Numeric. Floor which should be applied to the alignment
#'   values in the data. Values smaller than floor are plotted on the border of
#'   the plot.
#'
#' @return object of type "ggplot"
#' @export
#'
#' @examples
#' #TODO
plot_scatter <- function(
    data,
    sector = NULL,
    scenario_source = NULL,
    scenario = NULL,
    year = NULL,
    region = NULL,
    title = NULL,
    subtitle = NULL,
    alignment_limit = NULL,
    data_level = c("company", "bank"),
    cap_outliers = NULL,
    floor_outliers = NULL
    ) {
  arg_match(data_level)

  caption <- ""
  if (!is.null(scenario_source) | !is.null(scenario) | !is.null(region) | !is.null(year)) {
    if (!is.null(scenario)) {
      caption <- glue("Scenario: {beautify_scenario_label(scenario)}\n", .trim = FALSE)
    }
    if (!is.null(scenario_source)) {
      caption <- glue("{caption}Scenario source: {beautify_scenario_label(scenario_source)}\n", .trim = FALSE)
    }
    if (!is.null(year)) {
      caption <- glue("{caption}Year of the analysis: {year}\n", .trim = FALSE)
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
      subtitle <- "Each dot is a company. In an ideal situation all dots should be in the top right\nquadrant, indicating that the company is both building out the low-carbon technologies\nand phasing out the high-carbon technologies at a rate required by the scenario."
    }
  } else {
    title <- paste0(title, " per Bank")
    if (is.null(subtitle)) {
      subtitle <- "Each dot is a bank. In an ideal situation all dots should be in the top right quadrant,\nindicating that, at the portfolio level, the bank is both building out the low-carbon\ntechnologies and phasing out the high-carbon technologies at a rate required by the scenario."
    }
  }

  check_plot_scatter(data, alignment_limit, cap_outliers, floor_outliers)

  if(!is.null(floor_outliers)) {
    data <- data %>%
      mutate(
        buildout = if_else(.data$buildout <= floor_outliers, floor_outliers, .data$buildout),
        phaseout = if_else(.data$phaseout <= floor_outliers, floor_outliers, .data$phaseout),
        net = if_else(.data$net <= floor_outliers, floor_outliers, .data$net)
      )
    subtitle <- glue("{subtitle}\nThe outliers are displayed on the borders of the plot.", .trim = FALSE)
  }
  if (!is.null(cap_outliers)) {
    data <- data %>%
      mutate(
        buildout = if_else(.data$buildout >= cap_outliers, cap_outliers, .data$buildout),
        phaseout = if_else(.data$phaseout >= cap_outliers, cap_outliers, .data$phaseout),
        net = if_else(.data$net >= cap_outliers, cap_outliers, .data$net)
      )
    if (is.null(floor_outliers)) {
      subtitle <- glue("{subtitle}\nThe outliers are displayed on the borders of the plot.", .trim = FALSE)
    }
  }

  if (is.null(alignment_limit)) {
    alignment_limit <- max(abs(c(data$buildout, data$phaseout, data$net)), na.rm = TRUE)
  }

  data_net_0 <- data.frame(
    buildout = c(-alignment_limit, 0, alignment_limit),
    phaseout = c(alignment_limit, 0, -alignment_limit),
    net = c(0, 0, 0)
  )

  p <- ggplot(data, aes(x = .data$buildout, y = .data$phaseout, colour = .data$net)) +
    geom_hline(yintercept = 0, colour = "#c0c0c0") +
    geom_vline(xintercept = 0, colour = "#c0c0c0") +
    geom_line(data = data_net_0) +
    annotate(
      geom = "text",
      x = alignment_limit * 0.7,
      y = -alignment_limit * 0.6,
      label = "Net alignment = 0",
      color = "white",
      angle = -45,
      size = 3
      ) +
    annotate(
      geom = "text",
      x = alignment_limit * 0.97,
      y = alignment_limit * 0.87,
      label = "Aligned buildout,\nAligned phaseout",
      color = "#c0c0c0",
      size = 3,
      hjust = 1
      ) +
    annotate(
      geom = "text",
      x = -alignment_limit * 0.03,
      y = alignment_limit * 0.87,
      label = "Misaligned buildout,\nAligned phaseout",
      color = "#c0c0c0",
      size = 3,
      hjust = 1
      ) +
    annotate(
      geom = "text",
      x = -alignment_limit * 0.97,
      y = -alignment_limit * 0.87,
      label = "Misaligned buildout,\nMisaligned phaseout",
      color = "#c0c0c0",
      size = 3,
      hjust = 0
      ) +
    annotate(
      geom = "text",
      x = alignment_limit * 0.03,
      y = -alignment_limit * 0.87,
      label = "Aligned buildout,\nMisaligned phaseout",
      color = "#c0c0c0",
      size = 3,
      hjust = 0
      ) +
    geom_point(aes(shape = .data$datapoint)) +
    scale_x_continuous(
      name = "Buildout",
      limits = c(-alignment_limit, alignment_limit),
      expand = expansion(mult = 0)
      ) +
    scale_y_continuous(
      name = "Phaseout",
      limits = c(-alignment_limit, alignment_limit),
      expand = expansion(mult = 0)
      ) +
    coord_cartesian(clip = 'off') +
    scale_colour_gradient2(
      name = "Net",
      low = "#e10000",
      mid = "white",
      high = "#3d8c40",
      midpoint = 0,
      limits = c(-alignment_limit, alignment_limit),
    ) +
    scale_shape_manual(
      name = "",
      values = c("bank" = 16, "benchmark" = 21, "company" = 16, "other" = 16),
      labels = r2dii.plot::to_title
    ) +
    r2dii.plot::theme_2dii() +
    theme(
      panel.background = element_rect(fill = "#6c6c6c"),
      legend.title = element_text(),
      aspect.ratio = 1
      ) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    )

  if (data_level == "company") {
    p <- p +
      guides(
        shape = "none"
      )
  }

  p
}

check_plot_scatter <- function(data, alignment_limit, cap_outliers, floor_outliers) {
  r2dii.plot:::abort_if_missing_names(data, c("name", "buildout",
   "phaseout", "net"))
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
  if (!is_null(floor_outliers)) {
    if ((length(floor_outliers) != 1) | (!is.numeric(floor_outliers))){
      rlang::abort("'floor_outliers' must be a numeric value.")
    }
  }
}
