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
    data_level = c("company", "bank")
    ) {

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
      title <- glue("Build-out vs. Phase-out Alignment \nin the {r2dii.plot::to_title(sector)} Sector per Company")
    } else {
      title <- "Build-out vs. Phase-out Alignment per Company"
    }
  }

  if (is.null(subtitle)) {
    subtitle <- "Each dot is a company. In an ideal situation all dots should be in the top right\nquadrant, indicating that the company is both building out the low-carbon technologies\nand phasing out the high-carbon technologies at a rate required by the scenario."
  }

  if (is.null(alignment_limit)) {
    alignment_limit <- max(abs(c(data$buildout, data$phaseout, data$net)), na.rm = TRUE)
  }

  check_plot_scatter(data)

  data_net_0 <- data.frame(
    buildout = c(-alignment_limit, 0, alignment_limit),
    phaseout = c(alignment_limit, 0, -alignment_limit),
    net = c(0, 0, 0)
  )

  p <- ggplot(data, aes(x = buildout, y = phaseout, colour = net)) +
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
    geom_point() +
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
    scale_colour_gradient2(
      name = "Net",
      low = "#e10000",
      mid = "white",
      high = "#3d8c40",
      midpoint = 0,
      limits = c(-alignment_limit, alignment_limit),
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
  p
}

check_plot_scatter <- function(data) {

}
