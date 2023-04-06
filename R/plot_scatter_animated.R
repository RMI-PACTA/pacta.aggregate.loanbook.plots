plot_scatter_animated <- function(data, sector = NULL) {

  alignment_limit <- max(abs(c(data$buildout, data$phaseout, data$net)), na.rm = TRUE)

  subtitle <- "Each dot is a company. The companies in the top right quadrant are both building out\nlow-carbon technologies and phasing out high-carbon technologies at rates\ngreater or equal to those required by the scenario."
  title <- glue("<b>Build-out vs. Phase-out Alignment in the {r2dii.plot::to_title(sector)} Sector</b>\n\n<sup>{subtitle}</sup>")

  caption <- "Scenario: X\nScenario source: Y\nRegion: Z"

  p <- plotly::plot_ly(
      x = ~buildout,
      y = ~phaseout,
      frame = ~year,
      showlegend = F,
      color = ~net,
      colors = colorRamp(c("#e10000", "#FFFFFF", "#3d8c40"))
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
      y = -0.7,
      yref = "paper",
      showarrow = F
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
      margin = list(l = 0, r = 0, t = 125, b = 250),
      shapes = list(
        list(
          type = "line",
          x0 = -alignment_limit,
          y0 = alignment_limit,
          x1 = alignment_limit,
          y1 = -alignment_limit,
          layer = "below",
          line = list(color = "#ffffff", width = 1),
          label = list(
            text = "0% net deviation from scenario",
            textposition = "middle",
            font = list(color = "#ffffff")
            )
        )
      )
    )

  p$x$layout$sliders[[1]]$pad$t <- 80
  p$x$layout$updatemenus[[1]]$pad$t <- 80

  p
}
