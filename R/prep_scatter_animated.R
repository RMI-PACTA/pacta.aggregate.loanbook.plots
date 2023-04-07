#' Prepare data to plot animated scatterplot
#'
#' @param data_bopo data.frame. Data containing buildout and phaseout alignment
#'   values. Must contain columns: 'bank_id', 'year', 'sector', 'region',
#'   'direction' and either 'name_abcd' and 'score' or
#'   'exposure_weighted_net_alignment'.
#' @param data_net data.frame. Data containing net alignment values. Must
#'   contain columns: 'bank_id', 'year', 'sector', 'region', 'direction' and
#'   either 'name_abcd' and 'score' or 'exposure_weighted_net_alignment'.
#' @param data_level Character. Level of the plotted data. Can be 'bank' or
#'   'company'.
#' @param sector Character. Sector to filter data on.
#' @param region Character. Region to filter data on.
#' @param bank_ids_to_plot Character vector. Bank ids to filter on.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' #TODO
prep_scatter_animated <- function(
    data_bopo,
    data_net,
    data_level = c("bank", "company"),
    sector,
    region,
    bank_ids_to_plot = NULL
    ) {
  arg_match(data_level)

  if (data_level == "bank") {
    name_col = "bank_id"
    value_col = "exposure_weighted_net_alignment"
  } else {
    name_col = "name_abcd"
    value_col = "score"
  }

  check_prep_scatter_animated(data_bopo, sector, region, bank_ids_to_plot, name_col, value_col)
  check_prep_scatter_animated(data_net, sector, region, bank_ids_to_plot, name_col, value_col)

  if (is.null(bank_ids_to_plot)) {
    bank_ids_to_plot <- unique(c(data_bopo$bank_id, data_net$bank_id))
  }

  data_scatter <- data_bopo %>%
    bind_rows(data_net) %>%
    filter(
      .data$sector == .env$sector,
      .data$region == .env$region,
      .data$bank_id %in% bank_ids_to_plot
      ) %>%
    select("name" = name_col, "direction", "year", "value" = value_col) %>%
    distinct() %>%
    tidyr::pivot_wider(names_from = "direction", values_from = "value") %>%
    mutate(
      datapoint = case_when(
        grepl(".*[Bb]enchmark,*", .data$name) ~ "Benchmark",
        TRUE & data_level == "bank" ~ "Bank",
        TRUE & data_level == "company" ~ "Company",
        TRUE ~ "Portfolio"
      )
    ) %>%
    mutate(
      datapoint = factor(datapoint, levels = c("Bank", "Company", "Portfolio", "Benchmark"))
    ) %>%
  arrange(.data$datapoint)
  data_scatter
}

check_prep_scatter_animated <- function(
    data,
    sector,
    region,
    bank_ids_to_plot,
    name_col,
    value_col
    ) {
  r2dii.plot:::abort_if_missing_names(data, c("bank_id", "year",
   "sector", "region", "direction", name_col, value_col))
  r2dii.plot:::abort_if_unknown_values(sector, data, "sector")
  r2dii.plot:::abort_if_unknown_values(region, data, "region")
  r2dii.plot:::abort_if_unknown_values(bank_ids_to_plot, data, "bank_id")
}
