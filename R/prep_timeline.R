#' Prepare data to plot timeline
#'
#' @param data data.frame. Must contain columns: 'direction', 'year',
#'   'exposure_weighted_net_alignment', 'bank_id', 'sector'.
#' @param sector Character. Sector to filter data on.
#' @param region Character. Region to filter data on.
#' @param bank_ids_to_plot Character vector. Bank ids to filter on.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' #TODO
prep_timeline <- function(data, sector, region, bank_ids_to_plot) {
  check_prep_timeline(data, sector, region, bank_ids_to_plot)

  data_timeline <- data %>%
    filter(
      .data$sector == .env$sector,
      .data$region == .env$region,
      .data$bank_id %in% bank_ids_to_plot
    )
  data_timeline
}

check_prep_timeline <- function(data, sector, region, bank_ids_to_plot) {
  r2dii.plot:::abort_if_missing_names(data, c("direction", "year",
   "exposure_weighted_net_alignment", "bank_id", "sector"))
  r2dii.plot:::abort_if_unknown_values(sector, data, "sector")
  r2dii.plot:::abort_if_unknown_values(region, data, "region")
  r2dii.plot:::abort_if_unknown_values(bank_ids_to_plot, data, "bank_id")
}

