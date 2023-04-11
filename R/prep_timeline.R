#' Prepare data to plot timeline
#'
#' @param data data.frame. Must contain columns: 'direction', 'year',
#'   'exposure_weighted_net_alignment', 'group_id', 'sector'.
#' @param sector Character. Sector to filter data on.
#' @param region Character. Region to filter data on.
#' @param group_ids_to_plot Character vector. Group ids to filter on.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' #TODO
prep_timeline <- function(data, sector, region, group_ids_to_plot) {
  check_prep_timeline(data, sector, region, group_ids_to_plot)

  data_timeline <- data %>%
    filter(
      .data$sector == .env$sector,
      .data$region == .env$region,
      .data$group_id %in% group_ids_to_plot
    )
  data_timeline
}

check_prep_timeline <- function(data, sector, region, group_ids_to_plot) {
  r2dii.plot:::abort_if_missing_names(data, c("direction", "year",
   "exposure_weighted_net_alignment", "group_id", "sector"))
  r2dii.plot:::abort_if_unknown_values(sector, data, "sector")
  r2dii.plot:::abort_if_unknown_values(region, data, "region")
  r2dii.plot:::abort_if_unknown_values(group_ids_to_plot, data, "group_id")
}

