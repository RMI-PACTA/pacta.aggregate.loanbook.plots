prep_scatter_company_level <- function(
    data_bopo,
    data_net,
    year,
    sector,
    region,
    bank_ids_to_plot
    ) {
  check_prep_scatter_company(data_bopo, year, sector, region, bank_ids_to_plot)
  check_prep_scatter_company(data_net, year, sector, region, bank_ids_to_plot)

  data_scatter <- data_bopo %>%
    bind_rows(data_net) %>%
    filter(
      .data$year == .env$year,
      .data$sector == .env$sector,
      .data$region == .env$region,
      .data$bank_id %in% bank_ids_to_plot
      ) %>%
    select("name" = "name_abcd", "direction", "value" = "score") %>%
    distinct() %>%
    tidyr::pivot_wider(names_from = "direction", values_from = "value")
  data_scatter
}

check_prep_scatter_company <- function(data, year, sector, region, bank_ids_to_plot) {
  r2dii.plot:::abort_if_missing_names(data, c("bank_id", "year",
   "sector", "region", "name_abcd", "direction", "score"))
  r2dii.plot:::abort_if_unknown_values(sector, data, "sector")
  r2dii.plot:::abort_if_unknown_values(region, data, "region")
  r2dii.plot:::abort_if_unknown_values(year, data, "year")
  r2dii.plot:::abort_if_unknown_values(bank_ids_to_plot, data, "bank_id")
}
