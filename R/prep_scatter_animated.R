prep_scatter_animated <- function(
    data_bopo,
    data_net,
    data_level = c("bank", "company"),
    sector,
    region,
    bank_ids_to_plot = NULL
    ) {

  if (data_level == "bank") {
    name_col = "bank_id"
    value_col = "exposure_weighted_net_alignment"
  } else {
    name_col = "name_abcd"
    value_col = "score"
  }

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
        grepl(".*[Bb]enchmark,*", .data$name) ~ "benchmark",
        TRUE & (data_level == "bank") ~ "bank",
        TRUE & (data_level == "company") ~ "company",
        TRUE ~ "other"
      )
    )
  data_scatter
}
