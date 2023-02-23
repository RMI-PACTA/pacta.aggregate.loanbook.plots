#' Prepare data to plot using `plot_sankey()`
#'
#' @param tms_aggregated data.frame. Holds aggregated alignment scores per
#'   company for tms sectors. Must contain columns: `bank_id`, `name_abcd`,
#'   `sector`.
#' @param sda_aggregated data.frame. Holds aggregated alignment scores per
#'   company for sda sectors. Must contain columns: `bank_id`, `name_abcd`,
#'   `sector`.
#' @param matched_loanbook data.frame. Holds the matched loan books of a set of
#'   banks. Must include a column `bank_id` and `loan_size_outstanding`.
#' @param region_tms Character. Region to filter `tms_aggregated` data frame on.
#' @param region_sda Character. Region to filter `sda_aggregated` data frame on.
#' @param middle_node Character. Column specifying the middle nodes to be
#'   plotted in sankey plot. Must be present in `tms_aggegated` and
#'   `sda_aggregated` input data frames.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' # TODO
prep_sankey <- function(
    tms_aggregated,
    sda_aggregated,
    matched_loanbook,
    region_tms,
    region_sda,
    middle_node = c("sector", "name_abcd")) {

  tms_aggregated <- tms_aggregated %>%
    filter(.data$region == region_tms)

  sda_aggregated <- sda_aggregated %>%
    filter(.data$region == region_sda)

  data_out <- tms_aggregated %>%
    dplyr::bind_rows(sda_aggregated) %>%
    inner_join(matched_loanbook, by = c("bank_id", "name_abcd", "sector")) %>%
    mutate(
      is_aligned = case_when(
        score >= 0 ~ "Aligned",
        score <0 ~ "Not aligned",
        TRUE ~ "Unknown"
      ),
      middle_node =!! sym(middle_node)
      ) %>%
    group_by(middle_node) %>%
    mutate(middle_node_total = sum(.data$loan_size_outstanding)) %>%
    ungroup() %>%
    arrange(desc(.data$middle_node_total)) %>%
    select("bank_id", "middle_node", "is_aligned", "loan_size_outstanding")

  data_out
}
