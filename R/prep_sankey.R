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
#' @param middle_node2 Character. Column specifying the middle nodes to be
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
    year,
    middle_node,
    middle_node2 = NULL) {

  check_prep_sankey(
    tms_aggregated,
    sda_aggregated,
    matched_loanbook,
    region_tms,
    region_sda,
    year,
    middle_node,
    middle_node2
    )

  tms_aggregated <- tms_aggregated %>%
    filter(
      .data$region == region_tms,
      .data$year == .env$year
      )

  sda_aggregated <- sda_aggregated %>%
    filter(
      .data$region == region_sda,
      .data$year == .env$year
      )

  matched_loanbook <- matched_loanbook %>%
    select("bank_id", "name_abcd", "sector", "loan_size_outstanding")

  if (is.null(middle_node2)) {
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
    select("bank_id", "middle_node", "is_aligned", "loan_size_outstanding")
  } else {
    data_out <- tms_aggregated %>%
    dplyr::bind_rows(sda_aggregated) %>%
    inner_join(matched_loanbook, by = c("bank_id", "name_abcd", "sector")) %>%
    mutate(
      is_aligned = case_when(
        score >= 0 ~ "Aligned",
        score <0 ~ "Not aligned",
        TRUE ~ "Unknown"
      ),
      middle_node =!! sym(middle_node),
      middle_node2 =!! sym(middle_node2)
      ) %>%
    select("bank_id", "middle_node", "middle_node2", "is_aligned", "loan_size_outstanding")

  }

  data_out
}

check_prep_sankey <- function(
    tms_aggregated,
    sda_aggregated,
    matched_loanbook,
    region_tms,
    region_sda,
    year,
    middle_node,
    middle_node2
) {
  names_all <- c("bank_id", "name_abcd", "sector")
  names_aggergate <- c("region", "year")
  r2dii.plot:::abort_if_missing_names(tms_aggregated, c(names_all, names_aggergate))
  r2dii.plot:::abort_if_missing_names(sda_aggregated, c(names_all, names_aggergate))
  r2dii.plot:::abort_if_missing_names(matched_loanbook, c(names_all, "loan_size_outstanding"))
  if (!(region_tms %in% unique(tms_aggregated$region))) {
    abort(c(
      "`region_tms` value not found in `tms_aggeragted` dataset.",
      i = glue("Regions in `tms_aggregated` are: {toString(unique(tms_aggregated$region))}"),
      x = glue("You provided region_tms = {region_tms}.")
      ))
  }
  if (!(region_sda %in% unique(sda_aggregated$region))) {
    abort(c(
      "`region_sda` value not found in `sda_aggeragted` dataset.",
      i = glue("Regions in `sda_aggregated` are: {toString(unique(sda_aggregated$region))}"),
      x = glue("You provided region_sda = {region_sda}.")
      ))
  }
  if (!(year %in% unique(c(sda_aggregated$year, tms_aggregated$year)))) {
    abort(c(
      "`year` value not found in one of the aggeragted datasets.",
      i = glue(
        "Years in `sda_aggregated` are: {toString(unique(sda_aggregated$year))}
         Years in `tms_aggregated` are: {toString(unique(tms_aggregated$year))}
        "
        ),
      x = glue("You provided year = {year}.")
      ))
  }
  r2dii.plot:::abort_if_missing_names(tms_aggregated, middle_node)
  r2dii.plot:::abort_if_missing_names(sda_aggregated, middle_node)
  if (!is.null(middle_node2)) {
    r2dii.plot:::abort_if_missing_names(tms_aggregated, middle_node2)
    r2dii.plot:::abort_if_missing_names(sda_aggregated, middle_node2)
  }

}
