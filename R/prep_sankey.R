#' Prepare data to plot using `plot_sankey()`
#'
#' @param data_alignment data.frame. Holds aggregated alignment scores per
#'   company for tms sectors. Must contain columns: `bank_id`, `name_abcd`,
#'   `sector`.
#' @param matched_loanbook data.frame. Holds the matched loan books of a set of
#'   banks. Must include a column `bank_id` and `loan_size_outstanding`.
#' @param region Character. Region to filter `data_alignment` data frame on.
#' @param middle_node Character. Column specifying the middle nodes to be
#'   plotted in sankey plot. Must be present in `data_alignment`.
#' @param middle_node2 Character. Column specifying the middle nodes to be
#'   plotted in sankey plot. Must be present in `data_alignment`.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' # TODO
prep_sankey <- function(
    data_alignment,
    matched_loanbook,
    region,
    year,
    middle_node,
    middle_node2 = NULL) {

  check_prep_sankey(
    data_alignment,
    matched_loanbook,
    region,
    year,
    middle_node,
    middle_node2
    )

  data_alignment <- data_alignment %>%
    filter(
      .data$region == .env$region,
      .data$year == .env$year
      )

  matched_loanbook <- matched_loanbook %>%
    select("bank_id", "name_abcd", "sector", "loan_size_outstanding")

  if (is.null(middle_node2)) {
    data_out <- data_alignment %>%
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
    data_out <- data_alignment %>%
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
    data_alignment,
    matched_loanbook,
    region,
    year,
    middle_node,
    middle_node2
) {
  names_all <- c("bank_id", "name_abcd", "sector")
  names_aggergate <- c("region", "year")
  r2dii.plot:::abort_if_missing_names(data_alignment, c(names_all, names_aggergate))
  r2dii.plot:::abort_if_missing_names(matched_loanbook, c(names_all, "loan_size_outstanding"))
  if (!(region %in% unique(data_alignment$region))) {
    abort(c(
      "`region_tms` value not found in `data_alignment` dataset.",
      i = glue("Regions in `data_alignment` are: {toString(unique(data_alignment$region))}"),
      x = glue("You provided region = {region}.")
      ))
  }
  if (!(year %in% unique(data_alignment$year))) {
    abort(c(
      "`year` value not found in `data_alignment`.",
      i = glue(
        "Years in `data_alignment` are: {toString(unique(data_alignment$year))}
        "
        ),
      x = glue("You provided year = {year}.")
      ))
  }
  abort_if_middle_node_column_not_found(data_alignment, middle_node, env = list(data = substitute(data_alignment)))
  if (!is.null(middle_node2)) {
    abort_if_middle_node_column_not_found(data_alignment, middle_node2, list(data = substitute(data_alignment)))
  }
}

abort_if_middle_node_column_not_found <- function(data, name, env = parent.frame()) {
  .data <- deparse_1(substitute(data, env = env))

  if (!(name %in% names(data))) {
    abort(c(
      glue("Column name you passed as one of the middle nodes not found in {.data}."),
      i = glue(
        "Column names in `{.data}` are: {toString(names(data))}"
        ),
      x = glue("You asked to use column named: `{name}`.")
      ))
  }
}

# Backport `base::deparse1()` to R < 4.0.0
deparse_1 <- function(expr, collapse = " ", width.cutoff = 500L, ...) {
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}
