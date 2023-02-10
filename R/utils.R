#' Validate that a data frame contains expected columns
#'
#' Validate that all expected columns for an operation are given in a data frame.
#'
#' @param data data frame that is to be validated
#' @param expected_columns Character vector listing the expected columns
#'
#' @return NULL
#' @export
validate_data_has_expected_cols <- function(data,
                                            expected_columns) {
  stopifnot(rlang::is_named(data))
  stopifnot(is.character(expected_columns))

  data_has_expected_columns <-
    all(expected_columns %in% colnames(data))

  if (!data_has_expected_columns) {
    affected_cols <- glue::glue_collapse(sort(setdiff(expected_columns, names(data))), sep = ", ")
    rlang::abort(c(
      "Must include expected columns in data set.",
      x = glue::glue("Missing columns: {affected_cols}."),
      i = "Please check that data have expected columns."
    ))
  }
  invisible()
}
