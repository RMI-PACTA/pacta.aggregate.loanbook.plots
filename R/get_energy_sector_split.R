#' Return multi sector energy companies including sector split
#'
#' @param data data frame containing the asset-based company data (ABCD) in
#'   PACTA for Banks format.
#' @param start_year Numeric vector containing the start year of the analysis.
#' @param primary_energy_efficiency data frame containing efficiency factors for
#'   power technologies that can be used to back calculated the primary energy
#'   content implied in a unit of power capacity.
#' @param unit_conversion data frame containing conversion factors for all
#'   energy sectors covered in PACTA so that the metrics can be transformed to a
#'   single common energy unit.
#'
#' @return NULL
#' @export
get_energy_sector_split <- function(data,
                                    start_year,
                                    primary_energy_efficiency,
                                    unit_conversion) {
  # identify compenies active in more than one energy sector
  multi_sector_companies <- data %>%
    dplyr::filter(
      .data$sector %in% c("coal", "oil and gas", "power")
    ) %>%
    dplyr::distinct(.data$company_id, .data$sector) %>%
    dplyr::summarise(n = dplyr::n(), .by = "company_id") %>%
    dplyr::filter(n > 1) %>%
    dplyr::pull(.data$company_id)

  # TODO: should the split always be based on the entire company production or
  # should it be scoped to the scenario region?
  sector_split_energy_companies <- data %>%
    dplyr::filter(
      .data$company_id %in% .env$multi_sector_companies,
      .data$sector %in% c("coal", "oil and gas", "power"),
      .data$year == .env$start_year
    )

  # adjust power capacity by primary energy efficiency
  sector_split_energy_companies_power <- sector_split_energy_companies %>%
    dplyr::filter(.data$sector == "power") %>%
    dplyr::inner_join(primary_energy_efficiency, by = c("sector", "technology")) %>%
    dplyr::mutate(
      production = .data$production / .data$primary_energy_efficiency_factor
    ) %>%
    dplyr::select(-"primary_energy_efficiency_factor")

  # transform all energy sectors to common unit of energy: mtoe
  sector_split_energy_companies <- sector_split_energy_companies %>%
    dplyr::filter(.data$sector != "power") %>%
    dplyr::bind_rows(sector_split_energy_companies_power) %>%
    dplyr::summarise(
      production = sum(.data$production, na.rm = TRUE),
      .by = c("company_id", "name_company", "sector", "year", "production_unit")
    ) %>%
    dplyr::inner_join(
      unit_conversion,
      by = c("sector", "production_unit" = "unit")
    ) %>%
    dplyr::mutate(
      production = .data$production * .data$value_in_mtoe,
      production_unit = "mtoe"
    ) %>%
    dplyr::select(-"value_in_mtoe")

  # get the sector split for each company based on common energy units
  sector_split_energy_companies <- sector_split_energy_companies %>%
    dplyr::mutate(
      sector_split = .data$production / sum(.data$production, na.rm = TRUE),
      .by = c("company_id", "name_company", "year", "production_unit")
    ) %>%
    dplyr::select(c("company_id", "name_company", "sector", "production_unit", "production", "sector_split")) %>%
    dplyr::distinct()

  return(sector_split_energy_companies)
}
