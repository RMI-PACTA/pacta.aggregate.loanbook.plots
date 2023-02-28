#' Return multi sector energy companies including sector split
#'
#' @param data data frame containing the asset-based company data (ABCD) in
#'   PACTA for Banks format.
#' @param start_year Numeric vector containing the start year of the analysis.
#'
#' @return NULL
#' @export
get_energy_sector_split <- function(data,
                                    start_year) {
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

  # transform power capacity to generation (MW -> MWh)
  # MW are yearly capacity. We therefore apply the capacity factor and multiply by
  # 365.25 days and 24 hours
  capacity_factors <- capacity_factor_power %>%
    dplyr::select(c("technology", "capacity_factor")) %>%
    dplyr::distinct()

  sector_split_energy_companies_power <- sector_split_energy_companies %>%
    dplyr::filter(.data$sector == "power") %>%
    dplyr::inner_join(capacity_factors, by = c("technology")) %>%
    dplyr::mutate(
      production = .data$production * .data$capacity_factor * 365.25 * 24,
      production_unit = "MWh"
    )

  # transform all energy sectors to common unit of energy: mtoe
  sector_split_energy_companies <- sector_split_energy_companies %>%
    dplyr::filter(.data$sector != "power") %>%
    dplyr::bind_rows(sector_split_energy_companies_power) %>%
    dplyr::summarise(
      production = sum(.data$production, na.rm = TRUE),
      .by = c("company_id", "name_company", "lei", "is_ultimate_owner", "sector", "year", "production_unit")
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
      sector_split = production / sum(production, na.rm = TRUE),
      .by = c("company_id", "name_company", "lei", "is_ultimate_owner", "year", "production_unit")
    ) %>%
    dplyr::select(c("company_id", "name_company", "sector", "production_unit", "production", "sector_split")) %>%
    dplyr::distinct()

  return(sector_split_energy_companies)
}
