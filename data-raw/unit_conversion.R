# unit conversions are taken from: https://www.iea.org/data-and-statistics/data-tools/unit-converter
# last accessed on 23 Feb 2023
# MW are yearly capacity.
# we multiply the unit conversion by 365.25 days and 24 hours to get the MW per
# year conversion
MWh_in_GJ <- 3.6
MW_in_GJ <- MWh_in_GJ * 24 * 365.25

unit_conversion <- tibble::tribble(
  ~sector,       ~unit,             ~value_in_GJ,
  "coal",        "tonnes per year", 29.31,
  "oil and gas", "GJ",              1,
  "power",       "MW",              MW_in_GJ
)

usethis::use_data(unit_conversion, overwrite = TRUE)
