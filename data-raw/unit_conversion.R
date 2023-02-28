# unit conversions are taken from: http://wds.iea.org/wds/pdf/WORLDBAL_Documentation.pdf
# last accessed on 27 Feb 2023

unit_conversion <- tibble::tribble(
  ~sector,       ~unit,             ~value_in_mtoe,
  "coal",        "tonnes per year", 7e-07,
  "oil and gas", "GJ",              2.3885e-08,
  "power",       "MWh",             8.598e-08
)

usethis::use_data(unit_conversion, overwrite = TRUE)
