# Physical energy content and primary energy efficiency.
# Power generation based on heat/combustion causes a loss of a share of primary
# energy. To back calculate the primary energy content based on electricity
# metrics, such as power generation, therefore requires dividing the power
# capacity by this efficiency factor to derive the physical energy input.
# We apply such factors for fossil fuel based power generation only, since we
# are interested in approximating the exposure to fossil fuels.

# TODO: values are approximate placeholders and need updating
primary_energy_efficiency <- tibble::tribble(
  ~sector,     ~technology, ~primary_energy_efficiency_factor,
  "power",       "coalcap",                              0.30,
  "power",        "gascap",                              0.30,
  "power",        "oilcap",                              0.30,
  "power",      "hydrocap",                                 1,
  "power",    "nuclearcap",                                 1,
  "power", "renewablescap",                                 1
)

usethis::use_data(primary_energy_efficiency, overwrite = TRUE)
