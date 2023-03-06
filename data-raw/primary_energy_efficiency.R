# Physical energy content and primary energy efficiency.
# Power generation based on heat/combustion causes a loss of a share of primary
# energy. To back calculate the primary energy content based on electricity
# metrics, such as power generation, therefore requires dividing the power
# capacity by this efficiency factor to derive the physical energy input.
# In essence, this means that a primary energy equivalent (potentially) higher
# than the power capacity (which refers to electricity only) was used as input
# for a given quantity of generated electricity.

# TODO: values are approximate placeholders and need updating
primary_energy_efficiency <- tibble::tribble(
  ~sector,     ~technology, ~primary_energy_efficiency_factor,
  "power",       "coalcap",             0.30,
  "power",        "gascap",             0.30,
  "power",        "oilcap",             0.30,
  # no combustion/heat
  "power",      "hydrocap",             1,
  "power",    "nuclearcap",             0.30,
  # some renewables (wind, marine, solar PV) use no combustion/heat
  "power", "renewablescap",             0.60
)

usethis::use_data(primary_energy_efficiency, overwrite = TRUE)
