# Physical energy content and primary energy efficiency.
# Power generation based on heat/combustion causes a loss of a share of primary
# energy. To back calculate the primary energy content based on electricity
# metrics, such as power generation, therefore requires dividing the power
# capacity by this efficiency factor to derive the physical energy input.
# We apply such factors for fossil fuel based power generation only, since we
# are interested in approximating the exposure to fossil fuels.
# Values for efficiency of electricity production are taken from IEA "Energy
# Efficiency Indicators for Public Electricity Prodcution from Fossil Fuels" at
# https://iea.blob.core.windows.net/assets/acaecb98-4430-4395-a4fa-d1a4d5ccb3d3/EnergyEfficiencyIndicatorsforPublicElectricityProductionfromFossilFuels.pdf
# last accessed on 15 March, 2023.

primary_energy_efficiency <- tibble::tribble(
  ~region,  ~sector,     ~technology, ~primary_energy_efficiency_factor,
  "global", "power",       "coalcap",                             0.343,
  "global", "power",        "gascap",                             0.395,
  "global", "power",        "oilcap",                             0.365,
  "global", "power",      "hydrocap",                                 1,
  "global", "power",    "nuclearcap",                                 1,
  "global", "power", "renewablescap",                                 1
)

usethis::use_data(primary_energy_efficiency, overwrite = TRUE)
