# calculate_company_tech_deviation
test_data_calculate_company_tech_deviation <- tibble::tribble(
       ~sector, ~technology, ~year,  ~region,  ~scenario_source,     ~name_abcd,           ~metric, ~production, ~technology_share,       ~scope, ~percentage_of_initial_production_by_scope,    ~group_id,
  "automotive",  "electric",  2027, "global", "scenario_source", "test_company",       "projected",          25,              0.25,     "sector",                                      0.005, "test_group",
  "automotive",  "electric",  2027, "global", "scenario_source", "test_company", "target_scenario",          20,              0.25,     "sector",                                       0.01, "test_group",
  "automotive",       "ice",  2027, "global", "scenario_source", "test_company",       "projected",          75,              0.75, "technology",                                      0.005, "test_group",
  "automotive",       "ice",  2027, "global", "scenario_source", "test_company", "target_scenario",          60,              0.75, "technology",                                       0.01, "test_group"
)

test_technology_direction <- tibble::tribble(
   ~scenario_source,  ~scenario,      ~sector, ~technology, ~region, ~directional_dummy,
  "scenario_source", "scenario", "automotive", "electric",  "global",                 1,
  "scenario_source", "scenario", "automotive",      "ice",  "global",                -1
)

test_scenario_source <- "scenario_source"
test_scenario <- "scenario"
test_bridge_tech <- "none"

test_output_calculate_company_tech_deviation <- calculate_company_tech_deviation(
  data = test_data_calculate_company_tech_deviation,
  technology_direction = test_technology_direction,
  scenario_source = test_scenario_source,
  scenario = test_scenario,
  bridge_tech = test_bridge_tech
)

test_that("calculate_company_tech_deviation returns deviations and directions as expected", {
  expect_equal(test_output_calculate_company_tech_deviation$total_tech_deviation, c(5, -15))
  expect_equal(test_output_calculate_company_tech_deviation$direction, c("buildout", "phaseout"))
})

# add_total_tech_deviation
test_data_add_total_tech_deviation <- tibble::tribble(
  ~projected,~target_scenario, ~directional_dummy,
          25,              20,                  1,
          25,              20,                 -1
)

test_target_scenario <- paste0("target_", test_scenario)

test_output_add_total_tech_deviation <- add_total_tech_deviation(
  data = test_data_add_total_tech_deviation,
  target_scenario = test_target_scenario
)

test_that("total tech deviation is difference of projected and target times directional dummy", {
  expect_equal(test_output_add_total_tech_deviation$total_tech_deviation, c(5, -5))
})

# add_tech_direction
test_data_add_tech_direction <- tibble::tribble(
  ~directional_dummy,
                  -1,
                   1
)

test_output_add_tech_direction <- add_tech_direction(
  data = test_data_add_tech_direction
)

test_that("tech direction is mapped correctly based on directional dummy", {
  expect_equal(test_output_add_tech_direction$direction, c("phaseout", "buildout"))
})

# add_technology_share_by_direction
test_data_add_technology_share_by_direction <- tibble::tribble(
     ~sector, ~technology,   ~year,  ~region, ~scenario_source,     ~name_abcd, ~projected,    ~group_id, ~direction, ~activity_unit,
  "sector_A", "technology_A", 2027, "global",    "test_source", "test_company",         10, "test_group", "buildout",    "test_unit",
  "sector_A", "technology_B", 2027, "global",    "test_source", "test_company",         20, "test_group", "buildout",    "test_unit",
  "sector_A", "technology_C", 2027, "global",    "test_source", "test_company",         20, "test_group", "phaseout",    "test_unit",
  "sector_B", "technology_D", 2027, "global",    "test_source", "test_company",         20, "test_group", "buildout",    "test_unit",
  "sector_B", "technology_E", 2027, "global",    "test_source", "test_company",         20, "test_group", "phaseout",    "test_unit"
)

test_output_add_technology_share_by_direction <- add_technology_share_by_direction(
  data = test_data_add_technology_share_by_direction
)

test_that("technology shares by direction within a sector are calculated correctly", {
  expect_equal(test_output_add_technology_share_by_direction$technology_share_by_direction, c(0.6, 0.6, 0.4, 0.5, 0.5))
})

