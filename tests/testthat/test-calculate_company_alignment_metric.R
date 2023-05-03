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

# remove_tech_no_plans_no_target
test_target_scenario <- paste0("target_", test_scenario)

# 1) zero projected and target values
test_data_remove_tech_no_plans_no_target_1 <- tibble::tribble(
       ~sector, ~technology, ~year,  ~region,  ~scenario_source,     ~name_abcd, ~projected, ~target_scenario,    ~group_id,
  "automotive",  "electric",  2027, "global", "scenario_source", "test_company",         25,               20, "test_group",
  "automotive",       "ice",  2027, "global", "scenario_source", "test_company",          0,                0, "test_group"
)
# 2) zero projected value, positive target
test_data_remove_tech_no_plans_no_target_2 <- tibble::tribble(
       ~sector, ~technology, ~year,  ~region,  ~scenario_source,     ~name_abcd, ~projected, ~target_scenario,    ~group_id,
  "automotive",  "electric",  2027, "global", "scenario_source", "test_company",         25,               20, "test_group",
  "automotive",       "ice",  2027, "global", "scenario_source", "test_company",          0,               10, "test_group"
)
# 3) positive projected value, zero target
test_data_remove_tech_no_plans_no_target_3 <- tibble::tribble(
       ~sector, ~technology, ~year,  ~region,  ~scenario_source,     ~name_abcd, ~projected, ~target_scenario,    ~group_id,
  "automotive",  "electric",  2027, "global", "scenario_source", "test_company",         25,               20, "test_group",
  "automotive",       "ice",  2027, "global", "scenario_source", "test_company",         10,                0, "test_group"
)
# 4) positive projected and target values
test_data_remove_tech_no_plans_no_target_4 <- tibble::tribble(
       ~sector, ~technology, ~year,  ~region,  ~scenario_source,     ~name_abcd, ~projected, ~target_scenario,    ~group_id,
  "automotive",  "electric",  2027, "global", "scenario_source", "test_company",         25,               20, "test_group",
  "automotive",       "ice",  2027, "global", "scenario_source", "test_company",         75,               60, "test_group"
)
# 5) NAs in projected and target values
# TODO: reconsider if this needs to be handled somewhere else, effectively NAs are treated like zero
test_data_remove_tech_no_plans_no_target_5 <- tibble::tribble(
       ~sector, ~technology, ~year,  ~region,  ~scenario_source,     ~name_abcd, ~projected, ~target_scenario,    ~group_id,
  "automotive",  "electric",  2027, "global", "scenario_source", "test_company",   NA_real_,         NA_real_, "test_group",
  "automotive",       "ice",  2027, "global", "scenario_source", "test_company",   NA_real_,         NA_real_, "test_group"
)
# 6) NA in one of projected and target values
# TODO: reconsider if this needs to be handled somewhere else, effectively NAs are treated like zero
test_data_remove_tech_no_plans_no_target_6 <- tibble::tribble(
       ~sector, ~technology, ~year,  ~region,  ~scenario_source,     ~name_abcd, ~projected, ~target_scenario,    ~group_id,
  "automotive",  "electric",  2027, "global", "scenario_source", "test_company",   NA_real_,         NA_real_, "test_group",
  "automotive",       "ice",  2027, "global", "scenario_source", "test_company",         40,               20, "test_group"
)

test_output_remove_tech_no_plans_no_target_1 <- remove_tech_no_plans_no_target(
  data = test_data_remove_tech_no_plans_no_target_1,
  target_scenario = test_target_scenario
)
test_output_remove_tech_no_plans_no_target_2 <- remove_tech_no_plans_no_target(
  data = test_data_remove_tech_no_plans_no_target_2,
  target_scenario = test_target_scenario
)
test_output_remove_tech_no_plans_no_target_3 <- remove_tech_no_plans_no_target(
  data = test_data_remove_tech_no_plans_no_target_3,
  target_scenario = test_target_scenario
)
test_output_remove_tech_no_plans_no_target_4 <- remove_tech_no_plans_no_target(
  data = test_data_remove_tech_no_plans_no_target_4,
  target_scenario = test_target_scenario
)
test_output_remove_tech_no_plans_no_target_5 <- remove_tech_no_plans_no_target(
  data = test_data_remove_tech_no_plans_no_target_5,
  target_scenario = test_target_scenario
)
test_output_remove_tech_no_plans_no_target_6 <- remove_tech_no_plans_no_target(
  data = test_data_remove_tech_no_plans_no_target_6,
  target_scenario = test_target_scenario
)

test_that("only rows with zero values in both projected and target values are removed", {
  expect_equal(nrow(test_output_remove_tech_no_plans_no_target_1), 1)
  expect_equal(nrow(test_output_remove_tech_no_plans_no_target_2), 2)
  expect_equal(nrow(test_output_remove_tech_no_plans_no_target_3), 2)
  expect_equal(nrow(test_output_remove_tech_no_plans_no_target_4), 2)
  expect_equal(nrow(test_output_remove_tech_no_plans_no_target_5), 0)
  expect_equal(nrow(test_output_remove_tech_no_plans_no_target_6), 1)
})

# remove_sector_no_target
test_target_scenario <- paste0("target_", test_scenario)

# 1) zero values in all target values of sector
test_data_remove_sector_no_target_1 <- tibble::tribble(
       ~sector, ~technology, ~year,  ~region,  ~scenario_source,     ~name_abcd, ~projected, ~target_scenario,    ~group_id,
  "automotive",  "electric",  2027, "global", "scenario_source", "test_company",         25,                0, "test_group",
  "automotive",       "ice",  2027, "global", "scenario_source", "test_company",         10,                0, "test_group"
)
# 2) zero values in some target values of sector
test_data_remove_sector_no_target_2 <- tibble::tribble(
       ~sector, ~technology, ~year,  ~region,  ~scenario_source,     ~name_abcd, ~projected, ~target_scenario,    ~group_id,
  "automotive",  "electric",  2027, "global", "scenario_source", "test_company",         25,                0, "test_group",
  "automotive",       "ice",  2027, "global", "scenario_source", "test_company",         10,                5, "test_group"
)
# 3) zero values in all projected values of sector, but not target
test_data_remove_sector_no_target_3 <- tibble::tribble(
       ~sector, ~technology, ~year,  ~region,  ~scenario_source,     ~name_abcd, ~projected, ~target_scenario,    ~group_id,
  "automotive",  "electric",  2027, "global", "scenario_source", "test_company",          0,                0, "test_group",
  "automotive",       "ice",  2027, "global", "scenario_source", "test_company",          0,                5, "test_group"
)
# 4) NA values in all target values of sector
# TODO: reconsider if this needs to be handled somewhere else, effectively NAs are treated like zero
test_data_remove_sector_no_target_4 <- tibble::tribble(
       ~sector, ~technology, ~year,  ~region,  ~scenario_source,     ~name_abcd, ~projected, ~target_scenario,    ~group_id,
  "automotive",  "electric",  2027, "global", "scenario_source", "test_company",         25,         NA_real_, "test_group",
  "automotive",       "ice",  2027, "global", "scenario_source", "test_company",         10,         NA_real_, "test_group"
)

test_output_remove_sector_no_target_1 <- remove_sector_no_target(
  data = test_data_remove_sector_no_target_1,
  target_scenario = test_target_scenario
)
test_output_remove_sector_no_target_2 <- remove_sector_no_target(
  data = test_data_remove_sector_no_target_2,
  target_scenario = test_target_scenario
)
test_output_remove_sector_no_target_3 <- remove_sector_no_target(
  data = test_data_remove_sector_no_target_3,
  target_scenario = test_target_scenario
)
test_output_remove_sector_no_target_4 <- remove_sector_no_target(
  data = test_data_remove_sector_no_target_4,
  target_scenario = test_target_scenario
)

test_that("only company sector combinations with zero values in all target rows are removed", {
  expect_equal(nrow(test_output_remove_sector_no_target_1), 0)
  expect_equal(nrow(test_output_remove_sector_no_target_2), 2)
  expect_equal(nrow(test_output_remove_sector_no_target_3), 2)
  expect_equal(nrow(test_output_remove_sector_no_target_4), 0)
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

# apply_bridge_technology_cap
test_bridge_tech <- "bridge_technology"

test_data_apply_bridge_technology_cap_1 <- tibble::tribble(
          ~technology, ~total_tech_deviation,
  "bridge_technology",                    10,
  "bridge_technology",                   -10
)
test_data_apply_bridge_technology_cap_2 <- tibble::tribble(
  ~technology, ~total_tech_deviation,
  "bridge_technology",                    10,
  "bridge_technology",                     0
)
test_data_apply_bridge_technology_cap_3 <- tibble::tribble(
         ~technology, ~total_tech_deviation,
  "other_technology",                    10,
  "other_technology",                   -10
)
test_data_apply_bridge_technology_cap_4 <- tibble::tribble(
         ~technology, ~total_tech_deviation,
  "other_technology",                    10,
  "other_technology",                     0
)


test_output_apply_bridge_technology_cap_1 <- apply_bridge_technology_cap(
  data = test_data_apply_bridge_technology_cap_1,
  bridge_tech = test_bridge_tech
)
test_output_apply_bridge_technology_cap_2 <- apply_bridge_technology_cap(
  data = test_data_apply_bridge_technology_cap_2,
  bridge_tech = test_bridge_tech
)
test_output_apply_bridge_technology_cap_3 <- apply_bridge_technology_cap(
  data = test_data_apply_bridge_technology_cap_3,
  bridge_tech = test_bridge_tech
)
test_output_apply_bridge_technology_cap_4 <- apply_bridge_technology_cap(
  data = test_data_apply_bridge_technology_cap_4,
  bridge_tech = test_bridge_tech
)

test_that("total_tech_deviation is less or equal 0 for all technologies in bridge tech, but unchanged else", {
  expect_true(all(sign(test_output_apply_bridge_technology_cap_1$total_tech_deviation) == -1))
  expect_true(all(sign(test_output_apply_bridge_technology_cap_2$total_tech_deviation) %in% c(-1, 0)))
  expect_equal(sign(test_output_apply_bridge_technology_cap_3$total_tech_deviation), sign(test_data_apply_bridge_technology_cap_3$total_tech_deviation))
  expect_equal(sign(test_output_apply_bridge_technology_cap_4$total_tech_deviation), sign(test_data_apply_bridge_technology_cap_4$total_tech_deviation))
})

# calculate_company_aggregate_alignment_tms

test_data_calculate_company_aggregate_alignment_tms <- tibble::tribble(
  ~sector,     ~technology, ~year,  ~region, ~scenario_source,     ~name_abcd,    ~group_id, ~projected, ~target_scenario, ~direction, ~total_tech_deviation, ~activity_unit, ~technology_share_by_direction,
  "power",        "gascap",  2027, "global",    "test_source", "test_company", "test_group",        100,               80, "phaseout",                   -20,           "MW",                              1,
  "power", "renewablescap",  2027, "global",    "test_source", "test_company", "test_group",         32,               40, "buildout",                    -8,           "MW",                              1
)

test_scenario_source <- "test_source"
test_scenario <- "scenario"
test_level_net <- "net"
test_level_bo_po <- "bo_po"

test_output_calculate_company_aggregate_alignment_tms_1 <- calculate_company_aggregate_alignment_tms(
  data = test_data_calculate_company_aggregate_alignment_tms,
  scenario_source = test_scenario_source,
  scenario = test_scenario,
  level = test_level_net
)
test_output_calculate_company_aggregate_alignment_tms_2 <- calculate_company_aggregate_alignment_tms(
  data = test_data_calculate_company_aggregate_alignment_tms,
  scenario_source = test_scenario_source,
  scenario = test_scenario,
  level = test_level_bo_po
)

# TODO: add expectations for the actual output values
test_that("calculate_company_aggregate_alignment_tms returns expected outputs", {
  expect_equal(test_output_calculate_company_aggregate_alignment_tms_1$direction, c("net"))
  expect_equal(nrow(test_output_calculate_company_aggregate_alignment_tms_1), length(unique(test_data_calculate_company_aggregate_alignment_tms$name_abcd)))
  expect_equal(test_output_calculate_company_aggregate_alignment_tms_1$total_deviation, sum(test_data_calculate_company_aggregate_alignment_tms$total_tech_deviation))
  expect_equal(test_output_calculate_company_aggregate_alignment_tms_2$direction, test_data_calculate_company_aggregate_alignment_tms$direction)
  expect_equal(nrow(test_output_calculate_company_aggregate_alignment_tms_2), 2 * length(unique(test_data_calculate_company_aggregate_alignment_tms$name_abcd)))
  expect_equal(test_output_calculate_company_aggregate_alignment_tms_2$total_deviation, test_data_calculate_company_aggregate_alignment_tms$total_tech_deviation)
})
