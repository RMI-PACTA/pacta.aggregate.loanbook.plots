# calculate_company_tech_deviation----
# styler: off
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
# styler: on

test_scenario_source <- "scenario_source"
test_scenario <- "scenario"
test_bridge_tech <- "none"
test_time_frame <- 5L

test_output_calculate_company_tech_deviation <- calculate_company_tech_deviation(
  data = test_data_calculate_company_tech_deviation,
  technology_direction = test_technology_direction,
  scenario_source = test_scenario_source,
  scenario = test_scenario,
  bridge_tech = test_bridge_tech,
  time_frame = test_time_frame
)

test_that("calculate_company_tech_deviation returns deviations and directions as expected", {
  expect_equal(test_output_calculate_company_tech_deviation$total_tech_deviation, c(5, -15))
  expect_equal(test_output_calculate_company_tech_deviation$direction, c("buildout", "phaseout"))
})

## remove_tech_no_plans_no_target----
test_target_scenario <- paste0("target_", test_scenario)

# 1) zero projected and target values
# styler: off
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
# styler: on

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

## remove_sector_no_target----
# styler: off
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
# styler: on

test_target_scenario <- paste0("target_", test_scenario)

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

## add_total_tech_deviation----
# styler: off
test_data_add_total_tech_deviation <- tibble::tribble(
  ~projected,~target_scenario, ~directional_dummy,
          25,              20,                  1,
          25,              20,                 -1
)
# styler: on

test_target_scenario <- paste0("target_", test_scenario)

test_output_add_total_tech_deviation <- add_total_tech_deviation(
  data = test_data_add_total_tech_deviation,
  target_scenario = test_target_scenario
)

test_that("total tech deviation is difference of projected and target times directional dummy", {
  expect_equal(test_output_add_total_tech_deviation$total_tech_deviation, c(5, -5))
})

## add_tech_direction----
# styler: off
test_data_add_tech_direction <- tibble::tribble(
  ~directional_dummy,
                  -1,
                   1
)
# styler: on

test_output_add_tech_direction <- add_tech_direction(
  data = test_data_add_tech_direction
)

test_that("tech direction is mapped correctly based on directional dummy", {
  expect_equal(test_output_add_tech_direction$direction, c("phaseout", "buildout"))
})

## apply_bridge_technology_cap----
test_bridge_tech <- "bridge_technology"

# styler: off
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
# styler: on

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

# calculate_company_aggregate_alignment_tms----
# styler: off
test_data_calculate_company_aggregate_alignment_tms <- tibble::tribble(
  ~sector,     ~technology, ~year,  ~region, ~scenario_source,     ~name_abcd,    ~group_id, ~projected, ~target_scenario, ~direction, ~total_tech_deviation, ~activity_unit, ~technology_share_by_direction,
  "power",        "gascap",  2027, "global",    "test_source", "test_company", "test_group",        100,               80, "phaseout",                   -20,           "MW",                              1,
  "power", "renewablescap",  2027, "global",    "test_source", "test_company", "test_group",         32,               40, "buildout",                    -8,           "MW",                              1
)
# styler: on

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

## check_consistency_calculate_company_aggregate_alignment_tms----
# styler: off
test_data_consistency_tms <- tibble::tribble(
  ~scenario_source,
  "scenario_source"
)
# styler: on

test_scenario_source <- "scenario_source"
test_bad_source <- "bad_source"

test_that("consistency checks of calculate_company_aggregate_alignment_tms() pass and fail as expected", {
  expect_no_error({
    check_consistency_calculate_company_aggregate_alignment_tms(
      data = test_data_consistency_tms,
      scenario_source = test_scenario_source
    )
  })
  expect_error(
    {
      check_consistency_calculate_company_aggregate_alignment_tms(
        data = test_data_consistency_tms,
        scenario_source = test_bad_source
      )
    },
    regexp = "input value of `scenario_source` not found"
  )
})

## add_technology_share_by_direction----
# styler: off
test_data_add_technology_share_by_direction <- tibble::tribble(
     ~sector, ~technology,   ~year,  ~region, ~scenario_source,     ~name_abcd, ~projected,    ~group_id, ~direction, ~activity_unit,
  "sector_A", "technology_A", 2027, "global",    "test_source", "test_company",         10, "test_group", "buildout",    "test_unit",
  "sector_A", "technology_B", 2027, "global",    "test_source", "test_company",         20, "test_group", "buildout",    "test_unit",
  "sector_A", "technology_C", 2027, "global",    "test_source", "test_company",         20, "test_group", "phaseout",    "test_unit",
  "sector_B", "technology_D", 2027, "global",    "test_source", "test_company",         20, "test_group", "buildout",    "test_unit",
  "sector_B", "technology_E", 2027, "global",    "test_source", "test_company",         20, "test_group", "phaseout",    "test_unit"
)
# styler: on

test_level_net <- "net"
test_level_bo_po <- "bo_po"
test_level_false <- "foo"

test_output_add_technology_share_by_direction_bo_po <- add_technology_share_by_direction(
  data = test_data_add_technology_share_by_direction,
  level = test_level_bo_po
)

test_that("technology shares by direction within a sector are calculated correctly", {
  expect_equal(
    test_output_add_technology_share_by_direction_bo_po$direction,
    test_data_add_technology_share_by_direction$direction
  )
  expect_equal(
    test_output_add_technology_share_by_direction_bo_po$technology_share_by_direction,
    c(0.6, 0.6, 0.4, 0.5, 0.5)
  )
})

test_output_add_technology_share_by_direction_net <- add_technology_share_by_direction(
  data = test_data_add_technology_share_by_direction,
  level = test_level_net
)

test_that("technology shares by direction within a sector are calculated correctly", {
  expect_equal(
    test_output_add_technology_share_by_direction_net$direction,
    rep.int("net", times = nrow(test_data_add_technology_share_by_direction))
  )
  expect_equal(
    test_output_add_technology_share_by_direction_net$technology_share_by_direction,
    rep.int(1, times = nrow(test_data_add_technology_share_by_direction))
  )
})

test_that("technology shares by direction error gracefully with wrong input level", {
  expect_error(
    {
      add_technology_share_by_direction(
        data = test_data_add_technology_share_by_direction,
        level = test_level_false
      )
    },
    "Invalid input provided for argument: level."
  )
})

## add_net_absolute_scenario_value----
# styler: off
test_data_add_net_absolute_scenario_value <- tibble::tribble(
     ~group_id,     ~name_abcd, ~scenario_source,     ~region,        ~sector, ~technology, ~activity_unit, ~year, ~target_scenario, ~direction, ~total_tech_deviation,
  "test_group", "test_company",    "test_source", "somewhere",  "test_sector",    "tech_A",  "output unit",  2027,               20, "buildout",                    -5,
  "test_group", "test_company",    "test_source", "somewhere",  "test_sector",    "tech_B",  "output unit",  2027,               50, "phaseout",                    10,
  "test_group", "test_company",    "test_source", "somewhere",  "test_sector",    "tech_C",  "output unit",  2027,               30, "phaseout",                   -10,
  "test_group", "test_company",    "test_source", "somewhere", "other_sector",    "tech_X",  "output unit",  2027,               30, "buildout",                   -20,
  "test_group", "test_company",    "test_source", "somewhere", "other_sector",    "tech_Y",  "output unit",  2027,         NA_real_, "phaseout",              NA_real_
)
# styler: on

test_target_scenario <- "target_scenario"

test_output_add_net_absolute_scenario_value <- add_net_absolute_scenario_value(
  data = test_data_add_net_absolute_scenario_value,
  target_scenario = test_target_scenario
)

test_that("add_net_absolute_scenario_value adds sum of scenario values as expected", {
  expect_equal(
    test_output_add_net_absolute_scenario_value$net_absolute_scenario_value,
    c(rep.int(100, times = 3), rep.int(30, times = 2))
  )
})

## add_total_deviation----
# styler: off
test_data_add_total_deviation_bo_po <- tibble::tribble(
     ~group_id,     ~name_abcd, ~scenario_source,     ~region,        ~sector, ~technology, ~activity_unit, ~year, ~net_absolute_scenario_value, ~direction, ~total_tech_deviation, ~technology_share_by_direction,
  "test_group", "test_company",    "test_source", "somewhere",  "test_sector",    "tech_A",  "output unit",  2027,                          100, "buildout",                    -5,                            0.1,
  "test_group", "test_company",    "test_source", "somewhere",  "test_sector",    "tech_B",  "output unit",  2027,                          100, "phaseout",                    10,                            0.9,
  "test_group", "test_company",    "test_source", "somewhere",  "test_sector",    "tech_C",  "output unit",  2027,                          100, "phaseout",                   -10,                            0.9,
  "test_group", "test_company",    "test_source", "somewhere", "other_sector",    "tech_X",  "output unit",  2027,                           30, "buildout",                   -20,                            0.8,
  "test_group", "test_company",    "test_source", "somewhere", "other_sector",    "tech_Y",  "output unit",  2027,                           30, "phaseout",                     5,                            0.2
)
# styler: on
test_data_add_total_deviation_net <- test_data_add_total_deviation_bo_po %>%
  dplyr::mutate(
    direction = "net",
    technology_share_by_direction = 1
  )

test_output_add_total_deviation_bo_po <- add_total_deviation(
  data = test_data_add_total_deviation_bo_po
)
test_output_add_total_deviation_net <- add_total_deviation(
  data = test_data_add_total_deviation_net
)

test_that("add_total_deviation adds deviation by sector and direction as expected", {
  expect_equal(
    test_output_add_total_deviation_bo_po$total_deviation,
    c(-5, 0, -20, 5)
  )
  expect_equal(
    test_output_add_total_deviation_net$total_deviation,
    c(-5, -15)
  )
})

## calculate_company_alignment_metric----
# styler: off
test_data_calculate_company_alignment_metric <- tibble::tribble(
     ~group_id,     ~name_abcd,    ~sector, ~activity_unit,     ~region, ~scenario_source, ~year, ~direction, ~total_deviation, ~technology_share_by_direction, ~net_absolute_scenario_value,
  "test_group", "test_company", "sector_a",  "output_unit", "somewhere",    "that_source",  2027,      "net",               20,                              1,                           40,
  "test_group", "test_company", "sector_b",  "output_unit", "somewhere",    "that_source",  2027,      "net",               50,                              1,                           40,
  "test_group", "some_company", "sector_a",  "output_unit", "somewhere",    "that_source",  2027,      "net",               30,                              1,                           30
)
# styler: on
test_scenario <- "some_scenario"

test_output_calculate_company_alignment_metric <- calculate_company_alignment_metric(
  data = test_data_calculate_company_alignment_metric,
  scenario = test_scenario
)

test_that("calculate_company_alignment_metric calculates company alignment metrics correctly", {
  expect_equal(
    test_output_calculate_company_alignment_metric$alignment_metric,
    c(0.5, 1.25, 1)
  )
})

# calculate_company_aggregate_alignment_sda----
# styler: off
test_data_calculate_company_aggregate_alignment_sda <- tibble::tribble(
  ~sector, ~year,  ~region, ~scenario_source,  ~name_abcd,    ~group_id, ~emission_factor_metric, ~emission_factor_value,
  "steel",  2027, "global",    "test_source", "company_A", "test_group",             "projected",                    0.8,
  "steel",  2027, "global",    "test_source", "company_A", "test_group",       "target_scenario",                    0.7,
  "steel",  2027, "global",    "test_source", "company_B", "test_group",             "projected",                   0.55,
  "steel",  2027, "global",    "test_source", "company_B", "test_group",       "target_scenario",                    0.6
)
# styler: on

test_scenario_source <- "test_source"
test_scenario <- "scenario"
test_time_frame <- 5L

test_output_calculate_company_aggregate_alignment_sda <- calculate_company_aggregate_alignment_sda(
  data = test_data_calculate_company_aggregate_alignment_sda,
  scenario_source = test_scenario_source,
  scenario = test_scenario,
  time_frame = test_time_frame
)

added_columns <- c("activity_unit", "scenario", "direction", "total_deviation", "technology_share_by_direction", "alignment_metric")
dropped_columns <- c("emission_factor_metric", "emission_factor_value")
expected_output_columns <- c(names(test_data_calculate_company_aggregate_alignment_sda), added_columns)
expected_output_columns <- expected_output_columns[!expected_output_columns %in% dropped_columns]

expected_output_rows <- test_data_calculate_company_aggregate_alignment_sda %>%
  dplyr::distinct(.data$sector, .data$year, .data$region, .data$scenario_source, .data$name_abcd, .data$group_id) %>%
  nrow()

test_that("calculate_company_aggregate_alignment_sda returns expected structure of outputs", {
  expect_equal(sort(names(test_output_calculate_company_aggregate_alignment_sda)), sort(expected_output_columns))
  expect_equal(nrow(test_output_calculate_company_aggregate_alignment_sda), expected_output_rows)
  expect_equal(unique(test_output_calculate_company_aggregate_alignment_sda$direction), "net")
  expect_equal(unique(test_output_calculate_company_aggregate_alignment_sda$scenario), test_scenario)
})

## check_consistency_calculate_company_aggregate_alignment_sda----
# styler: off
test_data_consistency_sda_1 <- tibble::tribble(
   ~scenario_source, ~emission_factor_metric,
  "scenario_source",         "target_scen_1",
  "scenario_source",         "target_scen_2"
)
test_data_consistency_sda_2 <- tibble::tribble(
   ~scenario_source, ~emission_factor_metric,
  "scenario_source",         "target_scen_2",
  "scenario_source",         "target_scen_3"
)
# styler: on

test_scenario_source <- "scenario_source"
test_bad_source <- "bad_source"
test_scenario <- "scen_1"

test_that("consistency checks of calculate_company_aggregate_alignment_sda() pass and fail as expected", {
  expect_no_error({
    check_consistency_calculate_company_aggregate_alignment_sda(
      data = test_data_consistency_sda_1,
      scenario_source = test_scenario_source,
      scenario = test_scenario
    )
  })
  expect_error(
    {
      check_consistency_calculate_company_aggregate_alignment_sda(
        data = test_data_consistency_sda_1,
        scenario_source = test_bad_source,
        scenario = test_scenario
      )
    },
    regexp = "input value of `scenario_source` not found"
  )
  expect_error(
    {
      check_consistency_calculate_company_aggregate_alignment_sda(
        data = test_data_consistency_sda_2,
        scenario_source = test_scenario_source,
        scenario = test_scenario
      )
    },
    regexp = "input value of `scenario` not found"
  )
})

## prep_and_wrangle_aggregate_alignment_sda----
# styler: off
test_data_prep_and_wrangle_aggregate_alignment_sda_1 <- tibble::tribble(
   ~scenario_source,     ~name_abcd, ~year, ~emission_factor_metric, ~emission_factor_value,
  "scenario_source", "test_company",  2022,             "projected",                    0.9,
  "scenario_source", "test_company",  2022,       "target_scenario",                    0.9,
  "scenario_source", "test_company",  2027,             "projected",                    0.8,
  "scenario_source", "test_company",  2027,       "target_scenario",                    0.7
)
# styler: on

test_scenario_source <- "scenario_source"
test_target_scenario <- "target_scenario"
test_start_year <- 2022
test_time_frame <- 5L

test_output_prep_and_wrangle_aggregate_alignment_sda_1 <- prep_and_wrangle_aggregate_alignment_sda(
  data = test_data_prep_and_wrangle_aggregate_alignment_sda_1,
  scenario_source = test_scenario_source,
  target_scenario = test_target_scenario,
  start_year = test_start_year,
  time_frame = test_time_frame
)

test_output_cols <- names(test_output_prep_and_wrangle_aggregate_alignment_sda_1)
expected_output_cols <- c(names(test_data_prep_and_wrangle_aggregate_alignment_sda_1), "projected", test_target_scenario)
expected_output_cols <- expected_output_cols[!expected_output_cols %in% c("emission_factor_metric", "emission_factor_value")]

test_that("output columns replace emission_factor_* cols with projected and target_scenario", {
  expect_equal(
    test_output_cols,
    expected_output_cols
  )
})

test_nrows <- nrow(test_output_prep_and_wrangle_aggregate_alignment_sda_1)
expected_nrows <- test_data_prep_and_wrangle_aggregate_alignment_sda_1 %>%
  dplyr::distinct(.data$scenario_source, .data$name_abcd, .data$year) %>%
  nrow()

test_that("number of output rows are distinct number of input rows that do not contain emission_factor_* data", {
  expect_equal(
    test_nrows,
    expected_nrows
  )
})

# styler: off
test_data_prep_and_wrangle_aggregate_alignment_sda_2 <- tibble::tribble(
   ~scenario_source,     ~name_abcd, ~year, ~emission_factor_metric, ~emission_factor_value,
  "scenario_source", "test_company",  2021,             "projected",                    0.9,
  "scenario_source", "test_company",  2021,       "target_scenario",                    0.9,
  "scenario_source", "test_company",  2022,             "projected",                    0.9,
  "scenario_source", "test_company",  2022,       "target_scenario",                    0.9,
  "scenario_source", "test_company",  2027,             "projected",                    0.8,
  "scenario_source", "test_company",  2027,       "target_scenario",                    0.7
)
# styler: on

test_time_frame_short <- 4L

test_output_prep_and_wrangle_aggregate_alignment_sda_2 <- prep_and_wrangle_aggregate_alignment_sda(
  data = test_data_prep_and_wrangle_aggregate_alignment_sda_2,
  scenario_source = test_scenario_source,
  target_scenario = test_target_scenario,
  start_year = test_start_year,
  time_frame = test_time_frame_short
)

test_output_years <- unique(test_output_prep_and_wrangle_aggregate_alignment_sda_2$year)
expected_output_year <- test_data_prep_and_wrangle_aggregate_alignment_sda_2 %>%
  dplyr::filter(dplyr::between(.data$year, test_start_year, test_start_year + test_time_frame_short)) %>%
  dplyr::pull(.data$year) %>%
  unique()

test_that("years outside of start_year and start_year + time_frame are dropped", {
  expect_equal(
    test_output_years,
    expected_output_year
  )
})

# add_total_deviation_sda
# styler: off
test_data_add_total_deviation_sda <- tibble::tribble(
     ~group_id,  ~name_abcd,  ~scenario_source,      ~region,    ~sector, ~activity_unit, ~year, ~projected, ~net_absolute_scenario_value,
  "test_group", "company_A", "scenario_source", "some_place", "sector_1",  "output_unit",  2027,        0.8,                          0.7,
  "test_group", "company_A", "scenario_source", "some_place", "sector_2",  "output_unit",  2027,        1.1,                          0.8,
  "test_group", "company_B", "scenario_source", "some_place", "sector_1",  "output_unit",  2027,        0.6,                          0.7
)
# styler: on

test_output_add_total_deviation_sda <- add_total_deviation_sda(
  data = test_data_add_total_deviation_sda
)

test_that("add_total_deviation_sda produces expected deviations", {
  expect_equal(
    test_output_add_total_deviation_sda$total_deviation,
    c(-0.1, -0.3, 0.1)
  )
})
