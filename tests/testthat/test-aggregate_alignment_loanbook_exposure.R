# aggregate_alignment_loanbook_exposure----

# styler: off
test_data_aggregate_alignment_loanbook_exposure_net <- tibble::tribble(
     ~group_id, ~name_abcd,       ~sector, ~activity_unit,  ~region, ~scenario_source,       ~scenario, ~year, ~direction, ~total_deviation, ~alignment_metric,
  "test_group", "test_company_1", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",             -110,              -0.3,
  "test_group", "test_company_2", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",              -70,              -0.4,
  "test_group", "test_company_3", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",              -40,              -0.2
)
# styler: on

# styler: off
test_data_aggregate_alignment_loanbook_exposure_bopo <- tibble::tribble(
     ~group_id, ~name_abcd,       ~sector, ~activity_unit,  ~region, ~scenario_source,       ~scenario, ~year, ~direction, ~total_deviation, ~alignment_metric,
  "test_group", "test_company_1", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "buildout",              -10,             -0.05,
  "test_group", "test_company_1", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",             -100,             -0.25,
  "test_group", "test_company_2", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "buildout",              -50,             -0.35,
  "test_group", "test_company_2", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",              -20,             -0.05,
  "test_group", "test_company_3", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",              -40,              -0.2
)
# styler: on

# styler: off
test_matched <- tibble::tribble(
     ~group_id, ~id_loan, ~loan_size_outstanding, ~loan_size_outstanding_currency,       ~name_abcd, ~sector,
  "test_group",     "L1",                 300000,                           "USD", "test_company_1", "power",
  "test_group",     "L2",                 700000,                           "USD", "test_company_2", "power",
  "test_group",     "L3",                1000000,                           "USD", "test_company_3", "power"
)
# styler: on

test_output_aggregate_alignment_loanbook_exposure_net <- test_data_aggregate_alignment_loanbook_exposure_net %>%
  aggregate_alignment_loanbook_exposure(matched = test_matched)

test_output_aggregate_alignment_loanbook_exposure_bopo <- test_data_aggregate_alignment_loanbook_exposure_bopo %>%
  aggregate_alignment_loanbook_exposure(matched = test_matched)

test_that("aggregated net alignment equals sum of aggregated buildout and phaseout alignments", {
  expect_equal(
    test_output_aggregate_alignment_loanbook_exposure_net$exposure_weighted_net_alignment,
    sum(test_output_aggregate_alignment_loanbook_exposure_bopo$exposure_weighted_net_alignment, na.rm = TRUE)
  )
})

test_that("aggregated net loan size equals sum of aggregated buildout and phaseout loan size", {
  expect_equal(
    test_output_aggregate_alignment_loanbook_exposure_net$sum_loan_size_outstanding,
    sum(test_output_aggregate_alignment_loanbook_exposure_bopo$sum_loan_size_outstanding, na.rm = TRUE)
  )
})

test_that("aggregated loan size equals sum of matched loan size", {
  expect_equal(
    sum(test_output_aggregate_alignment_loanbook_exposure_bopo$sum_loan_size_outstanding, na.rm = TRUE),
    sum(test_matched$loan_size_outstanding, na.rm = TRUE)
  )
  expect_equal(
    sum(test_output_aggregate_alignment_loanbook_exposure_net$sum_loan_size_outstanding, na.rm = TRUE),
    sum(test_matched$loan_size_outstanding, na.rm = TRUE)
  )
})
