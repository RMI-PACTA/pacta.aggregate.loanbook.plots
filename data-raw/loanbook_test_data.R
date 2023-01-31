# styler: off
loanbook_test_data <- tibble::tribble(
  ~id_loan, ~id_direct_loantaker, ~name_direct_loantaker, ~id_intermediate_parent_1, ~name_intermediate_parent_1, ~id_ultimate_parent, ~name_ultimate_parent, ~loan_size_outstanding, ~loan_size_outstanding_currency, ~loan_size_credit_limit, ~loan_size_credit_limit_currency, ~sector_classification_system, ~sector_classification_input_type, ~sector_classification_direct_loantaker, ~fi_type, ~flag_project_finance_loan, ~name_project, ~lei_direct_loantaker, ~isin_direct_loantaker,
  "L1", "C1", "edge_case_1", NA_character_, NA_character_, "UP1", "edge_case_1", 1000000, "USD", 1000000, "USD", "NACE", "Code", 3511, "Loan", "No", NA_character_, NA_character_, NA_character_,
  "L2", "C2", "edge_case_2", NA_character_, NA_character_, "UP2", "edge_case_2", 1000000, "USD", 1000000, "USD", "NACE", "Code", 3511, "Loan", "No", NA_character_, NA_character_, NA_character_,
  "L3", "C3", "edge_case_3", NA_character_, NA_character_, "UP3", "edge_case_3", 1000000, "USD", 1000000, "USD", "NACE", "Code", 3511, "Loan", "No", NA_character_, NA_character_, NA_character_,
  "L4", "C4", "test_case_A", NA_character_, NA_character_, "UP4", "test_case_A", 1000000, "USD", 1000000, "USD", "NACE", "Code", 3511, "Loan", "No", NA_character_, NA_character_, NA_character_,
  "L5", "C5", "test_case_B", NA_character_, NA_character_, "UP5", "test_case_B", 1000000, "USD", 1000000, "USD", "NACE", "Code", 3511, "Loan", "No", NA_character_, NA_character_, NA_character_,
  "L6", "C6", "test_case_C", NA_character_, NA_character_, "UP6", "test_case_C", 1000000, "USD", 1000000, "USD", "NACE", "Code", 3511, "Loan", "No", NA_character_, NA_character_, NA_character_,
  "L7", "C7", "test_case_D", NA_character_, NA_character_, "UP7", "test_case_D", 1000000, "USD", 1000000, "USD", "NACE", "Code", 3511, "Loan", "No", NA_character_, NA_character_, NA_character_,
  "L8", "C8", "test_case_E", NA_character_, NA_character_, "UP8", "test_case_E", 1000000, "USD", 1000000, "USD", "NACE", "Code", 3511, "Loan", "No", NA_character_, NA_character_, NA_character_,
  "L9", "C9", "test_case_F", NA_character_, NA_character_, "UP9", "test_case_F", 1000000, "USD", 1000000, "USD", "NACE", "Code", 3511, "Loan", "No", NA_character_, NA_character_, NA_character_,
  "L10", "C10", "test_case_G", NA_character_, NA_character_, "UP10", "test_case_G", 1000000, "USD", 1000000, "USD", "NACE", "Code", 3511, "Loan", "No", NA_character_, NA_character_, NA_character_,
  "L11", "C11", "test_auto_A", NA_character_, NA_character_, "UP11", "test_auto_A", 1000000, "USD", 1000000, "USD", "NACE", "Code", 2920, "Loan", "No", NA_character_, NA_character_, NA_character_,
  "L12", "C12", "test_auto_B", NA_character_, NA_character_, "UP12", "test_auto_B", 1000000, "USD", 1000000, "USD", "NACE", "Code", 2920, "Loan", "No", NA_character_, NA_character_, NA_character_
)
# styler: on

usethis::use_data(loanbook_test_data, overwrite = TRUE)
