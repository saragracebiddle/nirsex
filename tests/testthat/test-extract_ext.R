test_that("extract_ext() works", {

  expect_match(extract_ext("file.txt"), "txt")

  expect_match(extract_ext('file.csv'), 'csv')

  expect_match(extract_ext('file.xlsx'), 'xlsx')

  expect_error(extract_ext('file.oxy4'))

  expect_no_condition(extract_ext(fs::path_package('inst','extdata','nirs_example.txt', package = 'nirsex')))

})
