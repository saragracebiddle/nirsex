test_that("nirsex_examples() returns character vector of files in
          ~/inst/extdata/", {
  expect_type(nirsex_example(), "character")

  expect_type(nirsex_example("nirs_example.txt"), "character")

  expect_error(nirsex_example("idonotexist.txt"))
})
