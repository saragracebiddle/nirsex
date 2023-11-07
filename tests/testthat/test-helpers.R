test_that("read_nirs_examples returns character vector of files in
          ~/inst/extdata/", {
  expect_type(read_nirs_example(), "character")

  expect_type(read_nirs_example("opticaldensity_raw.txt"), "character")

  expect_error(read_nirs_example("idonotexist.txt"))
})
