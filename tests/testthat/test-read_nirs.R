test_that("Throws error if file does not exist", {

  expect_error(
    read_nirs("thisdoesnotexist.txt"),
    class = "error_file_dne"
    )

})

test_that("Throws error if file does not have appropriate metadata", {

  expect_error(
    read_nirs("")
  )

})
