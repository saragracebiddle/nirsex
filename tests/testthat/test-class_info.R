test_that("constructor works", {

  basic_info <- list(
    "chs" = data.frame(
      ch_name = c("1", "2", "3"),
      ch_type = c("hbo","hbr", "misc"),
      source_num = c(1L,1L,1L),
      det_num = c(1L,2L,3L),
      wavelength = c(756L, 761L, 854L)
    )
  )

  info <- new_info(basic_info)

  expect_s3_class(info, c("info", "list"))
})
