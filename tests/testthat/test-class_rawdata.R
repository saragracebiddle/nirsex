test_that("constructor works", {


  rawdata <- new_rawdata(
    data.frame(samp_num = c(1,2,3),
               S1_D1_759 = c(1,1,1),
               S1_D1_856 = c(2,2,2)),
    info(chs = data.frame(
      ch_name = c("S1_D1_759", "S1_D1_856"),
      ch_types = c("hbo", "hbr"),
      source_num = c(1L,1L),
      det_num = c(1L, 1L),
      wavelength = c(759L, 856L)
    ))

  )

  expect_s3_class(rawdata, c("rawdata", "list"))
})
