test_that("get_metadata works", {

  ex = scan(test_path("fixtures", "metadata_complete.txt"),
            sep = "\t",
            what = character(), quiet = T, skipNul = T
  )

  expect_error(get_metadata(s = ex, type = "noexist"),
               class = "error_unsupp_metadata")

  expect_error(get_metadata(s = ex, type = 1),
               class = "error_bad_arg")

  expect_equal(get_metadata(s = ex,
                            type = "num_samples"),
               as.integer(14921))

  expect_equal(get_metadata(s = ex,
                            type = "hz"),
               as.double(10.00))

  expect_match(get_metadata(s = ex,
                            type = "optode_template"),
               "PortaMon TSI")

  expect_equal(
    get_metadata(s = ex, type = "legend"),
    data.frame(
      "X1" = c("1","2","3","4",'5','6','7','8','9', '10'),
      "X2" =c('(Sample number)',
        'Rx1-Tx1 O2Hb (Mov avg) (NIRS002_FP_02_10_15_2021)',
        'Rx1-Tx1 HHb (Mov avg) (NIRS002_FP_02_10_15_2021)',
        'Rx1-Tx2 O2Hb (Mov avg) (NIRS002_FP_02_10_15_2021)',
        'Rx1-Tx2 HHb (Mov avg) (NIRS002_FP_02_10_15_2021)',
        'Rx1-Tx3 O2Hb (Mov avg) (NIRS002_FP_02_10_15_2021)',
        'Rx1-Tx3 HHb (Mov avg) (NIRS002_FP_02_10_15_2021)',
        'Rx1-Tx1,Tx2,Tx3 TSI% (Mov avg) (NIRS002_FP_02_10_15_2021)',
        'Rx1-Tx1,Tx2,Tx3 TSI Fit Factor (Mov avg) (NIRS002_FP_02_10_15_2021)',
        '(Event)'

        )
               ))

  ex = scan(test_path("fixtures", "metadata_no_numsamples.txt"),
            sep = "\t",
            what = character(), quiet = T, skipNul = T
  )

  expect_error(get_metadata(s = ex, type = "num_samples"),
               class = "error_file_metadata")

  ex = scan(test_path("fixtures", "metadata_no_optodetemplate.txt"),
            sep = "\t",
            what = character(),
            quiet = T,
            skipNul = T
  )

  expect_error(get_metadata(s = ex, type = "optode_template"),
               class = "error_file_metadata")

  ex = scan(test_path("fixtures", "metadata_no_hz.txt"),
            sep = "\t",
            what = character(), quiet = T, skipNul = T
  )

  expect_error(get_metadata(s = ex, type = "hz"),
               class = "error_file_metadata")



})
