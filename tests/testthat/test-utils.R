test_that("get_ch_names_by_type works", {
  info <- info(chs = data.frame(ch_name = c("A", "B", "C", "D"),
                                ch_type = c("hbo","hbr","hbo","hbr")))

  expect_equal(
    get_ch_names_by_type(info), list("hbo" = c("A", "C"),
                                     "hbr" = c("B", "D"))
  )
})


test_that("prep_plot_data works", {

  rawdata <- new_rawdata(data = data.frame(
    "samp_num" = seq(1,10),
    "A" = rep(1, times = 10),
    "B" = rep(2, times =  10),
    "C" = rep(3, times = 10),
    "D" = rep(4, times = 10)
  ),
  info = info(chs = data.frame(
    ch_name = c("A","B","C","D"),
    ch_type = c("hbo","hbr","hbo","hbr")
  )))

  expect_equal(prep_plot_data(rawdata),
               data.frame(
                 "samp_num" = rep(seq(1,10), times = 4),
                 "ch_name" = rep(c("A","B","C","D"), each = 10),
                 "value" = rep(c(1,2,3,4), each = 10),
                 "ch_type" = rep(rep(c("hbo","hbr"), each = 10), times = 2)
               ))


  rawdata <- new_rawdata(data = data.frame(
    "samp_num" = seq(1,10),
    "A" = rep(1, times = 10),
    "B" = rep(2, times =  10),
    "C" = rep(3, times = 10),
    "D" = rep(4, times = 10)
  ),
  info = info(chs = data.frame(
    ch_name = c("A","B","C","D"),
    ch_type = c("hbo","hbr","hbo","tsi")
  )))
})
