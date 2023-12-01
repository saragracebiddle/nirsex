test_that("protocol constructor works", {
  p = data.frame(id = as.integer(c(1,2)),
                 description = c("a","b"),
                 duration = c(1,2),
                 endtime = c(1,2),
                 starttime = c(1,2))

  expect_s3_class(new_protocol(p), "nirsex_protocol")

  expect_error(new_protocol(c(1,2)))
})

test_that("protocol validator works",{
  p = data.frame(id = as.integer(c(1,2)),
                 description = c("a","b"),
                 duration = c(1,2),
                 endtime = c(1,2),
                 starttime = c(1,2))

  expect_equal(validate_protocol(p), p)
  # correct columns in different order
  expect_equal(validate_protocol(p[c(2,3,4,1,5)]), p[c(2,3,4,1,5)])
  # not enough columns
  expect_error(validate_protocol(p[c("id","description","duration","starttime")]))
  # too many columns
  expect_error(validate_protocol(data.frame(id = NULL,
                                            description = NULL,
                                            duration = NULL,
                                            starttime = NULL,
                                            endtime = NULL,
                                            extracol = NULL)))
  # correct number of columns, wrong names
  expect_error(validate_protocol(data.frame(id = NULL,
                                            id = NULL,
                                            description = NULL,
                                            duration = NULL,
                                            starttime = NULL)))
  # incorrect column types
  expect_error(validate_protocol(
    data.frame(id = c(1,2,3),
               description = c("a","b","c"),
               duration = c(1,1,1),
               starttime = c(0,2,4),
               endtime = c(1,3,5))
  ))


})

test_that("protocol helper works",{
  d = c("a","b","c")
  p_1dur = new_protocol(data.frame(id = as.integer(1:length(d)),
                      description = d,
                      duration = c(1,1,1),
                      endtime = c(1,2,3),
                      starttime = c(0,1,2),
                      row.names = 1:3))
  p_3dur = new_protocol(data.frame(id = as.integer(1:length(d)),
                     description = d,
                     duration = c(1,2,3),
                     endtime = c(1,3,6),
                     starttime = c(0,2,5),
                     row.names = 1:3))
  order = c(2,3,1)
  p_ordered_3dur = new_protocol(data.frame(id = as.integer(c(2,3,1)),
                         description = c("b","c","a"),
                         duration = c(2,3,1),
                         endtime = c(2,5,6),
                         starttime = c(0,3,4),
                         row.names = as.integer(c(2,3,1))
                         ))
  p_ordered_1dur = new_protocol(data.frame(id = as.integer(c(2,3,1)),
                              description = c("b","c","a"),
                              duration = c(1,1,1),
                              endtime = c(1,2,3),
                              starttime = c(0,1,2),
                              row.names = as.integer(c(2,3,1))
                              ))
  expect_s3_class(protocol(descriptions = d, duration = c(1,2,3)), "nirsex_protocol")
  expect_equal(protocol(d, c(1,2,3)), p_3dur)
  expect_equal(protocol(d, c(1)), p_1dur)
  expect_equal(protocol(d, c(1,2,3), order = c(2,3,1)), p_ordered_3dur)
  expect_equal(protocol(d, c(1), order = c(2,3,1)), p_ordered_1dur)
})
