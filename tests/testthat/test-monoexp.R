test_that("monoexp() works", {

  exp <- data.frame(x = seq_len(120),
                    y = 5 + 13*(1-exp(-(1:120 - 2) / 25))
                    )

  expect_equal(monoexp(Amp = 13, Baseline = 5, TD = 2, tau = 25, xlen = 120),
               exp)
})
