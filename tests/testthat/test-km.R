test_that("Test curve tabulation.", {
  
  status <- c(0, 0, 1, 0, 1, 0, 0, 0)
  strata <- c(1, 1, 1, 1, 2, 2, 2, 2)
  time <- c(1, 1, 2, 2, 1, 1, 2, 2)
  marg_km <- MargKM(status = status, strata = strata, time = time)
  
  expect_equal(marg_km$nar, c(8, 8, 4))
  expect_equal(marg_km$surv, c(1.0, 0.5 * 1.0 + 0.5 * 0.75, 0.5 * 0.5 + 0.5 * 0.75))
  
})
