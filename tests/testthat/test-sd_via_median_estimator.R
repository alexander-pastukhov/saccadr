test_that("sd_via_median_estimator works", {
  # scalar
  expect_equal(length(sd_via_median_estimator(rnorm(n = 100))), 1)
  
  # positive
  expect_true(sd_via_median_estimator(rnorm(n = 100)) > 0)
})
