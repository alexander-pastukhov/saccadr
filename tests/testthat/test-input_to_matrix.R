test_that("input_to_matrix works", {
  # vector to matrix
  expect_true(is.matrix(input_to_matrix(rnorm(n = 100))))
  
  # data.frame to matrix
  data("single_trial_binocular")
  expect_true(is.matrix(input_to_matrix( single_trial_binocular[, c("xL", "xR")])))
})
