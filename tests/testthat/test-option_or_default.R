test_that("option_of_default works", {
  # value from list
  expect_equal(option_or_default(list("A" = 25), "A", 20), 25)
  
  # default value
  expect_equal(option_or_default(list("A" = 25), "B", 20), 20)
})
