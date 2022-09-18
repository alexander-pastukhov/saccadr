test_that("extract_saccades does not work for mismatching x and y", {
  # monocular x and y: sizes do not match
  expect_error(extract_saccades(x = rnorm(n = 100), y = rnorm(n = 200), sample_rate = 250))
  
  # binocular x and monocular y
  expect_error(extract_saccades(x = matrix(rnorm(n = 400), ncol = 2),
                                y = rnorm(n = 200), 
                                sample_rate = 250))
  
  # binocular x and y: sizes do not match
  expect_error(extract_saccades(x = matrix(rnorm(n = 400), ncol = 2),
                                y = matrix(rnorm(n = 200), ncol = 2), 
                                sample_rate = 250))

  # too many columns
  expect_error(extract_saccades(x = matrix(rnorm(n = 400), ncol = 4),
                                y = matrix(rnorm(n = 400), ncol = 4), 
                                sample_rate = 250))
  
})


test_that("extract_saccades does not work for mismatching x, y and trial", {
  # monocular x and y: sizes do not match with trial
  expect_error(extract_saccades(x = rnorm(n = 100),
                                y = rnorm(n = 100),
                                sample_rate = 250,
                                trial = rep(1, 50)))
  
  # binocular x and y: sizes do not match
  expect_error(extract_saccades(x = matrix(rnorm(n = 400), ncol = 2),
                                y = matrix(rnorm(n = 400), ncol = 2),
                                sample_rate = 250,
                                trial = rep(1, 50)))
})


test_that("extract_saccades does not work for wrong method", {
  # string instead of a method handle
  expect_error(extract_saccades(x = rnorm(n = 100),
                                 y = rnorm(n = 100),
                                 sample_rate = 250,
                                 methods = "method_ek"))
  
  # empty list
  expect_error(extract_saccades(x = rnorm(n = 100),
                                 y = rnorm(n = 100),
                                 sample_rate = 250,
                                 methods = NULL))
  
})


test_that("extract_saccades does not work for wrong binocular option", {
  data("single_trial_binocular")
  
  # unknown option
  expect_error(extract_saccades(x = single_trial_binocular[, c("xL", "xR")],
                                y = single_trial_binocular[, c("yL", "yR")],
                                sample_rate = 1000,
                                binocular = "cyclopian"))
})


test_that("extract_saccades does not work for wrong sample_rate", {
  # wrong type
  expect_error(extract_saccades(x = rnorm(n = 100), y = rnorm(n = 100), sample_rate = "250"))
  
  # negative value
  expect_error(extract_saccades(x = rnorm(n = 100), y = rnorm(n = 100), sample_rate = -250))
  
  # multiple values
  expect_error(extract_saccades(x = rnorm(n = 100), y = rnorm(n = 100), sample_rate = c(250, 100)))
})

test_that("extract_saccades returns correct container", {
  data("single_trial")
  data("single_trial_binocular")
  
  # data.frame for a single trial
  expect_true(is.data.frame(extract_saccades(single_trial$x, single_trial$y, sample_rate = 250)))
  
  # matrix for votes on monocular data
  expect_true(is.matrix(extract_saccades(single_trial$x, single_trial$y, sample_rate = 250, return_votes = TRUE)))
  
  # list of matrices for votes on binocular data
  expect_true(is.list(extract_saccades(x = single_trial_binocular[, c("xL", "xR")],
                                       y = single_trial_binocular[, c("yL", "yR")],
                                       sample_rate = 1000,
                                       return_votes = TRUE)))

  # correct matrix for votes on monocular data
  methods_to_use <- list(method_ek, method_om, method_nh)
  monocular_votes <- extract_saccades(x = single_trial$x,
                                      y = single_trial$y,
                                      sample_rate = 250,
                                      methods = methods_to_use,
                                      return_votes = TRUE)
  expect_true(nrow(monocular_votes) == nrow(single_trial))
  expect_true(ncol(monocular_votes) == length(methods_to_use))
})


