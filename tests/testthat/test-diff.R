test_that("diff functions work", {
  data("single_trial")
  diff_methods <- c(diff_ek, diff_nh) 
  for(a_method in diff_methods){
    vel_df <- a_method(single_trial$x, single_trial$y,  rep(1, nrow(single_trial)), 250)
    expect_equal(nrow(vel_df), nrow(single_trial))
    expect_equal(ncol(vel_df), 3)
    expect_true(all(colnames(vel_df) == c("x", "y", "amp")))
  }
})
