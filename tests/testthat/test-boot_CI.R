test_that("boot_CI returns an error when the number of Bootstrap simulations is too small in relation to the confidence level", {
  expect_error(boot_CI(df_num, function(df) mean(df$x), B = 2), "the number of Bootstrap simulations is too small in relation to the confidence level")
})

test_that("boot_CI returns the right value when applied to the starwars dataset", {
  data(starwars,  package = 'dplyr')
  set.seed(1)
  CI <- boot_CI(starwars,
                function(df) mean(df$height, na.rm = TRUE),
                conf.level = 0.1)
  expect_lt(abs(CI[1] - 173.51), 0.1)
  expect_lt(abs(CI[2] - 175.18), 0.1)
})
