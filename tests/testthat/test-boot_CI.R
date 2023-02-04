##### Error handling #####

test_that("boot_CI returns an error when passed data that is not formatted as a data.frame",
          {
            values <- rnorm(100)
            expect_error(boot_CI(values, mean), "please provide data in  a data.frame or similar format")
          })

test_that("boot_CI returns an error when the number of Bootstrap simulations is too small in relation to the confidence level", {
  expect_error(boot_CI(df_num, function(df) mean(df$x), B = 2),
               "the number of Bootstrap simulations is too small in relation to the confidence level")
})

test_that("boot_CI returns an error when given a function that doesn't apply to the data", {
  data(starwars,  package = 'dplyr')
  expect_error(boot_CI(starwars, function(df) mean(df$fame)),
               "the function returned NA")
})

test_that("boot_CI returns an error when given a function that returns a null value", {
  data(starwars,  package = 'dplyr')
  expect_error(boot_CI(starwars, function(df) if(FALSE){return(1)}))
})

test_that("boot_CI returns an error when given a function that doesn't return a single value", {
  data(starwars,  package = 'dplyr')
  expect_error(boot_CI(starwars, function(df) return(c(1, 2))),
               "the function returned an output of length different from 1")
})

test_that("boot_CI returns an error when given a regression formula that doesn't apply to the data", {
  data(starwars,  package = 'dplyr')
  expect_error(boot_CI(starwars, "fame~beauty"))
})

##### Function argument #####

test_that("boot_CI returns the right value when passed the mean function to apply to the starwars dataset", {
  data(starwars,  package = 'dplyr')
  set.seed(1)
  CI <- boot_CI(starwars,
                function(df) mean(df$height, na.rm = TRUE),
                conf.level = 0.1)
  expect_lt(abs(CI[1] - 173.51), 0.1)
  expect_lt(abs(CI[2] - 175.18), 0.1)
})

test_that("boot_CI returns the right value when passed the mean function to apply to a large dataset", {
  skip_on_ci()
  CI <- boot_CI(df_large_num,
                function(df) mean(df$x),
                B = 20,
                conf.level = 0.2,
                cores = 2)
  expect_lt(abs(CI[1] - 50), 0.1)
  expect_lt(abs(CI[2] -50), 0.1)
})

##### Linear regression formula #####

test_that("boot_CI returns the right value when passed a linear regression formula to apply to the starwars dataset", {
  data(starwars,  package = 'dplyr')
  set.seed(1)
  CI <- boot_CI(starwars,
                "mass~height",
                conf.level = 0.1)
  expect_lt(abs(CI['height','lower_bound'] - 0.6386), 0.03)
  expect_lt(abs(CI['height','upper_bound'] - 0.6386), 0.03)
})

test_that("boot_CI returns the right value when passed a linear regression formula to apply to a large dataset", {
  set.seed(1)
  CI <- boot_CI(df_large_num,
                "y~x",
                B = 20,
                conf.level = 0.2,
                cores = 2)
  expect_lt(abs(CI['x', 'lower_bound'] - 1), 0.1)
  expect_lt(abs(CI['x', 'upper_bound'] -1), 0.1)
})



