##### Error handling for initial data validation #####

test_that("boot_ci returns an error when passed data that is not formatted as a data.frame",
          {
            values <- rnorm(100)
            expect_error(boot_ci(values, mean), "please provide data in  a data.frame or similar format")
          })

test_that("boot_ci returns an error when passed an empty data frame",
          {
            values <- data.frame()
            expect_error(boot_ci(values, mean), "the data provided is empty")
          })

test_that("boot_ci returns an error when provided a negative number of Bootstrap simulations",
          {
            values <- data.frame(x = c(1,2,3))
            expect_error(boot_ci(values, mean, B = -10),
                         "the value provided for the number of Bootstrap simulations is negative")
          })

test_that("boot_ci returns an error when provided a negative confidence level",
          {
            values <- data.frame(x = c(1,2,3))
            expect_error(boot_ci(values, mean, conf.level = -0.1),
                         "the value provided for the confidence level is negative")
          })

test_that("boot_ci returns an error when provided a confidence level above 1",
          {
            values <- data.frame(x = c(1,2,3))
            expect_error(boot_ci(values, mean, conf.level = 2),
                         "the value provided for the confidence level is above 1")
          })

test_that("boot_ci returns an error when the number of Bootstrap simulations is too small in relation to the confidence level", {
  expect_error(boot_ci(df_num, function(df) mean(df$x), B = 2),
               "the number of Bootstrap simulations is too small in relation to the confidence level")
})

test_that("boot_ci returns an error when given a function that doesn't apply to the data", {
  expect_error(boot_ci(starwars, function(df) mean(df$fame)),
               "the function returned NA")
})

test_that("boot_ci returns an error when given a function that returns a null value", {
  expect_error(boot_ci(starwars, function(df) if(FALSE){return(1)}))
})

test_that("boot_ci returns an error when given a function that doesn't return a single value", {
  expect_error(boot_ci(starwars, function(df) return(c(1, 2))),
               "the function returned an output of length different from 1")
})

test_that("boot_ci returns an error when given a regression formula that doesn't apply to the data", {
  data(starwars,  package = 'dplyr')
  expect_error(boot_ci(starwars, "fame~beauty"))
})

##### Function argument #####

test_that("boot_ci returns an error if the function applied sometimes return NA", {
  set.seed(1)
  faulty_mean <- function(df){
    val <- ifelse(stats::runif(1) < 0.1, NA, mean(df$height))
    return(val)
  }
  expect_error(boot_ci(starwars, faulty_mean),
               "the function returned NA")
})

test_that("boot_ci returns the right value when passed the mean function to apply to the starwars dataset", {
  set.seed(1)
  CI <- boot_ci(starwars,
                function(df) mean(df$height, na.rm = TRUE),
                conf.level = 0.1)
  expect_lt(abs(CI[1] - 174), 2)
  expect_lt(abs(CI[2] - 174), 2)
})

test_that("boot_ci returns the right value when passed the mean function to apply to a large dataset", {
  skip_on_ci()
  CI <- boot_ci(df_large_num,
                function(df) mean(df$x),
                B = 20,
                conf.level = 0.2,
                cores = 2)
  expect_lt(abs(CI[1] - 50), 0.1)
  expect_lt(abs(CI[2] -50), 0.1)
})

test_that("boot_ci returns an error if the second argument is not a function", {
  expect_error(boot_ci(starwars, 2L),
               "the second argument of the function must be a function or a regression formula")
})

##### Linear regression formula #####

test_that("boot_ci returns the right value when passed a linear regression formula to apply to the starwars dataset", {
  set.seed(1)
  df <- starwars |> select(mass, height)
  df <- df[complete.cases(df), ]
  CI <- boot_ci(df,
                "mass~height",
                conf.level = 0.1)
  expect_lt(abs(CI['height','lower_bound'] - 0.64), 0.1)
  expect_lt(abs(CI['height','upper_bound'] - 0.64), 0.1)
})

test_that("boot_ci returns the right value when passed a linear regression formula to apply to a large dataset", {
  set.seed(1)
  CI <- boot_ci(df_large_num,
                "y~x",
                B = 20,
                conf.level = 0.2,
                cores = 2)
  expect_lt(abs(CI['x', 'lower_bound'] - 1), 0.1)
  expect_lt(abs(CI['x', 'upper_bound'] -1), 0.1)
})



