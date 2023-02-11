
##### Input validation #####

test_that("pair yields an error when data is empty", {
  expect_error(pair(data.frame(), 'id'), "the data provided is empty")
})

test_that("pair yields an error when data contains NA values", {
  expect_error(pair(df_na, 'id'), "please address NA values before using this function")
})

test_that("pair yields an error when the id passed doesn't match any column name", {
  expect_error(pair(df_num, 'not_an_id'),
               "the id string doesn't match any column name")
})

test_that("pair generates a warning when the data has no numeric variable", {
  df_all_char <- data.frame(
    id = 1:5,
    x = c('a', 'a', 'b', 'c', 'b'),
    y = c('red', 'blue', 'red', 'blue', 'blue')
  )
  expect_warning(pair(df_all_char, 'id'),
                 "The data has no numeric variables. Results may be unstable.")
})

### Tests with numerical id column

test_that("pair works on numeric data", {
  expect_identical(pair(df_num, 'id'), matrix(data = c(2L, 1L, 4L, 3L, 6L, 5L), nrow = 3, byrow = TRUE))
})

test_that("pair works when the number of rows isn't divisible by the number of groups", {
  df1 <- data.frame(id = 1:7,
                    x = c(1, 1.5, 5, 5.5, 10, 10.5, 200),
                    y = c(1, 1.5, 5, 5.5, 10, 10.5, 200))
  expect_identical(pair(df1, 'id'), matrix(data = c(2L, 1L, 4L, 3L, 6L, 5L), nrow = 3, byrow = TRUE))
})

test_that("pair works with character data", {
  expect_identical(pair(df_char, 'id'), matrix(data = c(2L, 1L, 4L, 3L, 6L, 5L), nrow = 3, byrow = TRUE))
})


test_that("pair works with categorical data", {
  df1 <- data.frame(id = 1:6,
                    x = c(1, 1.5, 5, 5.5, 10, 10.5),
                    y = c(1, 1.5, 5, 5.5, 10, 10.5),
                    z = as.factor(c('A', 'A', 'B', 'B', 'C', 'C')))
  expect_identical(pair(df1, 'id'), matrix(data = c(2L, 1L, 4L, 3L, 6L, 5L), nrow = 3, byrow = TRUE))
})

### Tests with character/factor id column

test_that("pair works with character id", {
  df1 <- data.frame(id = c('a', 'b', 'c', 'd', 'e', 'f'),
                    x = c(1, 1.5, 5, 5.5, 10, 10.5),
                    y = c(1, 1.5, 5, 5.5, 10, 10.5))
  expect_identical(pair(df1, 'id'), matrix(data = c('b', 'a', 'd', 'c', 'f', 'e'), nrow = 3, byrow = TRUE))
})

test_that("pair works with starwars data and 2 groups", {
  sw_pairs <- pair(starwars_df, id = 'name')
  expect_identical(sw_pairs[1,], c("Anakin Skywalker", "Luke Skywalker"))
})

test_that("pair works with starwars data and 3 groups", {
  N <- 3
  sw_pairs <- pair(starwars_df, id = 'name', n.groups = N)
  expect_equal(ncol(sw_pairs), N)
  expect_identical(sw_pairs[1,], c("Owen Lars", "Anakin Skywalker", "Luke Skywalker"))
})


##### arg_part_sort #####

test_that("argpartsort returns an error if the number of values to return is not an integer", {
  expect_error(argpartsort(c(1, 5, 2, 3, 4), 0.5), "the number of values to return is not an integer")
})

test_that("argpartsort returns an error if the number of values to return is not strictly positive", {
  expect_error(argpartsort(c(1, 5, 2, 3, 4), -1L), "the number of values to return is not strictly positive")
})

test_that("argpartsort works as expected when passed an integer number of values to return", {
  expect_equal(argpartsort(c(1, 5, 2, 3, 4), 1L), 1)
})

test_that("argpartsort works as expected when passed a numeric number of values to return that is equal to an integer", {
  expect_equal(argpartsort(c(1.2, 5.1, 2.1, 3.8, 4.2), 1), 1)
})

test_that("argpartsort works as expected with a matrix", {
  mat <- matrix(c(2.5, 0.3, 1.3, 4.5, 2.3, 0.8), ncol = 2)
  expect_equal(argpartsort(mat, 1), c(2,3))
})

##### min_max_norm #####

test_that("min_max_norm works as expected", {
  val <- c(1,2,1,2)
  expect_equal(min_max_norm(val), c(0,1,0,1))
})

