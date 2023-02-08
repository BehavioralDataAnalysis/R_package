
### Tests with numerical id column

test_that("pair yields an error when data is empty", {
  expect_error(pair(data.frame(), 'id'), "the data provided is empty")
})

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

test_that("pair yields an error when data contains NA values", {
  expect_error(pair(df_na, 'id'), "please address NA values before using this function")
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



test_that("pair works with starwars data", {
  starwars_df <- setup_test('starwars')
  sw_pairs <- pair(starwars_df, id = 'name')
  expect_identical(sw_pairs[1,], c("Anakin Skywalker", "Luke Skywalker"))
})
