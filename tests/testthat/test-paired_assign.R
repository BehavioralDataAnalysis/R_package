test_that("paired assignment works on numeric data", {
  strat_df <- paired_assign(df_num, id = 'id')
  expect_equal(sum(strat_df[1:2, 'grp']), 1)
  expect_equal(sum(strat_df[3:4, 'grp']), 1)
  expect_equal(sum(strat_df[5:6, 'grp']), 1)
})

test_that("paired assignment works when the number of rows isn't divisible by the number of groups", {
  df1 <- data.frame(id = 1:7,
                    x = c(1, 1.5, 5, 5.5, 10, 10.5, 20),
                    y = c(1, 1.5, 5, 5.5, 10, 10.5, 20))
  strat_df <- paired_assign(df1, id = 'id')
  expect_equal(sum(strat_df[1:2, 'grp']), 1)
  expect_equal(sum(strat_df[3:4, 'grp']), 1)
  expect_equal(sum(strat_df[5:6, 'grp']), 1)
})

test_that("paired assignment works with character data", {
  strat_df <- paired_assign(df_char, id = 'id')
  expect_equal(sum(strat_df[1:2, 'grp']), 1)
  expect_equal(sum(strat_df[3:4, 'grp']), 1)
  expect_equal(sum(strat_df[5:6, 'grp']), 1)
})

test_that("paired assignment works with character id", {
  df <- data.frame(id = c('a', 'b', 'c', 'd', 'e', 'f'),
                    x = c(1, 1.5, 5, 5.5, 10, 10.5),
                    y = c(1, 1.5, 5, 5.5, 10, 10.5))
  strat_df <- paired_assign(df, id = 'id')
  expect_equal(sum(strat_df[1:2, 'grp']), 1)
  expect_equal(sum(strat_df[3:4, 'grp']), 1)
  expect_equal(sum(strat_df[5:6, 'grp']), 1)
})

### Tests with character/factor id column

test_that("paired assignment works with character id", {
  df <- data.frame(id = c('a', 'b', 'c', 'd', 'e', 'f'),
                    x = c(1, 1.5, 5, 5.5, 10, 10.5),
                    y = c(1, 1.5, 5, 5.5, 10, 10.5))
  strat_df <- paired_assign(df, id = 'id')
  expect_equal(sum(strat_df[1:2, 'grp']), 1)
  expect_equal(sum(strat_df[3:4, 'grp']), 1)
  expect_equal(sum(strat_df[5:6, 'grp']), 1)
})

test_that("paired assignment works with starwars data and 2 groups", {
  N <- 2
  strat_df <- paired_assign(starwars_df, id = 'name')
  expect_equal(n_distinct(strat_df$grp), N)
  expect_equal(strat_df %>% filter(grp == 0) %>% nrow(), nrow(starwars_df) %/% N)
})

test_that("paired assignment works with starwars data and 3 groups", {
  N <- 3
  strat_df <- paired_assign(starwars_df, id = 'name', n.groups = N)
  expect_equal(n_distinct(strat_df$grp), N)
  expect_equal(strat_df %>% filter(grp == 0) %>% nrow(), nrow(starwars_df) %/% N)
})
