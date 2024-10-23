test_that("boot_ci_fast_logistic returns the right value when passed a logistic regression formula
          to apply to the starwars dataset", {
  set.seed(1)
  df <- starwars |> select(gender, mass, height) |> mutate(gender = factor(gender,
                                                                           levels = c("masculine", "feminine"),
                                                                           labels = c(1, 0)))
  df <- df[complete.cases(df), ]
  CI <- boot_ci_fast_logistic(df,
                "gender~mass+height",
                conf.level = 0.1)
  expect_lt(abs(CI['mass','lower_bound'] - 0.145), 0.04)
  expect_lt(abs(CI['mass','upper_bound'] - 0.145), 0.04)
})
