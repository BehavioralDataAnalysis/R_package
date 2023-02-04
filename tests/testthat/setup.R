set.seed(1)
df_large_num <- data.frame(x = runif(1e6, 0, 100)) %>%
  mutate(y = x * rnorm(1e6, 1, 0.1) + rnorm(1e6, 0, 1))
