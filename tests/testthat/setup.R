set.seed(1)
df_large_num <- data.frame(x = stats::runif(1e6, 0, 100)) %>%
  mutate(y = x * stats::rnorm(1e6, 1, 0.1) + stats::rnorm(1e6, 0, 1))

utils::data(starwars, package = 'dplyr')
starwars_df <- starwars
starwars_df <- starwars_df %>%
  stats::na.omit() %>%
  dplyr::select(-films, -vehicles, -starships)
