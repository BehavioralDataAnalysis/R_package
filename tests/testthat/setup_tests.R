setup_test <- function(name){
  if(name == 'starwars'){
    data(starwars, package = 'dplyr')
    starwars_df <- starwars
    starwars_df <- starwars_df %>%
      na.omit() %>%
      select(-films, -vehicles, -starships)
    return(starwars_df)
  }
}


