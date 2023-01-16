#' Create a stratified assignment (aka. pair matching)
#'
#' @param df data to use
#' @param id column(s) used for identification
#' @param n.groups number of groups
#' @return the original data frame, with an added column for group assignment
#' @examples
#' df1 <- data.frame(
#' id = 1:6,
#' x = c(1, 1.5, 5, 5.5, 10, 10.5),
#' y = c(1, 1.5, 5, 5.5, 10, 10.5))
#' strat_assign(df1, 'id')
#' @export

strat_assign <- function(df, id, n.groups = 2){

  # Generating the matched pairs
  pairs_mat <- pairing(df, id = id, n.groups = n.groups)

  # Assigning experimental groups to the matched pairs
  N_pairs <- nrow(pairs_mat)
  assgnmt <- matrix(0, nrow = N_pairs, ncol = n.groups)
  assgnmt[,1] <- 1
  shuffled_assgnmt <- t(apply(assgnmt, 1, sample))

  pairs_df <- data.frame(
    id = c(pairs_mat),
    grp = c(shuffled_assgnmt)
  )

  # Replacing 'id' by the correct value
  colnames(pairs_df) <- c(eval(id), 'grp')

  df_out <- merge(df, pairs_df, by=id)

  return(df_out)
}
