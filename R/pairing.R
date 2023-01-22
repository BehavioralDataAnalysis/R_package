#' Creates a list of pairs of rows close to each other
#'
#' @param df data to use
#' @param id column(s) used for identification
#' @param n.groups number of groups
#' @return a list of pairs, each having n.groups elements
#' @import dplyr
#' @importFrom methods is
#' @examples
#' df1 <- data.frame(
#' id = 1:6,
#' x = c(1, 1.5, 5, 5.5, 10, 10.5),
#' y = c(1, 1.5, 5, 5.5, 10, 10.5))
#' pairing(df1, 'id')
#' @export


pairing <- function(df, id, n.groups = 2){

  # Running checks and getting distance matrix from auxiliary function
  d_mat <- checks_and_preps_dmat(df = df, id = id)

  # Setting parameters for the matching
  N <- nrow(df)
  pairs_mat_lim <- floor(N/n.groups)
  nb_matches_needed <- n.groups - 1 # Number of matches we want to find for each row

  available <- 1:N
  pairs_mat <- matrix(integer(), nrow = pairs_mat_lim, ncol = n.groups)
  nb_pairs <- 0L

  # Apply argpartsort to each column of d_mat until enough matches have been found
  for(c in 1:N){ # Iterating through the columns
    if(!(c %in% available)) { next } # Going to next iteration of the loop if the subject c is not available anymore
    if(length(available) < n.groups) { break } # Exiting the loop if we have gone through enough of the data
    for(search_lim in nb_matches_needed:N){
      closest_candidates <- argpartsort(d_mat[,c], search_lim)
      matches <- intersect(available, closest_candidates)
      if(length(matches) == nb_matches_needed){
        pair <- append(matches, c) # Adding the subject c to its matches to form a pair
        nb_pairs <- nb_pairs + 1
        pairs_mat[nb_pairs,] <- pair
        available <- setdiff(available, pair)
        break
      } else if(length(matches) > nb_matches_needed){
        # Resolving ties: suboptimal implementation until required by c++ code
        warning("The ELSE IF branch in pairing() was reached. Time to write a better tie-breaking code")
        matches <- matches[1:nb_matches_needed]
      }
      # Otherwise, redo the loop for search_lim += 1
    }
  }

  # Map the indices to values in the original data if necessary
  id_vec <- df[[id]]
  if(!identical(id_vec, 1:N)){
    pairs_mat <- matrix(sapply(pairs_mat, function(x) id_vec[x]),
                        ncol = n.groups)
  }
  return(pairs_mat)
}

##### Auxiliary functions #####
checks_and_preps_dmat <- function(df, id){

  N <- nrow(df)

  # Early input validation
  if(N == 0 | ncol(df) == 0) stop("the data provided is empty")

  #Stopping if there are NA values
  if(nrow(df %>% stats::na.omit()) != N) stop("please address NA values before using this function")

  #Isolating the identification variable
  if(!(id %in% colnames(df))) stop("the id string doesn't match any column name")

  # Further input validation
  #MAYBE NEED TO REMOVE SAPPLY FOR ROBUSTNESS?
  if(!all(sapply(df, function(x) is.numeric(x)| is.integer(x)| is.factor(x)| is.character(x)))) stop("please format all data columns to numeric, integer, factor or character (which will be treated as factor)")

  # Extracting the matching variables for distance measurement
  matching_vars <- df[, !names(df) %in% id]

  #Handling numeric variables
  normalized_vars <- data.frame(nrow(matching_vars))
  if(any(sapply(matching_vars, class)=='numeric')){
    num_vars <- matching_vars %>%
      dplyr::select_if(function(x) is.numeric(x)|is.integer(x))

    #Normalizing numeric variables
    normalized_vars <- num_vars %>%
      dplyr::mutate(dplyr::across(.fns = scales::rescale))
  } else {warning("The data has no numeric variables. Results may be unstable.")}

  #Handling categorical variables
  if(any(sapply(matching_vars, class)=='factor'|sapply(matching_vars, class)=='character')){
    cat_vars <- matching_vars %>%
      dplyr::select_if(function(x) is.factor(x)|is.character(x)) %>%
      dplyr::mutate(dplyr::across(.fns = as.factor))

    #One-hot encoding categorical variables
    normalized_cat_vars <- as.data.frame(stats::model.matrix( ~.-1, data = cat_vars))
    normalized_vars <- normalized_vars %>% cbind(normalized_cat_vars)
  }

  #Calculating distance matrix
  d_mat <- normalized_vars %>%
    stats::dist(method = 'euclidean', diag = TRUE, upper = TRUE) %>%
    as.matrix()
  diag(d_mat) <- N + 1

  return(d_mat)
}




#' Returns the indices of the n-smallest values in a vector, or in each column of a matrix
#'
#' @param dat A vector or a matrix
#' @param n the number of indices to return
#'
#' @return a vector of integers (for a vector input) or a matrix (for a matrix input)
#' @export
#'
#' @examples
#' # input data is a vector
#' vec <- c(20, 10, 30, 60, 50, 40)
#' argpartsort(vec, 3)
#'
#' # input data is a matrix
#' mat <- matrix(data = c(20, 10, 30, 60, 50, 40, 40, 20, 10, 30, 60, 50), ncol = 2)
#' argpartsort(mat, 2)
argpartsort <- function(dat, n){


  # Input validation
  if(!is(n, 'numeric')) stop("the number of values to return is not an integer")
  if(n <= 0) stop("the number of values to return is not positive")

  if(is(dat, 'vector')){
    # Returns the indices of the n smallest values
    return(order(dat)[1:n])
  } else if(is(dat, 'matrix')) {
    # Returns the indices of the n smallest values for each column
    ordered_mat <- apply(dat, 2, function(x) order(x)[1:n])
    return(ordered_mat)
  }
}

# Normalizing all the numeric vectors in matching_vars
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
