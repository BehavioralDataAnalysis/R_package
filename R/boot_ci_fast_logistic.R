#' Creates a Bootstrap confidence interval for the output of a logistic regression,
#' using fastLogisticRegressionWrap
#'
#' @param df data to use
#' @param formula logistic regression formula to run on the data
#' @param B number of Bootstrap sampling iterations to run
#' @param conf.level confidence interval percentage to use
#' @param cores the number of cores to use for parallel processing
#' @return a matrix where each row contains the lower and upper bounds of the confidence interval for the corresponding regression coefficient
#' @import doParallel foreach dplyr parallel stringr fastLogisticRegressionWrap
#' @export

boot_ci_fast_logistic <- function(df, formula, B = 100, conf.level = 0.90, cores = 2){

  formula <- "gender~mass+height"
  B <- 100
  conf.level = 0.10


  Nrow <- nrow(df)
  offset = round(B * (1 - conf.level) / 2)

  # Extracting the variable names
  y_name <- formula |>
    stringr::str_extract("^[^~]+") |>
    stringr::str_trim()
  if(!(y_name %in% colnames(df))) stop("the dependent variable in the formula doesn't appear in the data")

  X_names <- formula |>
    stringr::str_extract_all("(?<=[:symbol:]|[:blank:])([:alnum:]|_|\\.)+") |> unlist()

  ## TODO: handle cases with "y~." formula

  varnames <- c(y_name, X_names)

  # Filtering data down to only the variables used
  df <- df |> dplyr::select(all_of(varnames))

  # Controlling for NA values in the variables that will be used
  if(any(is.na(df))) stop("the data provided contains NA values, please remove them first")

  # Converting y variable to account for its binary status and representation
  df <- df |>
    mutate(across(all_of(y_name), ~ case_when(
      . %in% c("TRUE", "Y", "1", 1) ~ 1,
      . %in% c("FALSE", "N", "0", 0) ~ 0,
      TRUE ~ NA_real_
    )))
  #y_name <- paste(y_name, "1", sep = "")

  # Encoding the data according to the model matrix
  encoded_data <- stats::model.matrix(~., data = df)
  y <- encoded_data[,y_name]
  X <- encoded_data[, -which(colnames(encoded_data) == y_name)]

  ## Using sapply() for small data size

  if(Nrow <= 5e5){
    boot_mat <- sapply(1:B, function(x){

      # Bootstrapping the data
      indices <- sample(Nrow, replace = TRUE)
      y <- y[indices]
      X <- X[indices, , drop = FALSE]

      # Applying the logistic regression
      fast_model <- fast_logistic_regression(X, y)
      coeffs <- sapply(fast_model$coefficients, function(n) signif(n, 3))

      return(coeffs)
    })

    ordered_boot_mat <- apply(boot_mat, 1, sort, decreasing = FALSE)
    lower_bound <- ordered_boot_mat[offset,]
    upper_bound <- ordered_boot_mat[B+1-offset,]
    CI <- cbind(lower_bound, upper_bound)
    if(is.null(rownames(CI))){
      rownames(CI) <- c("(Intercept)", X_names)
    }

    ## TODO: calculate and return Achieved Significance Level here

  } else {
    ## TODO: add parallelized version here
  }
  return(CI)

}


