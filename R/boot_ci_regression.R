#' Creates a Bootstrap confidence interval for the output of a linear or logistic regression,
#' directly solving matrix cross-product or using fastLogisticRegressionWrap
#'
#' @param df data to use
#' @param formula regression formula to run on the data
#' @param B number of Bootstrap sampling iterations to run
#' @param conf.level confidence interval percentage to use
#' @param cores the number of cores to use for parallel processing
#' @return a matrix where each row contains the lower and upper bounds of the confidence interval for the corresponding regression coefficient
#' @import doParallel foreach dplyr parallel stringr fastLogisticRegressionWrap
#' @export

boot_ci_regression <- function(df, formula, B = 100, conf.level = 0.90, cores = 2){

  #Validating the inputs, without repeating conditions already checked by wrapper function
  if(!is.character(formula)) stop("boot_ci_regression was not passed a regression formula")
  offset = round(B * (1 - conf.level) / 2)

  # Extracting the dependent variable name
  y_name <- formula |>
    stringr::str_extract("^[^~]+") |>
    stringr::str_trim()
  if(!(y_name %in% colnames(df))) stop("the dependent variable in the formula doesn't appear in the data")

  # Extracting predictor variable names
  if(formula |> stringr::str_trim() |> stringr::str_ends("~\\.")){
    X_names <- colnames(df)
    X_names <- X_names[X_names != y_name] # Removing the dependent variable
  } else {
    X_names <- formula |>
      stringr::str_extract_all("(?<=[:symbol:]|[:blank:])([:alnum:]|_|\\.)+") |> unlist()
  }

  varnames <- c(y_name, X_names)

  # Filtering data down to only the variables used
  df <- df |> dplyr::select(all_of(varnames))

  # Controlling for NA values in the variables that will be used
  if(any(is.na(df))) stop("the data provided contains NA values, please remove them first")

  # Counting the number of distinct values of the dependent variable to determine the type of regression
  values_cnt <- df |>
    dplyr::select(all_of(y_name)) |>
    dplyr::n_distinct()
  if(values_cnt == 2) {regression_type <- "logistic"} else {regression_type <- "linear"}

 if(regression_type == "logistic"){
   # Converting y variable to account for its binary status and representation
   df <- df |>
     mutate(across(all_of(y_name), ~ case_when(
       . %in% c("TRUE", "Y", "1", 1) ~ 1,
       . %in% c("FALSE", "N", "0", 0) ~ 0,
       TRUE ~ NA_real_
     )))

   ## Creating the inner function to Bootstrap
   boot_fun <- function(X, y){
     # Bootstrapping the data
     indices <- sample(Nrow, replace = TRUE)
     y <- y[indices]
     X <- X[indices, , drop = FALSE]

     # Applying the logistic regression
     fast_model <- fastLogisticRegressionWrap::fast_logistic_regression(X, y)
     coeffs <- sapply(fast_model$coefficients, function(n) signif(n, 3))
     return(coeffs)
   }
 } else if(regression_type == "linear"){

   ## Creating the inner function to Bootstrap
   boot_fun <- function(X, y){
     # Bootstrapping the data
     indices <- sample(Nrow, replace = TRUE)
     y <- y[indices]
     X <- X[indices, , drop = FALSE]
     coeffs <- as.vector(solve( crossprod(X), crossprod(X, y) ))
     return(coeffs)
   }
 }

  # Encoding the data according to the model matrix
  encoded_data <- stats::model.matrix(~., data = df)
  y <- encoded_data[,y_name]
  X <- encoded_data[, -which(colnames(encoded_data) == y_name)]
  Nrow <- nrow(df)

  ## Branching the Bootstrap execution depending on the data size

  if(Nrow <= 5e5){ boot_mat <- replicate(B, boot_fun(X, y)) } # Using replicate() for small data size
  else {

    # Validating dependencies
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      stop("Package \"doParallel\" must be installed to use this function.", call. = FALSE)}

    N_coeffs = length(varnames)

    # Detecting the number of cores if not provided by the user
    if(missing(cores)){
      cores <- parallel::detectCores() - 1
    }
    # Initializing the cluster
    doParallel::registerDoParallel(cores = cores)
    boot_mat <- matrix(data = NA, nrow = N_coeffs, ncol = B)

    boot_mat <- foreach::foreach(i=1:B,
                                 .combine='cbind',
                                 .packages = c("dplyr"),
                                 .inorder = FALSE) %dopar% {
                                   boot_fun(X, y)
                                 }
    doParallel::stopImplicitCluster()
  }

  # Extracting the CI bounds from the Bootstrapped analyses
  ordered_boot_mat <- apply(boot_mat, 1, sort, decreasing = FALSE)
  lower_bound <- ordered_boot_mat[offset,]
  upper_bound <- ordered_boot_mat[B+1-offset,]
  CI <- cbind(lower_bound, upper_bound)
  if(is.null(rownames(CI))){
    rownames(CI) <- c("(Intercept)", X_names)
  }

  ## TODO: calculate and return Achieved Significance Level here

  return(CI)
}
