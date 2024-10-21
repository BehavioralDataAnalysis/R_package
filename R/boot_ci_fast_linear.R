#' Creates a Bootstrap confidence interval for the output of a linear regression,
#' directly solving matrix cross-product
#'
#' @param df data to use
#' @param formula linear regression formula to run on the data
#' @param B number of Bootstrap sampling iterations to run
#' @param conf.level confidence interval percentage to use
#' @param cores the number of cores to use for parallel processing
#' @return a matrix where each row contains the lower and upper bounds of the confidence interval for the corresponding regression coefficient
#' @import doParallel foreach dplyr parallel stringr
#' @export

boot_ci_fast_linear <- function(df, formula, B = 100, conf.level = 0.90, cores = 2){

  #Validating the inputs, without repeating conditions already checked by wrapper function
  if(!is.character(formula)) stop("boot_ci_fast_linear was not passed a regression formula")
  offset = round(B * (1 - conf.level) / 2)

  # Extracting the variable names
  y_name <- formula |>
    stringr::str_extract("^[^~]+") |>
    stringr::str_trim()
  if(!(y_name %in% colnames(df))) stop("the dependent variable in the formula doesn't appear in the data")

  X_names <- formula |>
    stringr::str_extract_all("(?<=[:symbol:])([:alnum:]|_|\\.)+") |> unlist()

  varnames <- c(y_name, X_names)

  # Filtering data down to only the variables used
  df <- df |> dplyr::select(all_of(varnames))

  # Controlling for NA values in the variables that will be used
  if(any(is.na(df))) stop("the data provided contains NA values, please remove them first")

  # Encoding the data according to the model matrix
  encoded_data <- stats::model.matrix(~., data = df)
  y <- encoded_data[,y_name]
  X <- encoded_data[, -which(colnames(encoded_data) == y_name)]
  Nrow <- nrow(df)

  ## Using sapply for small data size
  if(Nrow <= 5e5){
    boot_mat <- sapply(1:B, function(x){

      # Bootstrapping the data
      indices <- sample(Nrow, replace = TRUE)
      y <- y[indices]
      X <- X[indices, , drop = FALSE]
      mod_cross <- as.vector(solve( crossprod(X), crossprod(X, y) ))
      return(mod_cross)
      })

    ordered_boot_mat <- apply(boot_mat, 1, sort, decreasing = FALSE)
    lower_bound <- ordered_boot_mat[offset,]
    upper_bound <- ordered_boot_mat[B+1-offset,]
    CI <- cbind(lower_bound, upper_bound)
    if(is.null(rownames(CI))){
      rownames(CI) <- c("(Intercept)", X_names)
    }

    ## TODO: calculate and return Achieved Significance Level here

  }
  ## Running the Bootstrap loop for large samples, with foreach, and doParallel as backend
  else {
    # Validating dependencies
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      stop("Package \"doParallel\" must be installed to use this function.", call. = FALSE)}

    # Function to bootstrap the data and run the regression
    boot_reg_fun <- function(X, y){
      indices <- sample(Nrow, replace = TRUE)
      # Creating copies to avoid side effects
      inner_y <- y[indices]
      inner_X <- X[indices, , drop = FALSE]
      mod_cross <- as.vector(solve( crossprod(X), crossprod(X, y) ))
      return(mod_cross)
      #mod_fast <- Rfast::lmfit(inner_X, inner_y)
      #return(mod_fast$be)
    }
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
                                   boot_reg_fun(X, y)
                                 }
    doParallel::stopImplicitCluster()

    ordered_boot_mat <- apply(boot_mat, 1, sort, decreasing = FALSE)
    lower_bound <- ordered_boot_mat[offset,]
    upper_bound <- ordered_boot_mat[B+1-offset,]
    CI <- cbind(lower_bound, upper_bound)
    if(is.null(rownames(CI))){
      rownames(CI) <- cbind("(Intercept)", X_names)
    }

    ## TODO: calculate and return Achieved Significance Level here
  }

  return(CI)
}
