#' Creates a Bootstrap confidence interval for the output of a function
#'
#' @param df data to use
#' @param fct function or linear regression formula to run on the data
#' @param B number of Bootstrap sampling iterations to run
#' @param conf.level confidence interval percentage to use
#' @param cores the number of cores to use for parallel processing
#' @return if the input is a function: a vector with two values, the lower and upper bounds of the confidence interval; otherwise, if the input is a linear regression formula, a matrix where each row contains the lower and upper bounds of the confidence interval for the corresponding regression coefficient
#' @import doParallel foreach dplyr parallel
#' @examples
#' options(mc.cores = 2)
#' df1 <- data.frame(
#' id = 1:6,
#' x = c(1, 1.5, 5, 5.5, 10, 10.5),
#' y = c(1, 1.5, 5, 5.5, 10, 10.5))
#' boot_CI(df1, function(df) mean(df$x), cores = 2)
#' @export

boot_CI <- function(df, fct, B = 100, conf.level = 0.90, cores = 2){

  #Validating the inputs
  if(nrow(df) == 0) stop("the data provided is empty")
  if(B <= 0) stop("the value provided for the number of Bootstrap simulations is negative")
  if(conf.level <= 0) stop("the value provided for the confidence level is negative")
  if(conf.level > 1) stop("the value provided for the confidence level is above 1")

  # Calculating and validating the offset
  if(B * (1 - conf.level) / 2 < 1) stop("the number of Bootstrap simulations is too small in relation to the confidence level")
  offset = round(B * (1 - conf.level) / 2)


  ### First case: fct is a function
  if(is.function(fct)){

    # Validating that the function returns a non-null, single-valued output
    if(suppressWarnings(length(fct(df))!=1)) stop("the function returned an output of length different from 1")
    if(suppressWarnings(is.na(fct(df)))) stop("the function returned NA")


    ## Using sapply for small data size
    if(nrow(df) <= 5e5){
      boot_vec <- sapply(1:B, function(x){
        fct(slice_sample(df, n = nrow(df), replace = TRUE))})
    }

    ## Running the Bootstrap loop with foreach, and doParallel as backend
    else {
      # Validating dependencies
      if (!requireNamespace("doParallel", quietly = TRUE)) {
        stop("Package \"doParallel\" must be installed to use this function.", call. = FALSE)}

      # Detecting the number of cores if not provided by the user
      if(missing(cores)){
        cores <- parallel::detectCores() - 1
      }
      # Initializing the cluster
      doParallel::registerDoParallel(cores = cores)
      boot_vec <- rep(NA, B)
      inner_df <- df

      boot_vec <- foreach::foreach(i=1:B,
                                   .combine='c',
                                   .packages = "dplyr",
                                   .inorder = FALSE) %dopar% {
                                     sample_df <- dplyr::slice_sample(inner_df, n = nrow(inner_df), replace = TRUE)
                                     fct(sample_df)
                                   }
      doParallel::stopImplicitCluster()
    }

    ## Formatting the output
    if(any(is.na(boot_vec))) stop("the function returned an NA")
    boot_vec <- sort(boot_vec, decreasing = FALSE)
    lower_bound <- boot_vec[offset]
    upper_bound <- boot_vec[B+1-offset]
    CI <- c(lower_bound, upper_bound)
  }
  ### Second case: fct is a regression formula
  else if(is.character(fct)){

    ## Using sapply for small data size
    if(nrow(df) <= 5e5){
      boot_mat <- sapply(1:B, function(x){
        mod <- stats::lm(formula = fct,
                  data    = slice_sample(df, n = nrow(df), replace = TRUE))
        return(mod$coefficients)})
    }
    ## TODO: Running the Bootstrap loop with foreach, and doParallel as backend
    #else {}

    ordered_boot_mat <- apply(boot_mat, 1, sort, decreasing = FALSE)
    lower_bound <- ordered_boot_mat[offset,]
    upper_bound <- ordered_boot_mat[B+1-offset,]
    CI <- cbind(lower_bound, upper_bound)

  } else stop("the second argument of the function must be a function or a regression formula")

  return(CI)
}
