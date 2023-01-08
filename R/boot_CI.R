#' Creates a Bootstrap confidence interval for the output of a function
#'
#' @param df data to use
#' @param fct function to run on the data
#' @param B number of Bootstrap sampling iterations to run
#' @param conf.level confidence interval percentage to use
#' @return a vector with two values, the lower and upper bounds of the confidence interval
#' @importFrom doParallel, dplyr, foreach
#' @examples
#' df1 <- data.frame(
#' id = 1:6,
#' x = c(1, 1.5, 5, 5.5, 10, 10.5),
#' y = c(1, 1.5, 5, 5.5, 10, 10.5))
#' boot_CI(df1, function(df) mean(df$x), B=100)
#' @export

boot_CI <- function(df, fct, B = 100, conf.level = 0.90){
  # Validating dependencies
  if (!requireNamespace("doParallel", quietly = TRUE)) {
    stop("Package \"doParallel\" must be installed to use this function.", call. = FALSE)}

  #Validating the inputs
  if(nrow(df) == 0) stop("the data provided is empty")
  if(B <= 0) stop("the value provided for the number of Bootstrap simulations is negative")
  if(conf.level <= 0) stop("the value provided for the confidence level is negative")
  if(conf.level > 1) stop("the value provided for the confidence level is above 1")

  # Validating that the function returns a single-valued output
  if(suppressWarnings(is.na(fct(df)|length(fct(df))>1))) stop("the function doesn't return a single-valued, non-null output.")

  #Calculating and validating the offset
  if(B * (1 - conf.level) / 2 < 1) stop("the number of Bootstrap simulations is too small in relation to the confidence level")
  offset = round(B * (1 - conf.level) / 2)

  # Running the Bootstrap loop with foreach, and doParallel as backend
  doParallel::registerDoParallel()
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

  if(any(is.na(boot_vec))) stop("the function returned an NA")
  boot_vec <- sort(boot_vec, decreasing = FALSE)

  lower_bound <- boot_vec[offset]
  upper_bound <- boot_vec[B+1-offset]
  CI <- c(lower_bound, upper_bound)
  return(CI)
}
