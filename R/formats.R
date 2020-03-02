#' Trim Number
#'
#' Format number to n decimal places
#'
#' @param x Numeric to be formatted.
#' @param n_dec Numeric for number of digits to format after the decimal.
#'
#' @return The formatted \code{x}.
#'
#' @examples
#' trim_n(pi, 3)
#' trim_n(1.2345, 1)
#'
#' @export
trim_n <- function(x, n_dec = 1){sprintf(paste0("%.", n_dec, "f"), round(as.double(x), n_dec))}



#' Pretty Estimate
#'
#' Create character string of an estimate with its confidence limits.
#'
#' @import purrr
#' @importFrom stats sd
#'
#' @param estimate Numeric estimate.
#' @param limit_low Numeric estimate lower confidence interval.
#' @param limit_high Numeric estimate higher confidence interval.
#' @param n_dec Numeric indicating number of digits to round after decimal.
#'
#' @return Character string of formatted estimate.
#'
#' @examples
#' iris_estimate   <- mean(iris$Petal.Length)
#'  iris_std_err   <- sd(iris$Petal.Length) / sqrt(length(iris$Petal.Length))
#' iris_limit_low  <- iris_estimate - 1.96 * iris_std_err
#' iris_limit_high <- iris_estimate + 1.96 * iris_std_err
#' pretty_estimate(iris_estimate, iris_limit_low, iris_limit_high, 2)
#'
#' @export
pretty_estimate <- function(estimate, limit_low, limit_high, n_dec = 3){

  df_estimate <- c(estimate, limit_low, limit_high)

  # trim variables to n decimals
  pretty_estimate <- map_chr(.x = df_estimate, .f = trim_n, n_dec)

  # return pretty estimate
  paste0(pretty_estimate[[1]], " (", pretty_estimate[[2]], ", ", pretty_estimate[[3]], ")")

}


#' Standardize Variable
#'
#' Standardize variable by centering on its mean and dividing by its standard deviation.
#' The new variable will now have a mean of 0 and a standard deviation of 1.
#' All missing data points are excluded from the mean and deviation calculations.
#'
#' @param x Numeric variable to be formatted.
#'
#' @return The standardized \code{x}.
#'
#' @examples
#' std_var(iris$Petal.Length)
#' std_var(cars$speed)
#'
#' @export
std_var <- function(x){(x - mean(x, na.rm = T)) / sd(x, na.rm = T)}

