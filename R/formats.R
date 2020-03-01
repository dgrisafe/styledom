# function for formatting numbers
trimn <- function(x, n = 1){sprintf(paste0("%.", n, "f"), round(as.double(x), n))}


# function to standardize variables
std_var <- function(x){(x - mean(x, na.rm = T)) / sd(x, na.rm = T)}


# function that creates pretty estimates with lower and upper confidence limits
pretty_estimate <- function(estimate, limit_low, limit_high, n_round = 3){

  df_estimate <- c(estimate, limit_low, limit_high)

  # trim variables to n decimals
  pretty_estimate <- map_chr(.x = df_estimate, .f = trimn, n_round)

  # return pretty estimate
  paste0(pretty_estimate[[1]], " (", pretty_estimate[[2]], ", ", pretty_estimate[[3]], ")")

}
