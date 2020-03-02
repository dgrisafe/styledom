#' Quick Kable
#'
#' Create table with default settings for kableExtra::kable() function.
#' Produces table in R Markdown.
#'
#' @param df Dataframe to be converted to table.
#' @param p_digits Numeric for maximum number of digits to round numeric values.
#' @param p_format Character string. Possible values are latex, html, markdown, pandoc, and rst; this will be automatically determined if the function is called within knitr; it can also be set in the global option knitr.table.format. If format is a function, it must return a character string.
#' @param lab_title Character for table title.
#' @param col_names Character for column names. Must have the same length as the number of columns in the dataframe.
#'
#' @return The table ready for output in R Markdown.
#'
#' @examples
#' quick_kable(iris)
#' quick_kable(
#'     iris,
#'     lab_title = "Cool HTML Table of Iris Data",
#'     col_names = c("S Length", "S Width", "P Length", "P Width", "Species")
#'     )
#'
#' @export
quick_kable <- function(df, p_digits = 3, p_format = "html", lab_title = "", col_names = NULL){

  # if no column names are specified, give original column names
  if(is.null(col_names)){save_col_names <- colnames(df)}
  # if column names are specified, use those instead
  else{save_col_names <- col_names}

  df %>%
    knitr::kable(digits = p_digits,
          format = p_format,
          caption = lab_title,
          col.names = save_col_names,
          escape = F
          ) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))

}


#' Colorful P-Values
#'
#' Color the p-values in html so they change colors conditional on values.
#'    Red indicates p-values less than 0.05.
#' Orange indicates p-values less than 0.15.
#'
#' @import kableExtra
#'
#' @param var_p_value Numeric variable of p-values to be colored conditionally.
#'
#' @return New character string variable with html code for color of p-value.
#'
#' @examples
#' p_val <- c(0.00, 0.03, 0.05, 0.09, 0.11, 0.12, 0.16, 0.16, 0.21, 0.22)
#' p_val_color <- color_p_value(p_val)
#' quick_kable(data.frame(p_val, p_val_color))
#'
#' @export
color_p_value <- function(var_p_value){
  kableExtra::cell_spec(
    sprintf("%.3f", round(var_p_value, 3)),
    "html",
    color = ifelse(var_p_value < 0.05, "red", ifelse(var_p_value < 0.15, "orange", "black"))
  )
}
