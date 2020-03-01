# function for a quick and dirty html table (kable())
quick_kable <- function(df, p_digits = 3, p_format = "html", lab_title = "", col_names = NULL){

  # if no column names are specified, give original column names
  if(is.null(col_names)){save_col_names <- colnames(df)}
  # if column names are specified, use those instead
  else{save_col_names <- col_names}

  df %>%
    kable(digits = p_digits, format = p_format, caption = lab_title, col.names = save_col_names, escape = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))

}

# function for formatting p-values in kable
form_kable_p_value <- function(var_p_value){cell_spec(sprintf("%.3f", round(var_p_value, 3)), "html", color = ifelse(var_p_value < 0.05, "red", ifelse(var_p_value < 0.15, "orange", "black")))}
