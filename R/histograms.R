

# function that makes histograms of continuous variables
plot_hist_continuous <- function(df, var, n_binwidth, color_fill = "grey40"){
  var_dplyr <- enquo(var)

  # get stats for line and label
  data_stats <- df %>% summarise(
    var_mean = !!var_dplyr %>% mean(na.rm = T),
    var_sd  = !!var_dplyr %>% sd(na.rm = T)
  )

  # label to paste
  stats_label <- paste0(
    data_stats[["var_mean"]] %>% abs() %>% trimn(1),
    " (", data_stats[["var_sd"]]%>% trimn(1), ")"
  )

  # plot
  df %>%
    ggplot(aes(x = !!var_dplyr)) +
    geom_histogram(binwidth = n_binwidth, fill = color_fill) +
    geom_vline(data = data_stats, aes(xintercept = var_mean), size = 0.5, linetype = 2) +
    geom_label(data = data_stats, aes(x = var_mean, label = stats_label, y = 0), size = 3) +
    xlab(var_label(df %>% dplyr::select(!!var_dplyr))) +
    labs(caption = "Mean (Std. Dev.)")
}


# function that makes histograpms of categorical variables
plot_hist_categorical <- function(df, var){
  var_dplyr <- enquo(var)

  # https://github.com/tidyverse/forcats/issues/122
  df_internal <- df %>%
    # rename NA to Missing
    mutate_if(is.factor, fct_explicit_na, na_level = "Missing") %>%
    # order factors so Missing is at bottom
    mutate_if(is.factor, fct_rev)

  # function that keeps ordering after relabeling
  str_wrap_factor <- function(x, ...) {
    levels(x) <- levels(x) %>% str_wrap(...)
    x
  }

  # get stats and label
  data_stats <- df_internal %>% group_by(!!var_dplyr) %>% summarise(
    var_count = n(),
    var_perc  = var_count / nrow(df)
  )

  # label to paste
  stats_label <- paste0(
    data_stats[["var_count"]],
    " (", data_stats[["var_perc"]] %>% scales::percent(accuracy = 1), ")"
  )

  # plot
  df_internal %>%
    ggplot(aes(x = !!var_dplyr %>% str_wrap_factor(12))) +
    geom_bar() +
    geom_label(data = data_stats, aes(y = var_count/2, label = stats_label), size = 3) +
    xlab(var_label(df %>% dplyr::select(!!var_dplyr))) +
    coord_flip()
}