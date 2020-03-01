# function that makes histograpms of variables
plot_scat_continuous <- function(var, df = data_afeds_airpol_label, var_outcome = std_vad, color_fill = "grey40", span_size = 0.6){
  var_dplyr <- enquo(var)
  var_outcome_dplyr <- enquo(var_outcome)

  # plot
  df %>%
    ggplot(aes(x = !!var_dplyr, y = !!var_outcome_dplyr)) +
    geom_point(color = color_fill) +
    geom_smooth(color = color_fill, method = "loess", span = span_size) +
    xlab(var_label(data_afeds_airpol_label %>% dplyr::select(!!var_dplyr))) +
    ylab(var_label(data_afeds_airpol_label %>% dplyr::select(!!var_outcome_dplyr))) +
    labs(caption = paste0("LOWESS (", span_size, " Window, 95% Conf. Int.)"))
}


# function that makes scatter plots of variables
plot_boxp_categorical <- function(var, df = data_afeds_airpol_label, var_outcome = std_vad){
  var_dplyr <- enquo(var)
  var_outcome_dplyr <- enquo(var_outcome)

  # get stats for line and label
  data_stats <- df %>%
    # rename NA to Missing
    mutate_if(is.factor, fct_explicit_na, na_level = "Missing") %>%
    # order factors so Missing is at bottom
    mutate_if(is.factor, fct_rev) %>%
    group_by(!!var_dplyr) %>% summarise(var_n = n())

  # get max value of outcome for label positioning
  max_outcome <- df %>% summarise(max_outcome = !!var_outcome_dplyr %>% max(na.rm = T)) %>% as.double()

  # label to paste
  stats_label <- data_stats[["var_n"]]

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

  # plot
  df_internal %>%
    ggplot(aes(x = !!var_dplyr %>% str_wrap_factor(12) %>% factor(), y = !!var_outcome_dplyr)) +
    geom_hline(yintercept = 0, linetype = 3) +
    geom_boxplot(alpha = 0.7) +
    geom_label(data = data_stats, aes(y = max_outcome, label = stats_label), color = "blue", size = 3) +
    xlab(var_label(df %>% dplyr::select(!!var_dplyr))) +
    ylab(var_label(df %>% dplyr::select(!!var_outcome_dplyr))) +
    labs(caption = "Count") +
    theme(plot.caption = element_text(color = "blue")) +
    coord_flip()

}