# var_label(df %>% dplyr::select(!!var_dplyr))


#' Set Cowplot Theme
#'
#' Set the cowplot theme, a minimal theme for ggplot2
#'
#' @import ggplot2 cowplot
#'
#' @return set theme
#'
#' @examples
#' set_theme_cowplot()
#'
#' @export
set_theme_cowplot <- function(){theme_set(theme_cowplot())}


#' Histogram for Continuous Variables
#'
#' Histogram for exploratory data analysis of a continuous variable.
#'
#' @import ggplot2 dplyr
#' @importFrom rlang .data
#' @importFrom stats sd
#'
#' @param df Dataframe to be converted to table.
#' @param var Numeric for maximum number of digits to round numeric values.
#' @param n_binwidth Numeric indicating width of bins.
#' @param color_fill Default color for bars.
#'
#' @return Histogram.
#'
#' @examples
#' plot_hist_continuous(df = iris, var = Sepal.Length,  n_binwidth = 0.5, color_fill = "grey40")
#' plot_hist_continuous(df = USArrests, var = UrbanPop, n_binwidth = 0.5, color_fill = "red")
#'
#' @export
plot_hist_continuous <- function(df, var, n_binwidth, color_fill = "grey40"){
  var_dplyr <- enquo(var)

  # get stats for line and label
  data_stats <- df %>% summarise(
    var_mean = !!var_dplyr %>% mean(na.rm = T),
    var_sd   = !!var_dplyr %>% sd(na.rm = T)
  )

  # label to paste
  stats_label <- paste0(
    trim_n(abs(data_stats$var_mean), 1),
    " (", trim_n(data_stats$var_sd, 1), ")"
  )

  # plot
  df %>%
    ggplot() +
    geom_histogram(aes(x = !!var_dplyr), binwidth = n_binwidth, fill = color_fill) +
    # https://www.r-bloggers.com/no-visible-binding-for-global-variable/
    geom_vline(data = data_stats, aes(xintercept = .data$var_mean), size = 0.5, linetype = 2) +
    geom_label(data = data_stats, aes(x = .data$var_mean, label = stats_label, y = 0), size = 3) +
    labs(caption = "Mean (Std. Dev.)")

}


#' Histogram for Categorical Variables
#'
#' Histogram for exploratory data analysis of a categorical variable.
#'
#' @import ggplot2 dplyr
#' @importFrom forcats fct_rev
#' @importFrom forcats fct_explicit_na
#' @importFrom rlang .data
#' @importFrom scales percent
#'
#' @param df Dataframe to be converted to table.
#' @param var Numeric for maximum number of digits to round numeric values.
#' @param color_fill Default color for bars.
#'
#' @return Histogram.
#'
#' @examples
#' plot_hist_categorical(df = iris,   var = Species)
#' plot_hist_categorical(df = infert, var = education, color_fill = "blue")
#'
#' @export
plot_hist_categorical <- function(df, var, color_fill = "grey40"){
  var_dplyr <- enquo(var)

  # https://github.com/tidyverse/forcats/issues/122
  df_internal <- df %>%
    # rename NA to Missing
    mutate_if(is.factor, fct_explicit_na, na_level = "Missing") %>%
    # order factors so Missing is at bottom
    mutate_if(is.factor, fct_rev)

  # function that keeps ordering after relabeling
  str_wrap_factor <- function(x, ...) {
    levels(x) <- levels(x) %>% stringr::str_wrap(...)
    x
  }

  # get stats and label
  data_stats <- df_internal %>% group_by(!!var_dplyr) %>% summarise(
    var_count = n(),
    var_perc  = .data$var_count / nrow(df)
  )

  # label to paste
  stats_label <- paste0(
    data_stats$var_count,
    " (", data_stats$var_perc %>% scales::percent(accuracy = 1), ")"
  )

  # plot
  df_internal %>%
    ggplot(aes(x = !!var_dplyr %>% str_wrap_factor(12))) +
    geom_bar(fill = color_fill) +
    geom_label(data = data_stats, aes(y = .data$var_count/2, label = stats_label), size = 3) +
    coord_flip()
}
