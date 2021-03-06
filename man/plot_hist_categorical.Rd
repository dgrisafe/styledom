% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/histograms.R
\name{plot_hist_categorical}
\alias{plot_hist_categorical}
\title{Histogram for Categorical Variables}
\usage{
plot_hist_categorical(df, var, flip_plot = TRUE, color_fill = "grey40")
}
\arguments{
\item{df}{Dataframe to be converted to table.}

\item{var}{Numeric for maximum number of digits to round numeric values.}

\item{flip_plot}{Flip axis of categorical plot by default.}

\item{color_fill}{Default color for bars.}
}
\value{
Histogram.
}
\description{
Histogram for exploratory data analysis of a categorical variable.
}
\examples{
plot_hist_categorical(df = iris,   var = Species)
plot_hist_categorical(df = infert, var = education, flip_plot = FALSE, color_fill = "blue")

}
