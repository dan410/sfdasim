\name{plot_data}
\alias{plot_data}
\title{Plots the observed data from a functional data set}
\usage{
plot_data(dat)
}
\arguments{
  \item{dat}{Data frame containing observed functional
  data. Must have columns "ID", "Time", and "X" (see
  details)}
}
\description{
For observed functional data (arranged in a data frame with
a column named ID which contains integer values indexing
the curves and a column named Time containing values on the
x-axis and a column named X for the observed response) this
function will produce a plot of the observed data. By
default the points for each curve are connected by lines.
}

