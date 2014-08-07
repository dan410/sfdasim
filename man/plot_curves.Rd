\name{plot_curves}
\alias{plot_curves}
\title{Plots curves from a functional dataset with specified basis functions and coefficients}
\usage{
plot_curves(coef, basis.fns, xlim = c(0, 1), ylim = c(-2, 2), xlab = "",
  ylab = "", ...)
}
\arguments{
  \item{coef}{vector of matrix of coefficients. Rows
  correspond to the coefficients of a single functional
  observation}

  \item{basis.fns}{list of basis functions created by
  \code{create_basis}}

  \item{xlim}{vector of length 2}

  \item{ylim}{vector of length 2}

  \item{xlab}{x label}

  \item{ylab}{y label}

  \item{...}{arguments passed to plot}
}
\description{
Given basis functions created by create_basis() and a
matrix of coefficients will plot the family of curves on a
single plot
}

