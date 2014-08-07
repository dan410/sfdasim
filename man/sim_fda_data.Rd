\name{sim_fda_data}
\alias{sim_fda_data}
\title{generates data from a family of curves}
\usage{
sim_fda_data(coef, basis.fns, sigma0, m = NULL, pts = NULL, DIST = runif,
  ...)
}
\arguments{
  \item{coef}{matrix or data frame containing the
  coefficients of the basis functions. The number of
  columns is equal to the number of basis functions}

  \item{basis.fns}{basis functions given by the output of
  create_basis}

  \item{sigma0}{standard deviation of the noise term.}

  \item{m}{number of observations per curve}

  \item{pts}{vector of values where the curves will be
  evaluated}

  \item{DIST}{distribution function used to generate
  observatin locations for each curve. Default is runif().}

  \item{...}{additional parameter passed to dist function}
}
\value{
ID integer identifying a single curve

Time argument values where curves are evaluated

X response variable
}
\description{
generates a functional data set by evaluating curves and
adding Gaussian noise.
}

