\name{sim_sfda_data}
\alias{sim_sfda_data}
\title{generates data from a family of curves}
\usage{
sim_sfda_data(locs, coef, basis.fns, sigma0, m = NULL, pts = NULL,
  DIST = runif, ...)
}
\arguments{
  \item{locs}{N by 2 matrix of spatial locations}

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

locs spatial location of each curve

Time argument values where curves are evaluated

X response variable
}
\description{
generates a functional data set by evaluating curves and
adding Gaussian noise.
}
\examples{
curves <- sim_sfda_curves(nBasis = 2, cov.model = c("gaussian", "exponential"), cov.pars = rbind(c(1, 0.5), c(1, .3)), type="Cos", basis.pars = 2, locs = expand.grid(1:5/5, 1:10/10))
plot_curves(curves$coef, curves$basis.fns, ylim=c(-3,3))

sim.data <- sim_sfda_data(curves$locs, curves$coef, curves$basis.fns, sigma0=0.4, m = 10)
## plot_data(sim.data)
}

