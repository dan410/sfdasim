\name{sim_fda_curves}
\alias{sim_fda_curves}
\title{simulates a collection of independent curves.}
\usage{
sim_fda_curves(nbasis, nfuns, type = "Cos", basis.pars = NULL)
}
\arguments{
  \item{nbasis}{number of basis functions}

  \item{nfuns}{number of curves to generate}

  \item{type}{character specifying the type of basis
  functions to use. Default is \code{type="Cos"}.}

  \item{basis.pars}{extra parameters used in the
  construction of basis functions}
}
\value{
coef matrix containing coefficients of the basis functions.
The number of columns is equal to the number of basis
functions

basis.fns list of basis functions created by
\code{create_basis}
}
\description{
For the specified basis functions, spatial dependence is
incorporated through the covariance structure of the
spatial fields connected to the coefficients. The
coefficients of the first basis funciton is a simulated
Gaussian random field with covariance model equal to the
first specified covariance model and parameters equal to
the first specified covariance parameters. The same is true
for the remaining basis functions, which means that the
number of basis functions should be equal to the number of
covariance models specified, and the order in which they
are specified is matters. The Gaussian random fields are
simulated with the \code{grf} function in the \code{geoR}
package. See \code{geoR} documentation for specification of
covariance models.
}

