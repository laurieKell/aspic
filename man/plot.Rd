\docType{methods}
\name{plot}
\alias{plot}
\title{Create a \code{ggplot} plot}
\arguments{
  \item{\code{x},}{an object of class \code{aspics}}
}
\value{
  an \code{ggplot2} object
}
\description{
  Creates a \code{ggplot2} object that plots time series of
  biomass, harvest rate and catch. The basic object can
  then be modified by adding ggpot2 layers.
}
\examples{
refpts("logistic",FLPar(msy=100,k=500))
}
\seealso{
  \code{\link{plotSP}}
}

