\name{fit}
\alias{fit}
\title{Fit the \code{aspic} biomass dynamic model.}
\arguments{
  \item{object;}{an \code{aspic} object}

  \item{dir;}{an optional \code{dir} where aspic text files
  used for fitting can be found}
}
\value{
  An aspic object with fitted values and parameter
  estimates
}
\description{
  Fits the aspic model to catch and catch per unit effort
  data
}
\examples{
\dontrun{
    data(asp)
    asp=fit(asp)}
}
\seealso{
  \code{\link{aspic},\link{biodyn},\link{boot},\link{jk}}
}

