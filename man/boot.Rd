\name{boot}
\alias{boot}
\title{boot, Bootstraps the ASPIC biomass dynamic model.}
\arguments{
  \item{object;}{an \code{aspic} object or}

  \item{object;}{a character string giving an aspic "inp"
  file}

  \item{dir;}{an optional \code{dir} where aspic text files
  used for fitting can be found}
}
\value{
  An aspic object with fitted values and parameter
  estimates
}
\description{
  Bootstraps the aspic model
}
\examples{
\dontrun{
    data(asp)
    asp=boot(asp)}
}
\seealso{
  \code{\link{biodyn},\link{boot},\link{jk}}
}

