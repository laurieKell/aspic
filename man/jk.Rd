\name{jk}
\alias{jk}
\title{Runs a jack knife simulation, by leaving out 1 cpue index observation at a time}
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
  Runs a jack knife simulation, by leaving out 1 cpue index
  observation at a time
}
\examples{
\dontrun{
    data(asp)
    asp=jk(asp)}
}
\seealso{
  \code{\link{biodyn},\link{boot},\link{fit}}
}

