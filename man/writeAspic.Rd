\name{writeAspic}
\alias{writeAspic}
\title{\code[writeAspic} Writes the ASPIC text input file \cod{inp} to a file or connection.}
\arguments{
  \item{object;}{of type \code{aspic}}
}
\description{
  \code[writeAspic} Writes the ASPIC text input file
  \cod{inp} to a file or connection.
}
\note{
  The executable version of ASPIC uses an input file, this
  method generates that file
}
\examples{
\dontrun{
    data(asp)
    writeAspic(asp,"aspic.inp")}
}
\seealso{
  \code{\link{readAspic},\link{aspic:::files}}
}

