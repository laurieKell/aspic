\docType{methods}
\name{jk}
\alias{jk}
\alias{profile}
\title{jk, jack knifes \code{aspic}}
\arguments{
  \item{object;}{an \code{aspic} object or}

  \item{object;}{a character string giving an aspic "inp"
  file}

  \item{dir;}{an optional \code{dir} where aspic text files
  used for fitting can be found}

  \item{fitted:}{an \code{aspic} object}

  \item{which:}{\code{character} giving the parameters to
  do the profile for, i.e. to fix.}

  \item{fixed:}{\code{character} any parameters that should
  be fixed, all others are extimated.}

  \item{maxsteps:}{\code{numeric} number of parameter
  values to vary, default is 11.}

  \item{range;}{\code{numeric} how mucg to vary parameter
  values by, default [0.5,1.5].}

  \item{fn:}{\code{function} that gives values to be
  profiled.}

  \item{run:}{\code{logical} if \code{TRUE} then returns
  profile, otherwise it just sets the control object-}
}
\value{
  An aspic object with fitted values and parameter
  estimates

  a \code{data frame} with results turned by \code{fn} by
  values in \code{which}.
}
\description{
  Fits the aspic model to catch and catch per unit effort
  data removing 1 cpue observation at a time

  Performs a profile using residual sum of squares, fixes
  some parameters for a range of values and then estimate
  the others
}
\examples{
\dontrun{
    data(asp)
    asp=jk(asp)}
/dontrun{
data(asp)
res=profile(asp,which="msy",fixed="b0",maxsteps=31,range=c(0.5,1.1))
ggplot(res)+geom_line(aes(k,rss))
}
}
\seealso{
  \code{\link{biodyn},\link{boot},\link{fit}}

  \code{\link{biodyn},\link{fit}}
}

