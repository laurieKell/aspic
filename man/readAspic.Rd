\name{readAspic}
\alias{readAspic}
\title{read ASPIC text files.}
\arguments{
  \item{file;}{the name of the file which the data are to
  be read from.}
}
\value{
  The object returned depends upon what is being read in.
}
\description{
  Reads the the various \code{ASPIC} files and returns if
  possible an \code{FLR} object, otherwise returns a
  \code{data frame}.
}
\section{Longwinded Explanation}{
  The executable version of ASPIC uses a number of text
  files i.e.  \describe{ \item{\code{inp}, the input file
  with data, starting guesses, and run settings and for
  output,} \item{\code{bio} estimated stock and F
  trajectory for each bootstrap trial,} \item{\code{prb} as
  \code{bio} but with projection results,}
  \item{\code{rdat} inputs and estimates specially
  formatted for R, } \item{\code{det} estimates }}
}
\examples{
\dontrun{
   dirMy="/home/laurie/Desktop/gcode/gbyp-sam/tests/aspic/bet/2010/run3"

   ## list of file types
   aspic:::files

   is(readAspic(paste(dirMy,"aspic.inp",sep="/")))

   is(readAspic(paste(dirMy,"aspic.bio",sep="/")))

   is(readAspic(paste(dirMy,"aspic.inp",sep="/")))

   is(readAspic(paste(dirMy,"aspic.prb",sep="/")))

   is(readAspic(paste(dirMy,"aspic.rdat",sep="/")))

   is(readAspic(paste(dirMy,"aspic.det",sep="/")))
   }
}
\seealso{
  \code{\link{writeAspic},\link{biodyn}}
}

