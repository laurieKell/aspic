\name{aspic}
\alias{aspic}
\title{ASPIC Biomass Dynamic Model Class}
\arguments{
  \item{object,}{a factor or string that specifies the
  model type, has to be one of "fox", "schaefer", "pellat",
  "gulland", "fletcher", "shepherd", "logistic", "genfit"}

  \item{params,}{an \code{FLPar} object with model
  parameters}
}
\value{
  biodyn object

  an \code{aspic} object
}
\description{
  A class that represents the ASPIC biomass dynamic stock
  assessment model.

  Creates an object of the \pkg{aspic} class that
  implements a biomass dynamic stock assessment model.
}
\section{Slots}{
  \describe{ \item{\code{obj}: \code{factor} .}
  \item{\code{conditioning}: \code{factor} .}
  \item{\code{options}: \code{numeric} .}
  \item{\code{index}: \code{data.frame} .}
  \item{\code{stopmess}: \code{character} .}
  \item{\code{rnd}: \code{numeric} .} \item{\code{model}:
  \code{factor} .} \item{\code{catch}: \code{FLQuant} .}
  \item{\code{stock}: \code{FLQuant} .} \item{\code{diags}:
  \code{data.frame} .} \item{\code{params}: \code{FLPar} .}
  \item{\code{control}: \code{FLPar} .}
  \item{\code{priors}: \code{array} .} \item{\code{vcov}:
  \code{FLPar} .} \item{\code{hessian}: \code{FLPar} .}
  \item{\code{objFn}: \code{FLPar} .} \item{\code{mng}:
  \code{FLPars} .} \item{\code{name}: \code{character} .}
  \item{\code{desc}: \code{character} .}
  \item{\code{range}: \code{numeric} .} }
}
\examples{
\dontrun{aspic()}
\dontrun{bd=biodyn("logistic",FLPar(k=50000,msy=1000,b0=1))}
}

