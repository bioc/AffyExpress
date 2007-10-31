
\name{pre.process}
\alias{pre.process}
\title{Data Preprocessing}
\description{  
  This function converts an 'AffyBatch' into an 'ExpressionSet' using either
    RMA or GCRMA methods
}

\usage{
pre.process(method, raw, plot=FALSE, output=FALSE)
}

\arguments{
  \item{method}{either "rma" or "gcrma"}
  \item{raw}{an 'AffyBatch'} 
  \item{plot}{if plot = TRUE, it will plot gene expression for each chip}
  \item{output}{if output = TRUE, it will output 'ExpressionSet' to the
    current directory}
}

\value{
  an 'ExpressionSet'
}

\author{Xiwei Wu \email{xwu@coh.org}, Xuejun Arthur Li \email{xueli@coh.org}}

\examples{
data(testData)
normaldata<-pre.process("rma",testData)
}

\keyword{methods}


