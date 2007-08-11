
\name{AffyQA}
\alias{AffyQA}
\title{
  Microarray quality control and assessment
}
\description{  
Create quality control report in an HTML file that contains a set of assessment plots,
  including Affymetrix recommended quality assessment, RNA quality assessment,
  sample quality assessment, quality diagnostic using PLM (pseudo-chip images 
  and NUSE and RLE plots) in your current working directory.
}
\usage{
AffyQA(parameters, raw, Output="AffyQA.html") 
}

\arguments{
  \item{parameters}{a list of variables}
  \item{raw}{an 'AffyBatch'} 
  \item{Output}{name of the output file}  
}

\value{
  This function only create an html file
}

\author{Xiwei Wu \email{xwu@coh.org}, Xuejun Arthur Li \email{xueli@coh.org}}

\examples{
\dontrun{
data(testData)
AffyQA(parameters=c("group", "gender"), testData)
}
}


\keyword{misc}

