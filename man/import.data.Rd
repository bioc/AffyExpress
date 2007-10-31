
\name{import.data}
\alias{import.data}
\title{Importing Affy Cel File}
\description{  
  This function imports cel files and create an 'AffyBatch' based
	on the imported cel
}

\usage{
import.data(phenotype.file, path=getwd(), ...)
}

\arguments{
  \item{phenotype.file}{the name of the phenotype file - text file}
  \item{path}{the name of the directory storing phenotype.file and
    the cel files, the default value is the current working directory} 
  \item{...}{Refers to "read.AnnotatedDataFrame" (Biobase)} 
}

\value{
  an 'AffyBatch'
}

\author{Xiwei Wu \email{xwu@coh.org}, Xuejun Arthur Li \email{xueli@coh.org}}

\examples{
datadir <- system.file("extdata", package = "estrogen")
raw <- import.data("phenodata.txt", path=datadir, header=TRUE, sep="", 
    row.names="filename")
}

\keyword{methods}


