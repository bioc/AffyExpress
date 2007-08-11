
\name{Filter}
\alias{Filter}
\title{filter an 'ExpressionSet' using different methods}
\description{  
  Create a filtered 'ExpressionSet' based on background, range, or interquartile range
}

\usage{
  Filter(object, numChip=1, bg=0, range=0, iqrPct=0)
}

\arguments{
  \item{object}{an 'ExpressionSet'}
  \item{numChip}{number of chips.  If you would like to filter the 'ExpressionSet' based on
    at least 3 chips greater than 1 (bg=1), then set numChip = 3} 
  \item{bg}{background value. If you would like to filter the 'ExpressionSet' based on
    at least 3 chips greater than 1, then set bg=1}
  \item{range}{range = max value - min value of each gene}
  \item{iqrPct}{interquartile percentage.}
}

\details{
  There are three filtering methods.  The User can use either one, two, or three.
  1.  At least a certain number of chips (numChip) are greater than a given 
      background (bg).
  2.  The range of the gene have to be greater than a given value (range).
  3.  Calulating the interquatile range (IQR) of each gene to create an IQR vector.
      Based on the given percentage (e.g. iqrPct=0.2), find the corresponding
      percentile.  If IQR is less than percentile, the gene will be filtered.   
}
\value{
  a filtered 'ExpressionSet'
}

\author{Xiwei Wu \email{xwu@coh.org}, Xuejun Arthur Li \email{xueli@coh.org}}

\examples{
data(testData)
normaldata<-pre.process("rma",testData)

##At least one chip is greater than 4
filtered.1<-Filter(normaldata, numChip=1, bg=4)  

##At least one chip is greater than 4 and range >0.5
filtered.2<-Filter(normaldata, numChip=1, bg=4, range=0.5)  

##range >0.5 and IQR > 20percentile
filtered.3<-Filter(normaldata, range=0.5, iqrPct=0.2)  
}

\keyword{methods}


