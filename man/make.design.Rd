
\name{make.design}
\alias{make.design}
\title{Create a Design Matrix}
\description{  
  Create a design matrix for a linear model 
}

\usage{
  make.design(target, cov, int=NULL)
}

\arguments{
  \item{target}{a data frame contains chip and covaraite information, 
    or experimental phenotypes recorded in eSet and ExpressionSet-derived classes}
  \item{cov}{a list of 1-n covariates} 
  \item{int}{if int=NULL, the interaction effect is not considered;
    otherwise, use two integers to indicate which covariates are 
	considered for interaction effect.  For example, 
	if cov<-c("estrogen","drug","time")
	and you are considering the interaction between "estrogen" and 
	"time", then you would write	int=c(1,3) }
}

\value{
  a matrix containing design matrix for the linear model
}

\author{Xiwei Wu \email{xwu@coh.org}, Xuejun Arthur Li \email{xueli@coh.org}}

\seealso{
  \code{\link{make.contrast}}
}

\examples{
target<-data.frame(drug=(c(rep("A",4),rep("B",4),rep("C",4))), 
gender=factor(c(rep("M",6),rep("F",6))), 
group=factor(rep(c(1,2,3),4)))
			 
#To create a design matrix using "drug", "gender" as covariates
design1<-make.design(target, c("drug","gender"))
	
#To create a design matrix by using "drug","gender","group" as covariates, 
#and consider the interaction effect of "drug" and "group"
design2<-make.design(target, c("drug","gender", "group"), int=c(1,3))
}

\keyword{array}
