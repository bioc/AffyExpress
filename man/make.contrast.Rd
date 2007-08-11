
\name{make.contrast}
\alias{make.contrast}
\title{Create a contrast matrix}
\description{  
  Create a contrast matrix based on a given design matrix
}

\usage{
  make.contrast(design.matrix, compare1=NULL, compare2=NULL, level=NULL, 
    interaction=FALSE)
}

\arguments{
  \item{design.matrix}{a design matrix returned from the make.design funcion}
  \item{compare1}{the first value of the main covariate.  For example, suppose
	that the main covariate is "drug", and there are three unique values:
	"drug1", "drug2", and "placebo".  You would like to compare "drug1" to
	"drug2".  Then you would use "drug1" as compare1.  If interaction==TRUE,
	do not specify this value.} 
  \item{compare2}{the second value of the main covariate. Based on the example
        above, if you would like to compare "drug1" vs "drug2", then you would use
	"drug2" as compare2.  If interaction ==TRUE, do not specify this value}
  \item{level}{you only specify this term when the design matrix contains an
	interaction term.  Suppose that you would like to compare "drug1" to "drug2"
	only when estrogen is "present", where "present" is one of the values of the
	estrogen variable.  You will use "present" as level.  If interaction==TRUE,
	do not specify this value}
  \item{interaction}{you only specify interaction=TRUE when you would like to 
        detect the interaction effect between two covariates. When you specify 
        interaction=TRUE, do not provide values for compare1, compare2, and level}
}

\value{
  contrast matrix for the linear model
}

\author{Xiwei Wu \email{xwu@coh.org}, Xuejun Arthur Li \email{xueli@coh.org}}

\seealso{
  \code{\link{make.design}}
}

\examples{
target<-data.frame(drug=(c(rep("A",4),rep("B",4),rep("C",4))), 
gender=factor(c(rep("M",6),rep("F",6))), 
group=factor(rep(c(1,2,3),4)))

# Example1:  Compare drug "A" vs. "B"
design1<-make.design(target, "drug")
contrast1<-make.contrast(design1, "A", "B")

# Example2:  Compare drug "A" vs. "B", adjusting for "group" variable
design2<-make.design(target, c("drug","group"))
contrast2<-make.contrast(design2, "A", "B")

# Example3:  Suppose you are interested in "drug", "group" interaction
design3<-make.design(target, c("drug","group"), int=c(1,2))
contrast3<-make.contrast(design3, interaction=TRUE)

# Example4:  Compare drug "A" vs. "B" among "male"
# Notice that you must use an design matrix containing an interaction term
design4<-make.design(target, c("drug","gender"), int=c(1,2))
contrast4<-make.contrast(design4, "A", "B", "M")
}

\keyword{array}


