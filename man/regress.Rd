
\name{regress}
\alias{regress}
\title{Run regression to fit genewise linear model}
\description{  
    Fit genewise linear model using LIMMA package, ordinary linear regression, 
	or permutation method.
}

\usage{
regress(object, design, contrast, method, adj="none", permute.time=1000) 
}

\arguments{
    \item{object}{an "ExpressionSet"}
    \item{design}{design matrix from the make.design function} 
    \item{contrast}{contrast matrix from the make.contrast function}
    \item{method}{Three methods are supported by this function:
	  "L" for using LIMMA method - compute moderated t-statistics and log-odds 
	  of differential expression by empirical Bayes shrinkage of the standard 
	  errors towards a common value,  
	  "F" for using ordinary linear regression, 
	  "P" for permuation test by resampling the phenotype}
    \item{adj}{adjustment method for multiple comparison test, including "holm", 
	  "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".  The default
	  value is "none". Type help(p.adjust) for more detail.}
	\item{permute.time}{number of permutation times, only used for the permutation 
	  method.}
}

\value{
    A dataframe contains rows for all the genes from object and the
    following columns: ID(probeid); Log2Ratio (estimate of the effect or the contrast, 
    on the log2 scale);  F (F statistics); P.Value (raw p-value); adj.P.Value (adjusted 
    p-value or q-value)
}

\references{Smyth, G.K. (2005) Limma: linear models for microarray data.
  In: Bioinformatics and Computational Biology Solutions using R and 
  Bioconductor, R. Gentleman,  V. Carey, S. Dudoit, R. Irizarry, 
  W. Huber (eds.), Springer, New York, pages 397-420}
  
\author{Xiwei Wu \email{xwu@coh.org}, Xuejun Arthur Li \email{xueli@coh.org}}

\examples{
data(testData)
normaldata<-pre.process("rma",testData)

## Create design matrix
design<-make.design(pData(normaldata), "group")

## Create contrast matrix - Compare group "A" vs. "C"
contrast<-make.contrast(design, "A", "C")

## Identify differentially expressed gene by using LIMMA method
result<-regress(normaldata, design, contrast, "L")
}

\keyword{regression}


