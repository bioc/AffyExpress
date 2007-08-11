
\name{AffyRegress}
\alias{AffyRegress}
\title{
  Select differentially expressed genes, and output the result to a html file
}
\description{  
  This is a wrapper function complete the following tasks:
  1.  Create a design matrix
  2.  Create a contrast 
  3.  Run regression
  4.  Select differentaly expressed gene
  5.  Output the differentally expressed gene to a html file
}
\usage{
AffyRegress(normal.data, cov, compare1, compare2, method, adj="none",
    p.value=0.05, m.value=0, filename="result")
}

\arguments{
  \item{normal.data}{an 'ExpressionSet'}
  \item{cov}{a list of 1-n covariates} 
  \item{compare1}{the first value of the main covariate.  For example, suppose
    that the main covariate is drug, and there are three unique values:
    "drug1", "drug2", and "placebo".  You would like to compare "drug1" to
    "drug2".  Then you would use "drug1" as compare1} 
  \item{compare2}{the second value of the main covariate. Based on the previoius
    example, if you would like to compare "drug1" vs "drug2", then you would use
    "drug2" as compare2}
  \item{method}{Three methods are supported by this function:
    "L" for using LIMMA method - compute moderated t-statistics and log-odds 
       of differential expression by empirical Bayes shrinkage of the standard 
       errors towards a common value; 
    "F" for using ordinary linear regression; 
    "P" for permuation test by resampling the phenotype}
  \item{adj}{adjustment method for multiple comparison test, including "holm", 
    "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".  The default
    value is "none". Type help(p.adjust) for more detail.}
  \item{p.value}{p value, the default value is 0.05} 
  \item{m.value}{fold change cut-off value, default value is 0}
  \item{filename}{name of the output file}
}

\value{
  A dataframe which has the same format as the one created by select.sig.gene 
    function.	
}

\author{Xiwei Wu \email{xwu@coh.org}, Xuejun Arthur Li \email{xueli@coh.org}}


\examples{
data(testData)
normaldata<-pre.process("rma",testData)
result<-AffyRegress(normaldata, "group", "A", "C", "L")
}

\keyword{regression}

