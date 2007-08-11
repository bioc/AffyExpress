
\name{AffyInteraction}
\alias{AffyInteraction}
\title{Analyze interaction effect and produce output}
\description{  
  This wrapper function will create a design matrix and contrast matrix for 
	the interaction test.  Then it will fit linear model to test for 
	interaction effect for each gene and identify genes for which interaction 
	test is being significant.  For genes having the interaction effect, it fits linear 
	model for each genes in each level of strata.var. For genes don't have 
    interaction effect, it fits linear model for each genes without stratifying 
	the strata.var.  In the end, it will output signficant result.
}

\usage{
AffyInteraction(object, method, main.var, strata.var, compare1, compare2, 
  covariates=NULL, p.int=0.05, m.int=0, adj.int="none", p.value=0.05, 
  m.value=0, adj="none", filename1="result", filename2="inter_result")
}

\arguments{
  \item{object}{an "ExpressionSet"}
  \item{method}{Three methods are supported by this function:
	"L" for using LIMMA method - compute moderated t-statistics and log-odds 
	of differential expression by empirical Bayes shrinkage of the standard 
	errors towards a common value; 
	"F" for using ordinary linear regression; 
	"P" for permuation test by resampling the phenotype}
  \item{main.var}{the variable of your main interest}
  \item{strata.var}{a categorical variable serves as a potential effect 
    modifier.  An effect modifier is a variable that modifies the association
	between outcome variable and the main variable. If the interaction exits, 
	the association between the outcome and main.var will be analyzed 
	separately within each stratum of strata.var}
  \item{compare1}{first value of the variable of main interest.  Suppose the 
	main variable is "estrogen", and its has two values: "present" and "absent".  
	You would like to compare "present" versus "absent".  Then you will use 
	compare1 = "present"}
  \item{compare2}{second value of the variable of main interest.  Follow from 
    the same example above, you will set compare2 = "absent"}
  \item{covariates}{a list of covariates, not including main.var and strata.var, 
    the default value is NULL} 
  \item{p.int}{p value for the interaction test}
  \item{m.int}{fold change cut-off value for the interaction test}
  \item{adj.int}{adjustment method for multiple comparison for testing 
    interaction, including "holm", "hochberg", "hommel", "bonferroni", "BH", 
	"BY", "fdr", "none".  Type help(p.adjust) for more detail.}
  \item{p.value}{p value for main effect test}
  \item{m.value}{fold change cut-off value for main effect test}
  \item{adj}{adjustment method for multiple comparison for testing main effect}
  \item{filename1}{name of the output file for the main effect}
  \item{filename2}{name of the output file for the interaction test}
}

\value{a list of data frame:
  The first data frame contains results for genes with interaction effect.
  The second data frame contains results for genes don't have interaction effect.
  The rest of the data frames contain information for main effects for each
  stratum of strata.var.
}

\author{Xiwei Wu \email{xwu@coh.org}, Xuejun Arthur Li \email{xueli@coh.org}}

\examples{
	
## Suppose that you would like to test the interaction effect between "gender" 
## and "group" variables, "gender" is the main effect variable.  For genes in 
## which the interaction test are being significant, you would like to compare 
## "M" vs. "F" within each level of "group".  For genes in which the interaction 
## test are not being significant, you would like to compare "M" vs. "F" without 
## stratifying "group".

data(testData)
normaldata<-pre.process("rma",testData)
result<-AffyInteraction(normaldata, "L", "gender", "group","M", "F", p.int=0.05, 
  m.int=0, adj.int="none", p.value=0.05, m.value=0, adj="none", 
  filename1="result1", filename2="result2")
}

\keyword{misc}



