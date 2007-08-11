
\name{post.interaction}
\alias{post.interaction}
\title{Create a list of tables for the result based on the main effect variable}
\description{  
  Once the interaction effect is being detected for a list of genes, that means
    that for these group of genes, the main effect is different across different
    level of another variable (call it effect modifier).  This function will create 
    a list of data frame for each level of the effect modifier.  
}

\usage{
post.interaction(strata.var, compare1, compare2, design.int, object, method, adj="none", 
  p.value=0.05, m.value=0)
}

\arguments{
  \item{strata.var}{a categorical variable serves as a potential effect modifier}
  \item{compare1}{the first value of the main covariate.  For example, suppose
    that the main covariate is drug, and there are three unique values:
    "drug1", "drug2", and "placebo".  You would like to compare "drug1" to
    "drug2".  Then you would use "drug1" as compare1} 
  \item{compare2}{the second value of the main covariate. Based on the 
    example above, if you would like to compare "drug1" vs "drug2", then you would use
    "drug2" as compare2}
  \item{design.int}{the design matrix of the interaction effect} 
  \item{object}{an "ExpressionSet"}
  \item{method}{It is used to run regression within each level of the effect modifier.
     Three methods are supported by this function:
	  "L" for using LIMMA method - compute moderated t-statistics and log-odds 
	  of differential expression by empirical Bayes shrinkage of the standard 
	  errors towards a common value,  
	  "F" for using ordinary linear regression, 
	  "P" for permuation test by resampling the phenotype}

  \item{adj}{adjustment method for multiple comparison test, including "holm", 
    "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".  The default
    value is "none". Type help(p.adjust) for more detail.}
  \item{p.value}{a p-value used to select significant gene within each level of the
    effect modifier}
  \item{m.value}{a fold change value used to select significant gene within each 
    level of the effect modifier}
    
}

\value{a list of data frame.  The length of the data frame equals on number of 
  levels of effect modifier.	

    Each dataframe contains rows for all the genes from object and the
    following columns: ID (probeid); Log2Ratio (estimate of the effect or the contrast, 
    on the log2 scale);  F (F statistics); P.Value (raw p-value); adj.P.Value (adjusted 
    p-value or q-value); significant (either TRUE or FALSE based on p.value and m.value)
}

\author{Xiwei Wu \email{xwu@coh.org}, Xuejun Arthur Li \email{xueli@coh.org}}

\examples{
data(testData)
normaldata<-pre.process("rma",testData)

## Create design matrix for interaction effect between "group"
## and "gender"
design.int<-make.design(pData(normaldata), c("group", "gender"), int=c(1,2))

## Create the interaction contrast
contrast.int<-make.contrast(design.int, interaction=TRUE)

## Run Regression to detect interaction effect
result.int<-regress(normaldata, design.int, contrast.int, "L")

## Select differentally expressed genes based on p.value
select.int<-select.sig.gene(result.int, p.value=0.05)

## Identify genes with the interaction effect
sig.ID<-select.int$ID[select.int$significant==TRUE]
sig.index<-match(sig.ID, rownames(exprs(normaldata)))

## Create separate tables for each level of effect modifier
result<-post.interaction("group","M", "F", design.int, normaldata[sig.index,],
  "L","none", 0.05, log2(1.5))
}

\keyword{misc}

