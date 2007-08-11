
\name{interaction.result2html}
\alias{interaction.result2html}
\title{output differentially expressed genes for the interaction model to a HTML file}
\description{  
  output differentially expressed genes for the interaction model to a HTML file.
  It contais the following columns:  ProbeID, Symbol, Description, GenBank, 
  LocusLink, Log2ratio for each stratum, p value for each stratum, and 
  interaction p value.
}
\usage{
interaction.result2html(cdf.name, result, inter.result,filename="inter_result")
}

\arguments{
  \item{cdf.name}{cdf name which can be obtained from annotation function}
  \item{result}{a list of data frame returned from post.interaction function}
  \item{inter.result}{a data frame returned from select.sig.gene function, 
    this is the result based on testing the interaction effect.}
  \item{filename}{the name of the output file}
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

## Output significant result for the interaction model
interaction.result2html(annotation(normaldata), result, result.int, filename="interaction")
}

\keyword{misc}