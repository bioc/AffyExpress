
\name{result2html}
\alias{result2html}
\title{output differentially expressed genes to a HTML file}
\description{  
  output differentially expressed genes to a HTML file based on
    a result table from the select.sig.gene function.  It contais the 
    following columns:  Probe, Symbol, Description, GenBank, LocusLink,
    Log2ratio, and p value.
}
\usage{
result2html(cdf.name, result, filename="result")
}

\arguments{
  \item{cdf.name}{cdf name which can be obtained from the annotation function}
  \item{result}{a data frame returned from the gene.select function}
  \item{filename}{a file name for the output}
}

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

## Select differentially expressed gene based on p <0.05 and 
## fold change >=log2(1.5)
select<-select.sig.gene(result, p.value=0.05, m.value=log2(1.5))

## Output differentially expressed gene to a example.html
result2html(annotation(normaldata), select, "example")
}

\keyword{misc}

