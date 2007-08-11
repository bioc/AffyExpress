
\name{select.sig.gene}
\alias{select.sig.gene}
\title{
  select differentially expressed genes based on p value and/or fold change
}
\description{  
  select differentially expressed genes based on p value and/or fold change
}
\usage{
select.sig.gene(top.table, p.value =0.05, m.value =0)  
}

\arguments{
  \item{top.table}{an data frame returned from the regress function}
  \item{p.value}{p value, the default value is 0.05} 
  \item{m.value}{fold change cut-off value, default value is 0}
}

\value{
  A dataframe which is the similar to the one returned from regress function.
    An additional column 'significant' is added to the table from the "regress" function.
    If p value < p.value and absolute of fold change value >=m.value then signiicant = TRUE,
    otherwise, significant = FALSE.	
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

}

\keyword{misc}

