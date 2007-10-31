import.data<-function(phenotype.file, path=getwd(), ...){
    phenoD<-read.AnnotatedDataFrame(phenotype.file, path=path, ...)
    raw<-ReadAffy(filenames=rownames(pData(phenoD)), phenoData=phenoD, celfile.path=path)
    raw
}
