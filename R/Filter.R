
Filter<-function(object, numChip=1, bg=0, range=0, iqrPct=0)
{
    ## method 1:  filtering based on minimum # of chip > bg
    if (bg > 0){
        maxChip<-dim(exprs(object))[2]
        if (numChip > maxChip){
            stop (paste("numChip must <= ", maxChip,sep=""))
        } 
        f<-kOverA(numChip, bg) 		
	fun<-filterfun(f)
	good1<-genefilter(object, fun)
    }
    else {
        good1<-rep(TRUE, dim(exprs(object))[1])
    }
    ## method 2: max-min > range
    if (range >0) {
        max<-apply(exprs(object), 1, max)
	min<-apply(exprs(object), 1, min)
	data_ratio<-max-min
	if (range > max(data_ratio)){
            stop (paste("range must < ", max(data_ratio),sep=""))
        }
	good2 <- data_ratio >=range
    }
    else {
        good2<-rep(TRUE, dim(exprs(object))[1])
    }
    ## method 3: IQR > iqrPct 
    if (iqrPct>0) {
        iqr<- apply(exprs(object), 1, IQR)
	good3 <-iqr >= quantile(iqr, iqrPct)
    }
    else {
        good3<-rep(TRUE, dim(exprs(object))[1])
    }
    good <-good1 & good2 & good3
    print(paste("After Filtering, N = ", sum(good), sep=""))
    filtered<-object[good]
    filtered
}