select.sig.gene<-function(top.table, p.value =0.05, m.value =0)  
{
	good.p<-top.table$adj.P.Val <p.value
	index<-grep("Log2Ratio", names(top.table))
	good.matrix<-matrix(0,nrow=dim(top.table), ncol=length(index))
	for (i in 1:length(index)){
		good.matrix[,i]<-abs(top.table[,index[i]]) >= m.value 
	}
	good.m<-(apply(good.matrix, 1, sum)>0)
	significant<-good.p & good.m
	result<-cbind(top.table, significant)
	print(paste("There are ", sum(significant), " differentially expressed genes",sep=""))
	print("based on your selection criteria.")
	result
}