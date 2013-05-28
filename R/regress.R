regress<-function(object, design, contrast, method, adj="none", 
    permute.time=1000) 
{
	fit <- lmFit(object, design)
	fit2 <- contrasts.fit(fit, contrast)
	if (method == "F") {
		fit2 <- f.test(fit2)
	}
	else if (method == "L"){
		fit2 <- eBayes(fit2)
	}
	else if (method == "P"){
		fit2 <- f.test(fit2)
	 	f<-get.f(fit2)
		p<-matrix(NA, nrow=length(f), ncol=(permute.time-1))
		for (i in 1:(permute.time-1)){
    	      p[,i]<-permute.1(object, design, contrast, f)
		}
		p.1<-rep(1,length(f))
		p<-cbind(p, p.1)
		count<-apply(p,1,sum)
		fit2$F.p.value<-count/permute.time
	}
	adj.P.Value <- p.adjust(fit2$F.p.value, method = adj)
	diff<-cbind(rownames(fit2), as.data.frame(fit2$coefficients), 
	    as.data.frame(fit2$F), as.data.frame(fit2$F.p.value), 
		as.data.frame(adj.P.Value)) 
	Log2Ratio.name<-c()
	for (i in 1:(dim(contrast)[2])){
		Log2Ratio.name<-c(Log2Ratio.name, paste("Log2Ratio",i,sep="."))
	}
	names(diff)<-c("ID", Log2Ratio.name, "F", "P.Value","adj.P.Val")
	diff_sort <- SortMat(diff, Sort=1)
	diff_sort
}