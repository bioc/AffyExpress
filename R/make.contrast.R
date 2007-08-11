
make.contrast<-function(design.matrix, compare1=NULL, compare2=NULL, 
    level=NULL, interaction=FALSE){
	
	param<-colnames(design.matrix)
	if (interaction== TRUE) {
		inter.eq<-array(rep(0,length(param)*length(param)), 
		    dim=c(length(param),length(param)))
		inter.index<-rep(FALSE,length(param))
		for (j in 2:length(param)){
			if (length(grep(":",param[j]))!=0) {
				inter.eq[j,j]<-1
				inter.index[j]<-TRUE
			}
		}
		eq<-cbind(inter.eq[,inter.index])
	}
	else {
		eq1<-rep(0, length(param))
		eq1[1]<-1
		eq2<-eq1
		for (i in 2:length(param)){
			if (length(grep(":",param[i]))==0) {
				value<-unlist(strsplit(param[i], "\\/"))[2]
                        if (compare1 == value) eq1[i] <- 1
				if (compare2 == value) eq2[i] <- 1
			}
			else {
				var.1<-unlist(strsplit(param[i], "\\:"))[1]
				var.2<-unlist(strsplit(param[i], "\\:"))[2]
				eq1[i]<-cal.eq(compare1, var.1, var.2, level)
				eq2[i]<-cal.eq(compare2, var.1, var.2, level)
			}
		}
	eq<- cbind(eq1 - eq2)
	}
	eq
}

