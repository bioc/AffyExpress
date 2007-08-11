make.formula<-function (target, cov, int=NULL){

	factor.list<-list()
	index.cov<-c()
	formula.cov<-c()
	c.name<-colnames(target)
	
	if (length(int)>0) {

		if (length(int) !=2) stop ("You can only have two numbers")
		index.int1 = find.col.index(target, cov[int[1]])
		index.int2 = find.col.index(target, cov[int[2]])
		formula.int<-paste(as.name(c.name[index.int1]),as.name(c.name[index.int2]),sep="*")
		a<-(cov==cov[int[1]])
		b<-(cov==cov[int[2]])
		c<-a|b
		cov.not.int<-cov[!c]
		if (length(cov.not.int) !=0) {
		
			for (j in 1:length(cov.not.int)) {
				index.cov[j]<-find.col.index(target, cov.not.int[j])
				formula.int<-paste(formula.int,as.name(c.name[index.cov[j]]),sep="+")
			}

		}
		formula<-paste("~", formula.int, sep="")
	}
	else {
		index.cov[1]<-find.col.index(target, cov[1])
		formula.cov<-paste("~",as.name(c.name[index.cov[1]]),sep="")
		
		if (length(cov)>1) {
			for (j in 2:length(cov)) {
				index.cov[j]<-find.col.index(target, cov[j])
				formula.cov<-paste(formula.cov,as.name(c.name[index.cov[j]]),sep='+')
			}
		}	
		formula<-formula.cov
	}
	formula
}



