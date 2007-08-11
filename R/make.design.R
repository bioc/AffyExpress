
make.design<-function (target, cov, int = NULL) 
{    
    target<-data.frame(as.matrix(target))
    attach(target, warn.conflicts = FALSE)
    formula1 <- make.formula(target, cov, int = int)
    design <- model.matrix(as.formula(formula1))
    detach(target)
    newcolnames<-colnames(design)
    for (i in 1:length(cov)){
        newcolnames<-sub(cov[i], paste(cov[i],"/",sep=""), newcolnames)
    }
    colnames(design)<-newcolnames
    design
}

