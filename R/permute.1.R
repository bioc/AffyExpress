
permute.1<-function(object, design, contrast, fstat){
    shuffle<-as.integer(sample(rownames(design)))
    design.p<-design[shuffle,]
    fit <- lmFit(object, design.p)
    fit2 <- contrasts.fit(fit, contrast)
    f<-get.f(fit2)
    flag<-rep(0,length(f))
    for (i in 1:length(f)){
        if (f[i,1]$F>fstat[i,1]$F)  
            flag[i]<-1
    }
    flag
}