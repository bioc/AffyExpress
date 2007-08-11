post.interaction<-function(strata.var, compare1, compare2, design.int, 
    object, method, adj="none", p.value=0.05, m.value=0)
{
    target<-pData(object)
    result<-list()
    contrast.sep<-list()
    r.full.temp<-list()
    col.index.1<-find.col.index (target, strata.var)
    u.1<-unique(target[col.index.1])
    for (j in 1: dim(u.1)[1]){
        contrast.sep[[j]]<-make.contrast(design.int, compare1, compare2, 
	    level=t(u.1)[1,j])
	r.full.temp[[j]]<-regress(object, design.int, contrast.sep[[j]], 
	    method=method, adj=adj)

	tempname<-paste("Level",t(u.1)[1,j],sep="-")  
	print (paste ("At ",  tempname, ":", sep=""))
	result[[j]]<-select.sig.gene(r.full.temp[[j]],p.value=p.value, 
	    m.value=m.value) 
      names(result)[j]<-tempname
    }
    result
}