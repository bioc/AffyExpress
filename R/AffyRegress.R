
AffyRegress<-function(normal.data, cov, compare1, compare2, method, 
    int=NULL, level=NULL, adj="none", p.value=0.05, m.value=0, 
	filename="result")
{    
    cdf.name<-annotation(normal.data)
    target<-pData(normal.data)
    design<-make.design(target, cov, int)
    contrast<-make.contrast(design, compare1, compare2, level)
    result<-regress(normal.data, design, contrast, method, adj=adj)
    select<-select.sig.gene(result, p.value=p.value, m.value=m.value)
    result2html(cdf.name, select, filename)
    select
}
