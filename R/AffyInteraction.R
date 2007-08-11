
AffyInteraction<-function(object, method, main.var, strata.var,compare1, 
    compare2, covariates=NULL, p.int=0.05, m.int=0, adj.int="none",
	p.value=0.05, m.value=0, adj="none", filename1="result", 
      filename2="inter_result"){
	
    target<-pData(object)
    cdf.name <-annotation(object)
    result<-list()		
    if (length(covariates)==0) {
        design.int<-make.design(target, c(main.var, strata.var), int=c(1,2))
	  design.all<-make.design(target, main.var)
    }
    else{
	  design.int<-make.design(target, c(main.var, strata.var, covariates), 
	      int=c(1,2))
	  design.all<-make.design(target, c(main.var,covariates))
    }
    contrast.int<-make.contrast(design.int, interaction=TRUE)
    contrast.all<-make.contrast(design.all, compare1, compare2)
    result.int<-regress(object, design.int, contrast.int, method, adj.int)
    print ("For interaction test:")
    result[[1]]<-select.sig.gene(result.int, p.int, m.int)
    names(result)[1]<-"Interaction Test"	
    sig.ID<-result[[1]]$ID[result[[1]]$significant==TRUE]
    sig.index<-match(sig.ID, rownames(exprs(object)))
    result.no.int<-regress(object[-sig.index,], design.all, contrast.all, 
	    method, adj)
    print ("------------------------------------------------------")
    print ("For genes without interaction effect:")
    result[[2]]<-select.sig.gene(result.no.int, p.value, m.value)
    names(result)[2]<-"Genes Without Interaction Effect"
    print ("------------------------------------------------------")
    print ("For genes with interaction effect:")
    str.table<-post.interaction(strata.var, compare1, compare2, 
        design.int, object[sig.index,], method, adj, p.value, m.value)
    for (j in 1:length(str.table)){
	    result[[2+j]]<-str.table[[j]]
	    names(result)[2+j]<-names(str.table)[j]
    }
    result2html(cdf.name, result[[2]], filename1)
    interaction.result2html(cdf.name, str.table, result[[1]], filename2)
    result
}


