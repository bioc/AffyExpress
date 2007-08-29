

interaction.result2html<-function(cdf.name, result, inter.result, filename="inter_result")
{

    library(cdf.name,character.only =TRUE) 
	require("annaffy")
    fullresult<-list()
    name.index<-list()
    g<-list()
    for (j in 1:length(result)){
        fullresult[[j]]<-cbind(result[[j]]$ID, result[[j]]$Log2Ratio.1, 
	    result[[j]]$adj.P.Val)
	name.index[[j]]<-(unlist(strsplit(names(result)[j], "\\.")))[1]
	colnames(fullresult[[j]])<-c("ID", "M", "adj.P.Val")
	colnames(fullresult[[j]])[2]<-paste(name.index[[j]], ":Log2Ratio", 
	    sep="")
	colnames(fullresult[[j]])[3]<-paste(name.index[[j]],":Adj.P",sep="")
	g[[j]]<-result[[j]]$significant
    }
    result.table<-merge(fullresult[[1]], fullresult[[2]])
    good <-g[[1]]|g[[2]]
    if (length(result)>2){
        for (i in 3:length(result)){
	    result.table<-merge(result.table, fullresult[[i]])
	    good<-good|g[[i]]
	}
    }
    if (sum(good)==0) print ("No significant result is generated for the 
        interaction model")
    else {
	gN <- as.character(result[[1]]$ID)
	aaf.handler()
	anncols <- aaf.handler()[c(1:3, 7, 8)]
	probeid <- gN[good]
	anntable <- aafTableAnn(probeid, cdf.name, anncols)
	t.table<-list()
	for (h in 1:length(result)){
	    t.table[[h]]<-aafTable(result.table[,(h*2)][good], 
	        result.table[,(h*2+1)][good], 
		colnames=colnames(result.table)[(h*2):(h*2+1)])
	}
	testtable<-merge(t.table[[1]], t.table[[2]])
	if (length(result)>2){
	    for (i in 3:length(result)){
		testtable<-merge(testtable, t.table[[i]])
	    }
	}
	table<-merge(anntable, testtable)
	inter.sig<-cbind(inter.result$ID, inter.result$adj.P.Val)
	match.index<-match(result.table$ID[good], inter.sig[,1])
	inter.sig1<- aafTable("Interaction.P"=inter.sig[,2][match.index])
	table1<-merge(table, inter.sig1)
	saveHTML(table1, paste(filename,"html", sep="."), title="Interaction Table")
    }
}

