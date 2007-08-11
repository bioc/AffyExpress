result2html<-function(cdf.name, result, filename="result"){
    
    library(cdf.name,character.only =TRUE)
    good <-result$significant
    if (sum(good)==0) print("No significant result is generated")
    else{
        fullresult<-cbind(result$ID, result$Log2Ratio.1, result$adj.P.Val)
        gN <- as.character(result$ID)
        aaf.handler()
        anncols <- aaf.handler()[c(1:3, 7, 8)]
        probeid <- gN[good]
        anntable <- aafTableAnn(probeid, cdf.name, anncols)
        testtable <- aafTable( "Log2 ratio"=fullresult[,2][good],
        "P.Value"=fullresult[,3][good])
        table<-merge(anntable, testtable)
        saveHTML(table, paste(filename,"html", sep="."), title="Target Genes")
    }
}
