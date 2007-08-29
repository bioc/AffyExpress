
pre.process<-function(method, raw, plot=FALSE, output=FALSE)
{
    if (method == "gcrma") {
		require("gcrma")
 	    normaldata <- gcrma(raw)
    }
    else if (method == "rma") {
        normaldata  <- rma(raw)
    }
    else stop ("Only rma or gcrma are supported")
    if (plot == TRUE) {
        size<-dim(exprs(normaldata))[2]
        row <- round(sqrt(size))
	    for(x in row:size) {
	        if(((row*x)-size) >= 0) {
	            col <- x; break
	        } 
        }
        par(mar=c(1,1,2,1),mfrow=c(row,col))
        for (i in 1:size) {
            hist(exprs(normaldata)[,i],200)
        }
	}
    if (output == TRUE) {
        write.table(exprs(normaldata),file="normal.csv", sep = ",", 
		        col.names = NA)
    }
    normaldata
}