
find.col.index<-function(filename, parameter){

    index = which(colnames(filename) ==parameter)
    num <- c()
    if (length(index) >0){
	num <- c(num, index)
    }
    else stop (paste(parameter," is not found",sep=""))
    num
}