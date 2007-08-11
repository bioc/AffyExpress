
SortMat <- function(Mat, Sort) {
    if (dim(Mat)[2] == 1){
        temp<-rep(NA, dim(Mat)[1])
        Mat_temp0<-cbind(Mat, temp)
        m <- do.call("order", as.data.frame(Mat_temp0[,Sort]))
        result0<-Mat_temp0[m,]
        result <-data.frame(result0[,1])
        rownames(result)<-rownames(result0)
        colnames(result)<-colnames(result0)[1]
    }
    else{
        m <- do.call("order", as.data.frame(Mat[,Sort]))
        result<-Mat[m,]
    }
    result
}