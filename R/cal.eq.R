
cal.eq<-function(compare, var1, var2, level.var){
    value1 <-0
    value2 <-0
    v1<-unlist(strsplit(var1, "\\/"))[2]
    v2<-unlist(strsplit(var2, "\\/"))[2]
    if (compare == v1 | level.var == v1) value1 <-1
    if (compare == v2 | level.var == v2) value2 <-1
    value <- value1*value2
    value
}