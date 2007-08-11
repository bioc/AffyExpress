
get.f<-function(fit){
    fit$t <- fit$coef / fit$stdev.unscaled / fit$sigma
    F.stat <- classifyTestsF(fit, fstat.only = TRUE, df=fit$df.residual)
    fit$F <- as.vector(F.stat)
    fit
}