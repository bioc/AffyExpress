
f.test<-function(fit){
    fit$t <- fit$coef / fit$stdev.unscaled / fit$sigma
    F.stat <- classifyTestsF(fit, fstat.only = TRUE, df=fit$df.residual)
    fit$F <- as.vector(F.stat)
    df1 <- attr(F.stat, "df1")
    df2 <- attr(F.stat, "df2")
    fit$F.p.value <- pf(fit$F, df1, df2, lower.tail = FALSE)
    fit
}