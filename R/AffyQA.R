
AffyQA<-function (parameters, raw, Output = "AffyQA.html") 
{
    
    library(simpleaffy)
    targets_old <- pData(raw)
    dum <- rep(c("yellow", "red", "turquoise", "brown", "darkviolet", 
        "steelblue", "springgreen", "sienna", "darkslategray", 
        "salmon", "purple", "olivedrab", "orange", "wheat", "hotpink", 
        "black", "orchid"), 10)
   

    print("Retrieving grouping parameters")
    num <- match(parameters, colnames(targets_old))
    if (length(num) < 1 | sum(is.na(num)) > 0) {
        stop("Please check your parameter names, which must match the \n  colnames of phenotype files.")
    }
    target_sort <- SortMat(targets_old, Sort = num)
    target_sort <- as.matrix(target_sort)
    size <- nrow(target_sort)
    targets <- c()
    for (i in 1:size) {
        targets[i] <- target_sort[i, num[1]]
        if (length(num) > 1) {
            for (j in 2:length(num)) {
                targets[i] <- paste(targets[i], target_sort[i, 
                  num[j]], sep = ".")
            }
        }
    }
    singular <- unique(as.character(targets))
    counter <- array(0, length(singular))
    for (x in 1:length(singular)) {
        count = 0
        for (k in 1:length(targets)) {
            if (targets[k] == singular[x]) {
                count <- count + 1
            }
        }
        counter[x] = count
    }
    for (x in 1:length(singular)) {
        if (counter[x] > 1) {
            index <- which(targets == singular[x])
            targets[index] <- paste(targets[index], c(1:counter[x]), 
                sep = ".")
        }
    }
    target_sort <- cbind(target_sort, targets)

    colnames(target_sort)[length(colnames(target_sort))]<-"Sample ID"

    raw <- raw[, match(rownames(target_sort), rownames(pData(raw)))]
    genome <- cdfName(raw)
    testchip <- grep("^Test3", genome)
    if (length(testchip) > 0) {
        genome <- "test3"
        raw@cdfName <- "test3"
    }
    genome <- cleancdfname(cdfName(raw))

    included <-getAlpha1(genome)

    HTMLStart(filename = Output)
    HTML("<a name= \"top\"></a>", file = Output)
    titl <- as.title("<p align=center>Affymetrix Quality Assessment</p>")
    HTML(titl, file = Output, append = FALSE)
    targetDisplay <- xtable(target_sort)
    HTML(targetDisplay, file = Output)
    HTML(paste("The samples are grouped by:", paste(parameters, 
        collapse = " and ")), file = Output)
    HTML("<hr><dl><li> Figure 1 -<a href= #fig1> Affy Recommended Quality \n\t    Assessment </a>", 
        file = Output)
    HTML("<li> Figure 2 -<a href = #fig2> RNA Quality Assessment</a>", 
        file = Output)
    HTML("<li> Figure 3 -<a href = #fig3> Sample Quality Assessment </a>", 
        file = Output)
    HTML("<li> Figure 4 - Quality Diagnostic using PLM", file = Output)
    HTML("<dd>Figure 4a - <a href = #fig4>Pseudo-chip Images </a>", 
        file = Output)
    HTML("<dd>Figure 4b -<a href = #fig5> NUSE and RLE plots</a>", 
        file = Output)
    HTML("<li> <font size=4 color=red> I need <a href=\n\t    http://informatics.coh.org/microarray/quality_control.asp> \n\t\thelp</a> to interpret these plots. </font></dl>", 
        file = Output)
    if (is.na(included)) {
        figure1 <- "Figure1.png"
        png(file = figure1)
	      plot(1,1, col="white", yaxt="n", xaxt="n", xlab="", ylab="", bty="n")
	      text(1,1,label="Your Chip is not supported by simplyaffy", col="red")
	      HTML("<hr><a name= \"fig1\"></a>", file = Output)
        HTMLInsertGraph(figure1, file = Output, Caption = "Figure 1: Affymetrix \n\t\t    recommended Quality Assessment", 
            GraphBorder = 1, Align = "center", append = TRUE)
        dev.off()

    }
    else {
        data_mas5 <- call.exprs(raw, sc = 500, "mas5")
	      qc <- qc(raw, data_mas5)
        figure1 <- "Figure1.png"
        png(file = figure1, height = 960)
        layout(matrix(c(1, 3, 5, 7, 2, 4, 6, 7), nr = 4, ncol = 2), 
            TRUE)
        par(mar = c(2, 2, 2, 1))
        description(raw)@title <- "Raw Intensity"
        boxplot(raw, cex.main = 0.9, cex.axis = 0.7, col = dum[1:size], 
            names = rep("", size))
        barplot(avbg(qc), names = " ", cex.axis = 0.7, col = dum[1:size], 
            cex.main = 0.9, font.main = 3, main = "Average background", 
            font.main = 1)
        barplot(percent.present(qc), names = " ", cex.axis = 0.7, 
            col = dum[1:size], cex.main = 0.9, font.main = 3, 
            main = "Percentage Present", font.main = 1)
        barplot(sfs(qc), main = "Scaling Factor", cex.axis = 0.7, 
            names = " ", cex.main = 0.9, font.main = 3, col = dum[1:size], 
            font.main = 1)
        boxplot(data.frame(spikeInProbes(qc)), col = dum, cex.axis = 0.7, 
            cex.main = 0.9, main = "Hybridization Controls", 
            names = c(" ", " ", " ", " "))
        legend("topleft", col = dum[1:4], cex = 0.8, lty = 1, 
            legend = c("BioB", "BioC", "BioD", "Cre"))
        boxplot(data.frame(2^ratios(qc)), col = dum, cex.axis = 0.7, 
            cex.main = 0.9, main = "Housekeeping Controls", names = c(" ", 
                " ", " ", " "))
        legend("topright", col = dum[1:4], cex = 0.8, lty = 1, 
            legend = c("ACTIN 3'/5'", "GAPDH 3'/5'", "ACTIN 3'/M", 
                "GAPDH 3'/M"))
        ##barplot(0.1, cex.axis = 0.01, yaxt = "n", xaxt = "n", 
        ##    col = "white")
        plot(1,1, col="white", yaxt="n", xaxt="n", xlab="", ylab="", bty="n")
        legend("center", col = dum[1:size], lty = 1, cex = 0.9, 
            legend = targets)
        HTML("<hr><a name= \"fig1\"></a>", file = Output)
        HTMLInsertGraph(figure1, file = Output, Caption = "Figure 1: Affymetrix \n\t\t    recommended Quality Assessment", 
            GraphBorder = 1, Align = "center", append = TRUE)
        dev.off()
    }
    print("Generating RNA digestion plot...")
    figure2 <- "Figure2.png"
    png(file = figure2)
    deg <- AffyRNAdeg(raw)
    summaryAffyRNAdeg(deg)
    flag <- FALSE
    for (i in 1:dim(deg$means.by.number)[1]) {
        for (j in 1:dim(deg$means.by.number)[2]) {
            if (deg$means.by.number[i, j] < 0) {
                deg$means.by.number[i, j] <- NA
                flag <- TRUE
            }
        }
    }
    if (flag == TRUE) {
        for (i in 1:dim(deg$means.by.number)[1]) {
            for (j in 1:dim(deg$means.by.number)[2]) {
                if (is.na(deg$means.by.number[i, j])) {
                  deg$means.by.number[i, j] <- mean(deg$means.by.number[i, 
                    ], na.rm = TRUE)
                  deg$ses[i, j] <- mean(deg$ses[i, ], na.rm = TRUE)
                }
            }
        }
    }
    plotAffyRNAdeg(deg, col = dum[1:size])
    legend("topleft", col = dum[1:size], lty = 1, cex = 0.7, 
        legend = targets)
    dev.off()
    HTML("<hr><a name= \"fig2\"></a>", file = Output)
    HTMLInsertGraph(figure2, file = Output, Caption = "Figure 2: RNA Quality \n\t    Assessment", 
        GraphBorder = 2, Align = "center", append = TRUE)
    data <- rma(raw)
    r <- cor(exprs(data))
    d <- 1 - r
    hc <- hclust(as.dist(d), method = "average")
    print("Generating sample hierarchical clustering plot...")
    figure3 <- "Figure3.png"
    png(file = figure3)
    plot(hc, main = "Hierarchical Clustering of Samples", xlab = " ", 
        sub = " ", labels = targets)
    dev.off()
    HTML("<hr><a name= \"fig3\"></a>", file = Output)
    HTMLInsertGraph(figure3, file = Output, Caption = "Figure 3: Sample Quality \n\t    Assessment", 
        GraphBorder = 2, Align = "center", append = TRUE)
    row <- round(sqrt(size))
    for (x in row:size) {
        if (((row * x) - size) >= 0) {
            col <- x
            break
        }
    }
    print("Generating pseudo-chip images...")
    figure4 <- "Figure4.png"
    png(file = figure4)
    sampleNames(raw) <- targets
    data_PLM <- fitPLM(raw, output.param = list(varcov = "none"))
    par(mar = c(1, 1, 2, 1), mfrow = c(row, col))
    for (i in 1:size) {
        image(data_PLM, which = i)
    }
    dev.off()
    HTML("<hr><a name= \"fig4\"></a>", file = Output)
    HTMLInsertGraph(figure4, file = Output, Caption = "Figure 4a: Pseudo-chip \n\t    Images", 
        GraphBorder = 2, Align = "center", append = TRUE)
    figure5 <- "Figure5.png"
    png(file = figure5)
    par(mfrow = c(1, 2), las = 2, mar = c(3, 2, 2, 1))
    boxplot(data_PLM, main = "NUSE Plot", cex.axis = 0.7, names = targets, 
        col = dum[1:size])
    Mbox(data_PLM, main = "LPE Plot", names = targets, cex.axis = 0.7, 
        col = dum[1:size])
    dev.off()
    HTML("<a name= \"fig5\"></a>", file = Output)
    HTMLInsertGraph(figure5, file = Output, Caption = "Figure 4b: Normalized \n\t    Unscaled Standard Error Plot and Relative Log Expression Plot", 
        GraphBorder = 2, Align = "center", append = TRUE)
    HTML("<hr>This report was generated by ", file = Output)
    HTML(paste("<li>", "affy Version", package.version("affy"), 
        "(by Rafael A. Irizarry, Laurent Gautier and Benjamin Bolstad),"), 
        file = Output)
    HTML(paste("<li>", "affyPLM Version", package.version("affyPLM"), 
        "(by Benjamin Bolstad),"), file = Output)
    HTML(paste("<li>", "limma Version", package.version("limma"), 
        "(by Gordon Smyth),"), file = Output)
    HTML(paste("<li>", "R2HTML Version", package.version("R2HTML"), 
        "(by Eric Lecoutre),"), file = Output)
    HTML(paste("<li>", "simpleaffy Version", package.version("simpleaffy"), 
        "(by Crispin J. Miller), and"), file = Output)
    HTML(paste("<li>", "xtable Version", package.version("xtable"), 
        "(by David Dahl)"), file = Output)
    HTML(paste("<hr>Generated on: ", date()), file = Output)
    HTMLStop()
}

