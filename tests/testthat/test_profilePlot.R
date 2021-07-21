context("DGEobj.plots - tests for profilePlot.R functions")


test_that("profilePlot.R: profilePlot()", {
    # testing contrast objects defualts (no sizeBySignificance and no geneSymCol)
    ## BDL_vs_Sham
    contrastDF <- t_obj1$BDL_vs_Sham
    profile_plot <- profilePlot(contrastDF, title = "BDL_vs_Sham")
    expect_s3_class(profile_plot, c("canvasXpress", "htmlwidget"))
    profile_plot <- profilePlot(contrastDF, title = "BDL_vs_Sham", plotType = "ggplot", symbolSize = c(4, 1, 4))
    expect_s3_class(profile_plot, c("gg", "ggplot"))
    ### contrast objects defualts (with sizeBySignificance and no geneSymCol)-
    profile_plot <- profilePlot(contrastDF, title = "BDL_vs_Sham", sizeBySignificance = TRUE)
    expect_s3_class(profile_plot, c("canvasXpress", "htmlwidget"))
    profile_plot <- profilePlot(contrastDF, title = "BDL_vs_Sham", plotType = "ggplot",
                                sizeBySignificance = TRUE)
    expect_s3_class(profile_plot, c("gg", "ggplot"))
    ### contrast objects defualts (without sizeBySignificance and with geneSymCol)-
    gene_data <- t_obj1$geneData %>%
        dplyr::select(rgd_symbol)
    contrastDF <- merge(contrastDF, gene_data, by = 0, all = TRUE)
    rownames(contrastDF) <- contrastDF$Row.names
    contrastDF$Row.names <- NULL
    profile_plot <- profilePlot(contrastDF, title = "BDL_vs_Sham", geneSymCol = "rgd_symbol")
    expect_s3_class(profile_plot, c("canvasXpress", "htmlwidget"))
    profile_plot <- profilePlot(contrastDF, title = "BDL_vs_Sham", plotType = "ggplot",
                                geneSymCol = "rgd_symbol", symbolSize = c(4, 1, 4))
    expect_s3_class(profile_plot, c("gg", "ggplot"))
    ### testing lineFitType
    contrastDF <- t_obj1$BDL_vs_Sham
    profile_plot <- profilePlot(contrastDF, title = "BDL_vs_Sham", lineFitType = "lm",
                                xlab = "X label", ylab = "Y label")
    expect_s3_class(profile_plot, c("canvasXpress", "htmlwidget"))
    profile_plot <- profilePlot(contrastDF, title = "BDL_vs_Sham", lineFitType = "lm",
                                plotType = "ggplot", symbolSize = c(4, 1, 4),
                                xlab = "X label", ylab = "Y label")
    expect_s3_class(profile_plot, c("gg", "ggplot"))
    ## EXT1024_vs_BDL
    contrastDF <- t_obj1$EXT1024_vs_BDL
    profile_plot <- profilePlot(contrastDF, title = "EXT1024_vs_BDL")
    expect_s3_class(profile_plot, c("canvasXpress", "htmlwidget"))
    profile_plot <- profilePlot(contrastDF, title = "EXT1024_vs_BDL", plotType = "ggplot", symbolSize = c(4, 1, 4))
    expect_s3_class(profile_plot, c("gg", "ggplot"))
    ## Nint_vs_BDL
    contrastDF <- t_obj1$Nint_vs_BDL
    profile_plot <- profilePlot(contrastDF, title = "Nint_vs_BDL")
    expect_s3_class(profile_plot, c("canvasXpress", "htmlwidget"))
    profile_plot <- profilePlot(contrastDF, title = "Nint_vs_BDL", plotType = "ggplot", symbolSize = c(4, 1, 4))
    expect_s3_class(profile_plot, c("gg", "ggplot"))
    ## Sora_vs_BDL
    contrastDF <- t_obj1$Sora_vs_BDL
    profile_plot <- profilePlot(contrastDF, title = "Sora_vs_BDL")
    expect_s3_class(profile_plot, c("canvasXpress", "htmlwidget"))
    profile_plot <- profilePlot(contrastDF, title = "Sora_vs_BDL", plotType = "ggplot", symbolSize = c(4, 1, 4))
    expect_s3_class(profile_plot, c("gg", "ggplot"))
    # testing gene symbols
    contrastDF <- t_obj1$BDL_vs_Sham
    gene_data <- t_obj1$geneData %>%
        dplyr::select(rgd_symbol)
    contrastDF <- merge(contrastDF, gene_data, by = 0, all = TRUE)
    rownames(contrastDF) <- contrastDF$Row.names
    contrastDF$Row.names <- NULL
    sym_labels <- contrastDF[sample(nrow(contrastDF), 10), ]$rgd_symbol
    profile_plot <- profilePlot(contrastDF         = contrastDF,
                                title              = "BDL_vs_Sham with Symbols",
                                sizeBySignificance = TRUE,
                                geneSymCol         = "rgd_symbol",
                                geneSymLabels      = sym_labels,
                                footnote           = "This is footnote")
    expect_s3_class(profile_plot, c("canvasXpress","htmlwidget"))
    profile_plot <- profilePlot(contrastDF         = contrastDF,
                                title              = "BDL_vs_Sham with Symbols",
                                plotType           = "ggplot",
                                sizeBySignificance = TRUE,
                                geneSymCol         = "rgd_symbol",
                                geneSymLabels      = sym_labels,
                                footnote           = "This is footnote")
    expect_s3_class(profile_plot, c("gg","ggplot"))
    # testing asserts
    ## contrastDF
    msg <- "contrastDF must be specified as dataframe with LogIntensity and LogRatio columns and optionally a p-value"
    expect_error(profilePlot(),
                 regexp = msg)
    expect_error(profilePlot(NULL),
                 regexp = msg)
    expect_error(profilePlot(contrastDF %>% as.list()),
                 regexp = msg)
    expect_error(profilePlot(data.frame()),
                 regexp = msg)
    ## plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'."
    expect_warning(profilePlot(contrastDF, plotType = "xyz"),
                   regexp = msg)
    expect_warning(profilePlot(contrastDF, plotType = NULL),
                   regexp = msg)
    expect_warning(profilePlot(contrastDF, plotType = 123),
                   regexp = msg)
    expect_warning(profilePlot(contrastDF, plotType = c("canvasXpress", "ggplot")),
                   regexp = msg)
    ## logRatioCol
    msg <- "logRatioCol column not found in contrastDF."
    expect_error(profilePlot(contrastDF, logRatioCol = "xyz"),
                 regexp =  msg)
    expect_error(profilePlot(contrastDF, logRatioCol = NULL),
                 regexp =  msg)
    ## logIntCol
    msg <- "logIntCol column not found in contrastDF."
    expect_error(profilePlot(contrastDF, logRatioCol = "adj.P.Val", logIntCol = "xyz"),
                 regexp =  msg)
    expect_error(profilePlot(contrastDF, logRatioCol = "adj.P.Val", logIntCol = NULL),
                 regexp =  msg)
    ## pvalCol
    msg <- "pvalCol column not found in contrastDF."
    expect_error(profilePlot(contrastDF, logRatioCol = "adj.P.Val", pvalCol = "xyz"),
                 regexp = msg)
    expect_error(profilePlot(contrastDF, logRatioCol = "adj.P.Val", pvalCol = NULL),
                 regexp = msg)
    ## geneSymCol
    msg <- "geneSymCol column not found in contrastDF."
    expect_error(profilePlot(contrastDF, logRatioCol = "adj.P.Val", geneSymCol = NULL),
                 regexp = msg)
    expect_error(profilePlot(contrastDF, logRatioCol = "adj.P.Val", geneSymCol = "xyz"),
                 regexp = msg)
    ## pthreshold
    msg <- "pthreshold must be a singular numeric value. Assigning default value 0.01"
    expect_warning(profilePlot(contrastDF, logRatioCol = "adj.P.Val", pthreshold = NULL),
                   regexp = msg)
    expect_warning(profilePlot(contrastDF, logRatioCol = "adj.P.Val", pthreshold = "0.1"),
                   regexp = msg)
    expect_warning(profilePlot(contrastDF, logRatioCol = "adj.P.Val", pthreshold = c(0.1, 0.1)),
                   regexp = msg)
    ## foldChangeLines
    msg <- "foldChangeLines must be a singular numeric value. Assigning default value log2(1.5)"
    expect_warning(profilePlot(contrastDF, logRatioCol = "adj.P.Val", foldChangeLines = NULL),
                   regexp = msg,
                   fixed = TRUE)
    expect_warning(profilePlot(contrastDF, logRatioCol = "adj.P.Val", foldChangeLines = "0.1"),
                   regexp = msg,
                   fixed = TRUE)
    expect_warning(profilePlot(contrastDF, logRatioCol = "adj.P.Val", foldChangeLines = c(0.1, 0.1)),
                   regexp = msg,
                   fixed = TRUE)
    ## title
    msg <- "title must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(pPlot <- profilePlot(contrastDF, title = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", title = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(pPlot <- profilePlot(contrastDF, xlab = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", xlab = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(pPlot <- profilePlot(contrastDF, ylab = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", ylab = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## lineFitColor
    msg <- "lineFitColor must be a singular value of class character. Assigning default value 'goldenrod1'."
    expect_warning(pPlot <- profilePlot(contrastDF, lineFitColor = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", lineFitColor = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, lineFitColor = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", lineFitColor = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, lineFitColor = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", lineFitColor = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## symbolSize
    msg <- "symbolSize must be a vector of 3 integer values, at least 2 of them are different. Assigning default values 10, 4, 10."
    expect_warning(pPlot <- profilePlot(contrastDF, symbolSize = NULL),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", symbolSize = NULL),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, symbolSize = 1),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", symbolSize = 1),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, symbolSize = c(1, 2)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", symbolSize = c(1, 2)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, symbolSize = c("1", "2", "3")),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", symbolSize = c("1", "2", "3")),
                   regexp = msg)
    expect_warning(pPlot <- profilePlot(contrastDF, symbolSize = c(1, 1, 1)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", symbolSize = c(1, 1, 1)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, symbolSize = c(1, -1, 1)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", symbolSize = c(1, -1, 1)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## symbolShape
    msg <- "symbolShape must be a vector of 3 charcter values. Assigning default values 'circle', 'circle', 'circle'."
    expect_warning(pPlot <- profilePlot(contrastDF, symbolShape = NULL),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", symbolShape = NULL),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, symbolShape = 1),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", symbolShape = 1),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, symbolShape = c(1, 2)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", symbolShape = c(1, 2)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## symbolColor
    msg <- "symbolColor must be a vector of 3 character values. Assigning default values 'red3', 'grey25', 'deepskyblue4'."
    expect_warning(pPlot <- profilePlot(contrastDF, symbolColor = NULL),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", symbolColor = NULL),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, symbolColor = "black"),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", symbolColor = "black"),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, symbolColor = c("black", "grey0")),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", symbolColor = c("black", "grey0")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, symbolColor = c(1, 2, 3, 4)),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", symbolColor = c(1, 2, 3, 4)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## transparency
    msg <- "transparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.5'."
    expect_warning(pPlot <- profilePlot(contrastDF, transparency = "123"),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", transparency = "123"),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, transparency = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", transparency = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, transparency = NULL),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", transparency = NULL),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## referenceLine
    msg <- "referenceLine must be a singular value of class character or 'NULL' to disable. Assigning default value 'darkgoldenrod1'."
    expect_warning(pPlot <- profilePlot(contrastDF, referenceLine = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", referenceLine = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, referenceLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", referenceLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, referenceLine = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", referenceLine = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    msg <- "Color specified is not valid. Assigning default value 'darkgoldenrod1'."
    expect_warning(pPlot <- profilePlot(contrastDF, referenceLine = "abc"),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", referenceLine = "abc"),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## refLineThickness
    msg <- "refLineThickness must be a singular value of class numeric Assigning default value '1'."
    expect_warning(pPlot <- profilePlot(contrastDF, refLineThickness = "123"),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", refLineThickness = "123"),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, refLineThickness = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", refLineThickness = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, refLineThickness = NULL),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", refLineThickness = NULL),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, refLineThickness = -1),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", refLineThickness = -1),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## legendPosition
    msg <- "legendPosition must be one value from 'top', 'bottom', 'left', 'right', 'ne', 'se', 'nw', 'sw' or 'NULL' to disable. Assigning default value 'right'."
    expect_warning(pPlot <- profilePlot(contrastDF, legendPosition = 123),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", legendPosition = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, legendPosition = c("123", "456")),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", legendPosition = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, legendPosition = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", legendPosition = c(123, 456)),
                   regexp = msg)
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", legendPosition = "xyz"),
                   regexp = msg)
    ## footnote
    msg <- "footnote must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'."
    expect_warning(pPlot <- profilePlot(contrastDF, footnote = 123),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", footnote = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, footnote = c("123", "456")),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", footnote = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, footnote = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", footnote = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## footnote
    msg <- "footnote must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'."
    expect_warning(pPlot <- profilePlot(contrastDF, footnote = 123),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", footnote = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, footnote = c("123", "456")),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", footnote = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, footnote = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", footnote = c(123, 456)),
                   regexp = msg)
    ## sizeBySignificance
    msg <- "sizeBySignificance must be a singular logical value. Assigning default value FALSE"
    expect_warning(pPlot <- profilePlot(contrastDF, sizeBySignificance = "123"),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", sizeBySignificance = "123"),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, sizeBySignificance = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", sizeBySignificance = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, sizeBySignificance = NULL),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", sizeBySignificance = NULL),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## lineFitType
    msg <- "lineFitType must be one of 'glm', 'lm', 'loess', 'gam' or NULL to disable. Assigning default value 'loess'."
    expect_warning(pPlot <- profilePlot(contrastDF, lineFitType = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", lineFitType = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, lineFitType = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", lineFitType = c("loess", "loess")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, lineFitType = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", lineFitType = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## lineFitColor
    msg <- "lineFitColor must be a singular value of class character. Assigning default value 'goldenrod1'."
    expect_warning(pPlot <- profilePlot(contrastDF, lineFitColor = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", lineFitColor = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, lineFitColor = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", lineFitColor = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(contrastDF, lineFitColor = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", lineFitColor = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    msg <- "Color specified is not valid. Assigning default value 'goldenrod1'."
    expect_warning(pPlot <- profilePlot(contrastDF, lineFitColor = "abc"),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(contrastDF, plotType = "ggplot", lineFitColor = "abc"),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
})
