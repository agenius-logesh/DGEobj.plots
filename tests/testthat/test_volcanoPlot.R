context("DGEobj.plots - tests for volcanoPlot.R functions")


test_that("volcanoPlot.R: volcanoPlot()", {
    # testing contrast objects defualts (sizeByIntensity=TRUE and no geneSymCol)
    ## BDL_vs_Sham
    contrastDF <- t_obj1$BDL_vs_Sham
    volcanoPlot <- volcanoPlot(contrastDF, title = "BDL_vs_Sham")
    expect_s3_class(volcanoPlot, c("canvasXpress", "htmlwidget"))
    volcanoPlot <- volcanoPlot(contrastDF, title = "BDL_vs_Sham", plotType = "ggplot")
    expect_s3_class(volcanoPlot, c("gg", "ggplot"))
    ### contrast objects defualts (with sizeByIntensity=False with symbolSize and no geneSymCol)-
    volcanoPlot <- volcanoPlot(contrastDF, title = "BDL_vs_Sham", sizeByIntensity = FALSE, symbolSize = c(8,4,8))
    expect_s3_class(volcanoPlot, c("canvasXpress", "htmlwidget"))
    volcanoPlot <- volcanoPlot(contrastDF, title = "BDL_vs_Sham", plotType = "ggplot",
                               sizeByIntensity = FALSE, symbolSize = c(8,4,8))
    expect_s3_class(volcanoPlot, c("gg", "ggplot"))
    ### contrast objects defualts (without sizeByIntensity=False and with geneSymCol)-
    gene_data <- t_obj1$geneData %>%
        dplyr::select(rgd_symbol)
    contrastDF <- merge(contrastDF, gene_data, by = 0, all = TRUE)
    rownames(contrastDF) <- contrastDF$Row.names
    contrastDF$Row.names <- NULL
    volcanoPlot <- volcanoPlot(contrastDF, title = "BDL_vs_Sham", geneSymCol = "rgd_symbol")
    expect_s3_class(volcanoPlot, c("canvasXpress", "htmlwidget"))
    volcanoPlot <- volcanoPlot(contrastDF, title = "BDL_vs_Sham", plotType = "ggplot",
                               geneSymCol = "rgd_symbol")
    expect_s3_class(volcanoPlot, c("gg", "ggplot"))
    ## EXT1024_vs_BDL
    contrastDF <- t_obj1$EXT1024_vs_BDL
    volcanoPlot <- volcanoPlot(contrastDF, title = "EXT1024_vs_BDL")
    expect_s3_class(volcanoPlot, c("canvasXpress", "htmlwidget"))
    volcanoPlot <- volcanoPlot(contrastDF, title = "EXT1024_vs_BDL", plotType = "ggplot",
                                    sizeByIntensity = FALSE, symbolSize = c(8,4,8))
    expect_s3_class(volcanoPlot, c("gg", "ggplot"))
    ## Nint_vs_BDL
    contrastDF <- t_obj1$Nint_vs_BDL
    volcanoPlot <- volcanoPlot(contrastDF, title = "Nint_vs_BDL")
    expect_s3_class(volcanoPlot, c("canvasXpress", "htmlwidget"))
    volcanoPlot <- volcanoPlot(contrastDF, title = "Nint_vs_BDL", plotType = "ggplot",
                                    sizeByIntensity = FALSE,symbolSize = c(8,4,8))
    expect_s3_class(volcanoPlot, c("gg", "ggplot"))
    ## Sora_vs_BDL
    contrastDF <- t_obj1$Sora_vs_BDL
    volcanoPlot <- volcanoPlot(contrastDF, title = "Sora_vs_BDL",
                               sizeByIntensity = FALSE,symbolSize = c(8,4,8))
    expect_s3_class(volcanoPlot, c("canvasXpress", "htmlwidget"))
    volcanoPlot <- volcanoPlot(contrastDF, title = "Sora_vs_BDL", plotType = "ggplot",
                                    sizeByIntensity = FALSE,symbolSize = c(8,4,8))
    expect_s3_class(volcanoPlot, c("gg", "ggplot"))
    # testing gene symbols
    contrastDF <- t_obj1$BDL_vs_Sham
    gene_data <- t_obj1$geneData %>%
        dplyr::select(rgd_symbol)
    contrastDF <- merge(contrastDF, gene_data, by = 0, all = TRUE)
    rownames(contrastDF) <- contrastDF$Row.names
    contrastDF$Row.names <- NULL
    sym_labels <- contrastDF[sample(nrow(contrastDF), 10), ]$rgd_symbol
    volcanoPlot <- volcanoPlot(contrastDF         = contrastDF,
                               title              = "BDL_vs_Sham with Symbols",
                               geneSymCol         = "rgd_symbol",
                               geneSymLabels      = sym_labels,
                               footnote           = "This is footnote")
    expect_s3_class(volcanoPlot, c("canvasXpress","htmlwidget"))
    volcanoPlot <- volcanoPlot(contrastDF         = contrastDF,
                               title              = "BDL_vs_Sham with Symbols",
                               plotType           = "ggplot",
                               geneSymCol         = "rgd_symbol",
                               geneSymLabels      = sym_labels,
                               footnote           = "This is footnote")
    expect_s3_class(volcanoPlot, c("gg","ggplot"))
    # testing asserts
    ## contrastDF
    msg <- "contrastDF must be specified as dataframe with LogIntensity and LogRatio columns and optionally a p-value"
    expect_error(volcanoPlot(),
                 regexp = msg)
    expect_error(volcanoPlot(NULL),
                 regexp = msg)
    expect_error(volcanoPlot(contrastDF %>% as.list()),
                 regexp = msg)
    expect_error(volcanoPlot(data.frame()),
                 regexp = msg)
    ## plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'."
    expect_warning(volcanoPlot(contrastDF, plotType = "xyz"),
                   regexp = msg)
    expect_warning(volcanoPlot(contrastDF, plotType = NULL),
                   regexp = msg)
    expect_warning(volcanoPlot(contrastDF, plotType = 123),
                   regexp = msg)
    expect_warning(volcanoPlot(contrastDF, plotType = c("canvasXpress", "ggplot")),
                   regexp = msg)
    ## logRatioCol
    msg <- "logRatioCol column not found in contrastDF."
    expect_error(volcanoPlot(contrastDF, logRatioCol = "xyz"),
                 regexp =  msg)
    expect_error(volcanoPlot(contrastDF, logRatioCol = NULL),
                 regexp =  msg)
    ## logIntCol
    msg <- "logIntCol column not found in contrastDF."
    expect_error(volcanoPlot(contrastDF, logIntCol = "xyz"),
                 regexp =  msg)
    expect_error(volcanoPlot(contrastDF, logIntCol = NULL),
                 regexp =  msg)
    ## pvalCol
    msg <- "pvalCol column not found in contrastDF."
    expect_error(volcanoPlot(contrastDF, pvalCol = "xyz"),
                 regexp = msg)
    expect_error(volcanoPlot(contrastDF, pvalCol = NULL),
                 regexp = msg)
    ## geneSymCol
    msg <- "geneSymCol column not found in contrastDF."
    expect_error(volcanoPlot(contrastDF, geneSymCol = NULL),
                 regexp = msg)
    expect_error(volcanoPlot(contrastDF, geneSymCol = "xyz"),
                 regexp = msg)
    ## pthreshold
    msg <- "pthreshold must be a singular numeric value. Assigning default value 0.01"
    expect_warning(volcanoPlot(contrastDF, pthreshold = NULL),
                   regexp = msg)
    expect_warning(volcanoPlot(contrastDF, pthreshold = "0.1"),
                   regexp = msg)
    expect_warning(volcanoPlot(contrastDF, pthreshold = c(0.1, 0.1)),
                   regexp = msg)
    ## foldChangeLines
    msg <- "foldChangeLines must be a singular numeric value. Assigning default value log2(1.5)"
    expect_warning(volcanoPlot(contrastDF, foldChangeLines = NULL),
                   regexp = msg,
                   fixed = TRUE)
    expect_warning(volcanoPlot(contrastDF, foldChangeLines = "0.1"),
                   regexp = msg,
                   fixed = TRUE)
    expect_warning(volcanoPlot(contrastDF, foldChangeLines = c(0.1, 0.1)),
                   regexp = msg,
                   fixed = TRUE)
    ## title
    msg <- "title must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, title = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", title = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, xlab = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", xlab = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, ylab = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", ylab = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))

    ## symbolSize
    msg <- "symbolSize must be a vector of 3 integer values, at least 2 of them are different. Assigning default values 10, 4, 10."
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, symbolSize = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", symbolSize = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, symbolSize = 1),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", symbolSize = 1),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, symbolSize = c(1, 2)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", symbolSize = c(1, 2)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, symbolSize = c("1", "2", "3")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", symbolSize = c("1", "2", "3")),
                   regexp = msg)
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, symbolSize = c(1, 1, 1)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", symbolSize = c(1, 1, 1)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, symbolSize = c(1, -1, 1)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", symbolSize = c(1, -1, 1)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## symbolShape
    msg <- "symbolShape must be a vector of 3 charcter values. Assigning default values 'circle', 'circle', 'circle'."
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, symbolShape = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", symbolShape = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, symbolShape = 1),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", symbolShape = 1),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, symbolShape = c(1, 2)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", symbolShape = c(1, 2)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## symbolColor
    msg <- "symbolColor must be a vector of 3 character values. Assigning default values 'red3', 'grey25', 'deepskyblue4'."
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, symbolColor = NULL),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", symbolColor = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, symbolColor = "black"),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", symbolColor = "black"),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, symbolColor = c("black", "grey0")),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", symbolColor = c("black", "grey0")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, symbolColor = c(1, 2, 3, 4)),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", symbolColor = c(1, 2, 3, 4)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## transparency
    msg <- "transparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.5'."
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, transparency = "123"),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", transparency = "123"),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, transparency = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", transparency = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, transparency = NULL),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", transparency = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## pthresholdLine
    msg <- "pthresholdLine must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'."
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, pthresholdLine = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", pthresholdLine = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, pthresholdLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", pthresholdLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, pthresholdLine = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", pthresholdLine = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    msg <- "Color specified is not valid. Assigning default value 'NULL'."
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, pthresholdLine = "abc"),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", pthresholdLine = "abc"),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## refLineThickness
    msg <- "refLineThickness must be a singular value of class numeric Assigning default value '2'."
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, refLineThickness = "123"),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", refLineThickness = "123"),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, refLineThickness = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", refLineThickness = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, refLineThickness = NULL),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", refLineThickness = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, refLineThickness = -1),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", refLineThickness = -1),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## legendPosition
    msg <- "legendPosition must be one value from 'top', 'bottom', 'left', 'right', 'topRight', 'bottomRight', 'topLeft', 'bottomLeft' or 'NULL' to disable. Assigning default value 'right'."
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, legendPosition = 123),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", legendPosition = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, legendPosition = c("123", "456")),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", legendPosition = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, legendPosition = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", legendPosition = c(123, 456)),
                   regexp = msg)
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", legendPosition = "xyz"),
                   regexp = msg)
    ## footnote
    msg <- "footnote must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'."
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, footnote = 123),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", footnote = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, footnote = c("123", "456")),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", footnote = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, footnote = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", footnote = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## footnote
    msg <- "footnote must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'."
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, footnote = 123),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", footnote = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, footnote = c("123", "456")),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", footnote = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, footnote = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", footnote = c(123, 456)),
                   regexp = msg)
    ## sizeByIntensity
    msg <- "sizeByIntensity must be a singular logical value. Assigning default value TRUE"
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, sizeByIntensity = "123"),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", sizeByIntensity = "123"),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, sizeByIntensity = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", sizeByIntensity = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, sizeByIntensity = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(contrastDF, plotType = "ggplot", sizeByIntensity = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
})
