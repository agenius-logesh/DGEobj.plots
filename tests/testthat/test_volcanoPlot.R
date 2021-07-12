context("DGEobj.plots - tests for volcanoPlot.R functions")


test_that("volcanoPlot.R: volcanoPlot()", {
    # testing contrast objects defualts (sizeByIntensity=TRUE and no geneSymCol)
    ## BDL_vs_Sham
    volcanoPlot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham")
    expect_s3_class(volcanoPlot, c("canvasXpress", "htmlwidget"))
    volcanoPlot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", plotType = "ggplot")
    expect_s3_class(volcanoPlot, c("gg", "ggplot"))
    ### contrast objects defualts (with sizeByIntensity=False with symbolSize and no geneSymCol)-
    volcanoPlot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham",
                               sizeByIntensity = FALSE, symbolSize = c(8,4,8))
    expect_s3_class(volcanoPlot, c("canvasXpress", "htmlwidget"))
    volcanoPlot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", plotType = "ggplot",
                               sizeByIntensity = FALSE, symbolSize = c(8,4,8))
    expect_s3_class(volcanoPlot, c("gg", "ggplot"))
    ### contrast objects defualts (without sizeByIntensity=False and with geneSymCol)-
    volcanoPlot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", geneSymCol = "rgd_symbol")
    expect_s3_class(volcanoPlot, c("canvasXpress", "htmlwidget"))
    volcanoPlot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", plotType = "ggplot",
                               geneSymCol = "rgd_symbol")
    expect_s3_class(volcanoPlot, c("gg", "ggplot"))
    ## EXT1024_vs_BDL
    volcanoPlot <- volcanoPlot(DGEdata = t_obj1, contrast = "EXT1024_vs_BDL", title = "EXT1024_vs_BDL")
    expect_s3_class(volcanoPlot, c("canvasXpress", "htmlwidget"))
    volcanoPlot <- volcanoPlot(DGEdata = t_obj1, contrast = "EXT1024_vs_BDL", title = "EXT1024_vs_BDL", plotType = "ggplot",
                                    sizeByIntensity = FALSE, symbolSize = c(8,4,8))
    expect_s3_class(volcanoPlot, c("gg", "ggplot"))
    ## Nint_vs_BDL
    volcanoPlot <- volcanoPlot(DGEdata = t_obj1, contrast = "Nint_vs_BDL", title = "Nint_vs_BDL")
    expect_s3_class(volcanoPlot, c("canvasXpress", "htmlwidget"))
    volcanoPlot <- volcanoPlot(DGEdata = t_obj1, contrast = "Nint_vs_BDL", title = "Nint_vs_BDL", plotType = "ggplot",
                                    sizeByIntensity = FALSE,symbolSize = c(8,4,8))
    expect_s3_class(volcanoPlot, c("gg", "ggplot"))
    ## Sora_vs_BDL
    volcanoPlot <- volcanoPlot(DGEdata = t_obj1, contrast = "Sora_vs_BDL", title = "Sora_vs_BDL",
                               sizeByIntensity = FALSE,symbolSize = c(8,4,8))
    expect_s3_class(volcanoPlot, c("canvasXpress", "htmlwidget"))
    volcanoPlot <- volcanoPlot(DGEdata = t_obj1, contrast = "Sora_vs_BDL", title = "Sora_vs_BDL", plotType = "ggplot",
                                    sizeByIntensity = FALSE,symbolSize = c(8,4,8))
    expect_s3_class(volcanoPlot, c("gg", "ggplot"))
    # testing gene symbols
    contrastDF <- DGEobj::getItem(t_obj1,"BDL_vs_Sham")
    gene_data <- DGEobj::getItem(t_obj1,"geneData") %>%
        dplyr::select(rgd_symbol)
    contrastDF <- merge(contrastDF, gene_data, by = 0, all = TRUE)
    rownames(contrastDF) <- contrastDF$Row.names
    contrastDF$Row.names <- NULL
    sym_labels <- contrastDF[sample(nrow(contrastDF), 10), ]$rgd_symbol
    volcanoPlot <- volcanoPlot(DGEdata            = t_obj1,
                               contrast           = "BDL_vs_Sham",
                               title              = "BDL_vs_Sham with Symbols",
                               geneSymCol         = "rgd_symbol",
                               geneSymLabels      = sym_labels,
                               footnote           = "This is footnote")
    expect_s3_class(volcanoPlot, c("canvasXpress","htmlwidget"))
    volcanoPlot <- volcanoPlot(DGEdata            = t_obj1,
                               contrast           = "BDL_vs_Sham",
                               title              = "BDL_vs_Sham with Symbols",
                               plotType           = "ggplot",
                               geneSymCol         = "rgd_symbol",
                               geneSymLabels      = sym_labels,
                               footnote           = "This is footnote")
    expect_s3_class(volcanoPlot, c("gg","ggplot"))
    # testing asserts
    ## DGEdata
    msg <- "DGEdata must be specified as class of DGEobj."
    expect_error(volcanoPlot(),
                 regexp = msg)
    expect_error(volcanoPlot(NULL),
                 regexp = msg)
    expect_error(volcanoPlot(123),
                 regexp = msg)
    expect_error(volcanoPlot("123"),
                 regexp = msg)
    expect_error(volcanoPlot("xyz"),
                 regexp = msg)
    ## DGEdata
    DGEdata <- t_obj1
    ## contrast
    msg = "contrast to be a singular value of class character and must be one from DGEdata with LogIntensity and LogRatio columns and optionally a p-value."
    expect_error(volcanoPlot(DGEdata),
                 regexp = msg)
    expect_error(volcanoPlot(DGEdata, contrast = NULL),
                 regexp = msg)
    expect_error(volcanoPlot(DGEdata, contrast = "123"),
                 regexp = msg)
    expect_error(volcanoPlot(DGEdata, contrast = 123),
                 regexp = msg)
    expect_error(volcanoPlot(DGEdata, contrast = "xyz"),
                 regexp = msg)
    #contrast
    contrast <- "BDL_vs_Sham"
    ## plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'."
    expect_warning(volcanoPlot(DGEdata, contrast, plotType = "xyz"),
                   regexp = msg)
    expect_warning(volcanoPlot(DGEdata, contrast, plotType = NULL),
                   regexp = msg)
    expect_warning(volcanoPlot(DGEdata, contrast, plotType = 123),
                   regexp = msg)
    expect_warning(volcanoPlot(DGEdata, contrast, plotType = c("canvasXpress", "ggplot")),
                   regexp = msg)
    ## logRatioCol
    msg <- "logRatioCol column not found in contrast data."
    expect_error(volcanoPlot(DGEdata, contrast, logRatioCol = "xyz"),
                 regexp =  msg)
    expect_error(volcanoPlot(DGEdata, contrast, logRatioCol = NULL),
                 regexp =  msg)
    ## logIntCol
    msg <- "logIntCol column not found in contrast data."
    expect_error(volcanoPlot(DGEdata, contrast, logIntCol = "xyz"),
                 regexp =  msg)
    expect_error(volcanoPlot(DGEdata, contrast, logIntCol = NULL),
                 regexp =  msg)
    ## pvalCol
    msg <- "pvalCol column not found in contrast data."
    expect_error(volcanoPlot(DGEdata, contrast, pvalCol = "xyz"),
                 regexp = msg)
    expect_error(volcanoPlot(DGEdata, contrast, pvalCol = NULL),
                 regexp = msg)
    ## geneSymCol
    msg <- "geneSymCol column not found in geneData from DGEdata."
    expect_error(volcanoPlot(DGEdata, contrast, geneSymCol = NULL),
                 regexp = msg)
    expect_error(volcanoPlot(DGEdata, contrast, geneSymCol = "xyz"),
                 regexp = msg)
    ## pthreshold
    msg <- "pthreshold must be a singular numeric value. Assigning default value 0.01"
    expect_warning(volcanoPlot(DGEdata, contrast, pthreshold = NULL),
                   regexp = msg)
    expect_warning(volcanoPlot(DGEdata, contrast, pthreshold = "0.1"),
                   regexp = msg)
    expect_warning(volcanoPlot(DGEdata, contrast, pthreshold = c(0.1, 0.1)),
                   regexp = msg)
    ## foldChangeLines
    msg <- "foldChangeLines must be a singular numeric value. Assigning default value log2(1.5)"
    expect_warning(volcanoPlot(DGEdata, contrast, foldChangeLines = NULL),
                   regexp = msg,
                   fixed = TRUE)
    expect_warning(volcanoPlot(DGEdata, contrast, foldChangeLines = "0.1"),
                   regexp = msg,
                   fixed = TRUE)
    expect_warning(volcanoPlot(DGEdata, contrast, foldChangeLines = c(0.1, 0.1)),
                   regexp = msg,
                   fixed = TRUE)
    ## title
    msg <- "title must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, title = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", title = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, xlab = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", xlab = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, ylab = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", ylab = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))

    ## symbolSize
    msg <- "symbolSize must be a vector of 3 integer values, at least 2 of them are different. Assigning default values 10, 4, 10."
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, symbolSize = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", symbolSize = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, symbolSize = 1),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", symbolSize = 1),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, symbolSize = c(1, 2)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", symbolSize = c(1, 2)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, symbolSize = c("1", "2", "3")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", symbolSize = c("1", "2", "3")),
                   regexp = msg)
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, symbolSize = c(1, 1, 1)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", symbolSize = c(1, 1, 1)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, symbolSize = c(1, -1, 1)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", symbolSize = c(1, -1, 1)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## symbolShape
    msg <- "symbolShape must be a vector of 3 charcter values. Assigning default values 'circle', 'circle', 'circle'."
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, symbolShape = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", symbolShape = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, symbolShape = 1),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", symbolShape = 1),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, symbolShape = c(1, 2)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", symbolShape = c(1, 2)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## symbolColor
    msg <- "symbolColor must be a vector of 3 character values. Assigning default values 'red3', 'grey25', 'deepskyblue4'."
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, symbolColor = NULL),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", symbolColor = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, symbolColor = "black"),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", symbolColor = "black"),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, symbolColor = c("black", "grey0")),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", symbolColor = c("black", "grey0")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, symbolColor = c(1, 2, 3, 4)),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", symbolColor = c(1, 2, 3, 4)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## transparency
    msg <- "transparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.5'."
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, transparency = "123"),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", transparency = "123"),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, transparency = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", transparency = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, transparency = NULL),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", transparency = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## pthresholdLine
    msg <- "pthresholdLine must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'."
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, pthresholdLine = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", pthresholdLine = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, pthresholdLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", pthresholdLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, pthresholdLine = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", pthresholdLine = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    msg <- "Color specified is not valid. Assigning default value 'NULL'."
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, pthresholdLine = "abc"),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", pthresholdLine = "abc"),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## refLineThickness
    msg <- "refLineThickness must be a singular value of class numeric Assigning default value '2'."
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, refLineThickness = "123"),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", refLineThickness = "123"),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, refLineThickness = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", refLineThickness = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, refLineThickness = NULL),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", refLineThickness = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, refLineThickness = -1),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", refLineThickness = -1),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## legendPosition
    msg <- "legendPosition must be one value from 'top', 'bottom', 'left', 'right', 'topRight', 'bottomRight', 'topLeft', 'bottomLeft' or 'NULL' to disable. Assigning default value 'right'."
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, legendPosition = 123),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", legendPosition = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, legendPosition = c("123", "456")),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", legendPosition = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, legendPosition = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", legendPosition = c(123, 456)),
                   regexp = msg)
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", legendPosition = "xyz"),
                   regexp = msg)
    ## footnote
    msg <- "footnote must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'."
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, footnote = 123),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", footnote = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, footnote = c("123", "456")),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", footnote = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, footnote = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", footnote = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    ## footnote
    msg <- "footnote must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'."
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, footnote = 123),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", footnote = 123),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, footnote = c("123", "456")),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", footnote = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, footnote = c(123, 456)),
                   regexp = msg)
    #expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", footnote = c(123, 456)),
                   regexp = msg)
    ## sizeByIntensity
    msg <- "sizeByIntensity must be a singular logical value. Assigning default value TRUE"
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, sizeByIntensity = "123"),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", sizeByIntensity = "123"),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, sizeByIntensity = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", sizeByIntensity = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, sizeByIntensity = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(volcanoPlot <- volcanoPlot(DGEdata, contrast, plotType = "ggplot", sizeByIntensity = NULL),
                   regexp = msg)
    expect_s3_class(volcanoPlot , c("gg", "ggplot"))
})
