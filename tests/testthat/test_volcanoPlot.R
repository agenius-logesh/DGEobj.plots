context("DGEobj.plots - tests for volcanoPlot.R functions")


test_that("volcanoPlot.R: volcanoPlot()", {
    # testing contrast objects defualts (sizeByIntensity=TRUE and no geneNameCol)
    ## BDL_vs_Sham
    volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham")
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", plotType = "ggplot")
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    ### contrast objects defualts (with sizeByIntensity=False with symbolSize and no geneNameCol)-
    volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham",
                               sizeByIntensity = FALSE)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", plotType = "ggplot",
                               sizeByIntensity = FALSE)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    ### contrast objects defualts (without sizeByIntensity=False and with geneNameCol)-
    volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", geneNameCol = "rgd_symbol")
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", plotType = "ggplot",
                               geneNameCol = "rgd_symbol")
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    ## EXT1024_vs_BDL
    volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "EXT1024_vs_BDL", title = "EXT1024_vs_BDL")
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "EXT1024_vs_BDL", title = "EXT1024_vs_BDL", plotType = "ggplot",
                                    sizeByIntensity = FALSE)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    # testing gene symbols
    contrastDF <- DGEobj::getItem(t_obj1,"BDL_vs_Sham")
    gene_data <- DGEobj::getItem(t_obj1,"geneData") %>%
        dplyr::select(rgd_symbol)
    contrastDF <- merge(contrastDF, gene_data, by = 0, all = TRUE)
    rownames(contrastDF) <- contrastDF$Row.names
    contrastDF$Row.names <- NULL
    sym_labels <- contrastDF[sample(nrow(contrastDF), 10), ]$rgd_symbol
    volcano_plot <- volcanoPlot(DGEdata            = t_obj1,
                               contrast           = "BDL_vs_Sham",
                               title              = "BDL_vs_Sham with Symbols",
                               geneNameCol         = "rgd_symbol")
    expect_s3_class(volcano_plot, c("canvasXpress","htmlwidget"))
    volcano_plot <- volcanoPlot(DGEdata            = t_obj1,
                               contrast           = "BDL_vs_Sham",
                               title              = "BDL_vs_Sham with Symbols",
                               plotType           = "ggplot",
                               geneNameCol         = "rgd_symbol")
    expect_s3_class(volcano_plot, c("gg","ggplot"))
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
    ## contrast
    msg = "contrast to be a singular value of class character and must be one of the topTables in DGEdata."
    expect_error(volcanoPlot(DGEdata = t_obj1),
                 regexp = msg)
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = NULL),
                 regexp = msg)
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = "123"),
                 regexp = msg)
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = 123),
                 regexp = msg)
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = c(123,234)),
                 regexp = msg)
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = "xyz"),
                 regexp = msg)
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = c("xyz","abc")),
                 regexp = msg)
    #contrast
    contrast <- "BDL_vs_Sham"
    ## plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'."
    expect_warning(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "xyz"),
                   regexp = msg)
    expect_warning(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = NULL),
                   regexp = msg)
    expect_warning(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = 123),
                   regexp = msg)
    expect_warning(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = c("canvasXpress", "ggplot")),
                   regexp = msg)
    ## logRatioCol
    msg <- "logRatioCol column not found in contrast data."
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "xyz"),
                 regexp =  msg)
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = c("xyz","abc")),
                 regexp =  msg)
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = NULL),
                 regexp =  msg)
    ## logIntCol
    msg <- "logIntCol column not found in contrast data."
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", logIntCol = "xyz"),
                 regexp =  msg)
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", logIntCol = c("xyz","abc")),
                 regexp =  msg)
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", logIntCol = NULL),
                 regexp =  msg)
    ## pvalCol
    msg <- "pvalCol column not found in contrast data."
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", pvalCol = "xyz"),
                 regexp = msg)
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", pvalCol = c("xyz","abc")),
                 regexp =  msg)
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", pvalCol = NULL),
                 regexp = msg)
    ## geneNameCol
    msg <- "geneNameCol column not found in geneData from DGEdata."
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", geneNameCol = NULL),
                 regexp = msg)
    expect_error(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", geneNameCol = "xyz"),
                 regexp = msg)
    ## pthreshold
    msg <- "pthreshold must be a singular numeric value. Assigning default value 0.01"
    expect_warning(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", pthreshold = NULL),
                   regexp = msg)
    expect_warning(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", pthreshold = "0.1"),
                   regexp = msg)
    expect_warning(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", pthreshold = c(0.1, 0.1)),
                   regexp = msg)
    ## foldChangeLines
    msg <- "foldChangeLines must be a singular numeric value. Assigning default value log2(1.5)"
    expect_warning(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", foldChangeLines = NULL),
                   regexp = msg,
                   fixed = TRUE)
    expect_warning(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", foldChangeLines = "0.1"),
                   regexp = msg,
                   fixed = TRUE)
    expect_warning(volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", foldChangeLines = c(0.1, 0.1)),
                   regexp = msg,
                   fixed = TRUE)
    ## title
    msg <- "title must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = 123),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", title = 123),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    ## xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", xlab = 123),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", xlab = 123),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    ## ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", ylab = 123),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", ylab = 123),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))

    ## pthresholdLine
    msg <- "pthresholdLine must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'."
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", pthresholdLine = 123),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", pthresholdLine = 123),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", pthresholdLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", pthresholdLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", pthresholdLine = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", pthresholdLine = c(123, 456)),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    msg <- "Color specified is not valid. Assigning default value 'NULL'."
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", pthresholdLine = "abc"),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", pthresholdLine = "abc"),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))

    ## sizeByIntensity
    msg <- "sizeByIntensity must be a singular logical value. Assigning default value TRUE"
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", sizeByIntensity = "123"),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", sizeByIntensity = "123"),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", sizeByIntensity = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", sizeByIntensity = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", sizeByIntensity = NULL),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("canvasXpress", "htmlwidget"))
    expect_warning(volcano_plot <- volcanoPlot(DGEdata = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", sizeByIntensity = NULL),
                   regexp = msg)
    expect_s3_class(volcano_plot, c("gg", "ggplot"))
})
