context("DGEobj.plots - tests for profilePlot.R functions")


test_that("profilePlot.R: profilePlot()", {
    contrasts <- c("BDL_vs_Sham", "EXT1024_vs_BDL")
    skip_if(!all(contrasts %in% names(DGEobj::getType(t_obj1,"topTable"))))

    # testing contrast objects defualts (no sizeBySignificance and no geneNameCol)
    ## BDL_vs_Sham
    profile_plot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham")
    expect_s3_class(profile_plot, c("canvasXpress", "htmlwidget"))
    profile_plot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", plotType = "ggplot")
    expect_s3_class(profile_plot, c("gg", "ggplot"))
    ### contrast objects defualts (with sizeBySignificance and no geneNameCol)-
    profile_plot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", sizeBySignificance = TRUE)
    expect_s3_class(profile_plot, c("canvasXpress", "htmlwidget"))
    profile_plot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", plotType = "ggplot",
                                sizeBySignificance = TRUE)
    expect_s3_class(profile_plot, c("gg", "ggplot"))

    ### contrast objects defualts (without sizeBySignificance and with geneNameCol)-
    profile_plot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", geneNameCol = "rgd_symbol")
    expect_s3_class(profile_plot, c("canvasXpress", "htmlwidget"))
    profile_plot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", plotType = "ggplot",
                                geneNameCol = "rgd_symbol")
    expect_s3_class(profile_plot, c("gg", "ggplot"))

    ### testing lineFitType
    profile_plot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", lineFitType = "lm",
                                xlab = "X label", ylab = "Y label")
    expect_s3_class(profile_plot, c("canvasXpress", "htmlwidget"))
    profile_plot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", title = "BDL_vs_Sham", lineFitType = "lm",
                                plotType = "ggplot", xlab = "X label", ylab = "Y label")
    expect_s3_class(profile_plot, c("gg", "ggplot"))

    ## EXT1024_vs_BDL
    profile_plot <- profilePlot(dgeObj = t_obj1, contrast = "EXT1024_vs_BDL", title = "EXT1024_vs_BDL")
    expect_s3_class(profile_plot, c("canvasXpress", "htmlwidget"))
    profile_plot <- profilePlot(dgeObj = t_obj1, contrast = "EXT1024_vs_BDL", title = "EXT1024_vs_BDL", plotType = "ggplot")
    expect_s3_class(profile_plot, c("gg", "ggplot"))

    # testing gene symbols
    profile_plot <- profilePlot(dgeObj            = t_obj1,
                                contrast           = "BDL_vs_Sham",
                                title              = "BDL_vs_Sham with Symbols",
                                sizeBySignificance = TRUE,
                                geneNameCol         = "rgd_symbol")
    expect_s3_class(profile_plot, c("canvasXpress","htmlwidget"))
    profile_plot <- profilePlot(dgeObj            = t_obj1,
                                contrast           = "BDL_vs_Sham",
                                title              = "BDL_vs_Sham with Symbols",
                                plotType           = "ggplot",
                                sizeBySignificance = TRUE,
                                geneNameCol         = "rgd_symbol")
    expect_s3_class(profile_plot, c("gg","ggplot"))
    # testing asserts
    ## dgeObj
    msg <- "dgeObj must be specified and must belong to DGEobj"
    expect_error(profilePlot(),
                 regexp = msg)
    expect_error(profilePlot(NULL),
                 regexp = msg)
    expect_error(profilePlot(123),
                 regexp = msg)
    expect_error(profilePlot("123"),
                 regexp = msg)
    expect_error(profilePlot("xyz"),
                 regexp = msg)
    #topTables
    #removing toptable items
    t_obj <- DGEobj::rmItem(t_obj1, "BDL_vs_Sham")
    t_obj <- DGEobj::rmItem(t_obj, "EXT1024_vs_BDL")
    t_obj <- DGEobj::rmItem(t_obj, "Nint_vs_BDL")
    t_obj <- DGEobj::rmItem(t_obj, "Sora_vs_BDL")

    msg = "Plot cannot be rendered as dgeObj has no topTables."
    expect_error(profilePlot(dgeObj = t_obj),
                 regexp = msg)
    #geneData
    #removing geneData
    t_obj <- DGEobj::rmItem(t_obj1, "geneData")

    msg = "dgeObj must have exactly one geneData item."
    expect_error(profilePlot(dgeObj = t_obj, contrast = "BDL_vs_Sham", geneNameCol = "rgd_symbol"),
                 regexp = msg)

    ## contrast
    msg = "contrast to be a singular value of class character and must be one of the topTables in dgeObj."
    expect_error(profilePlot(dgeObj = t_obj1),
                 regexp = msg)
    expect_error(profilePlot(dgeObj = t_obj1, contrast = NULL),
                 regexp = msg)
    expect_error(profilePlot(dgeObj = t_obj1, contrast = "123"),
                 regexp = msg)
    expect_error(profilePlot(dgeObj = t_obj1, contrast = 123),
                 regexp = msg)
    expect_error(profilePlot(dgeObj = t_obj1, contrast = c(123,234)),
                 regexp = msg)
    expect_error(profilePlot(dgeObj = t_obj1, contrast = "xyz"),
                 regexp = msg)
    expect_error(profilePlot(dgeObj = t_obj1, contrast = c("xyz","abc")),
                 regexp = msg)
    ## plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'."
    expect_warning(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "xyz"),
                   regexp = msg)
    expect_warning(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = NULL),
                   regexp = msg)
    expect_warning(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = 123),
                   regexp = msg)
    expect_warning(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = c("canvasXpress", "ggplot")),
                   regexp = msg)
    ## logRatioCol
    msg <- "logRatioCol to be a singular value of class character and must be in contrast data."
    expect_error(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "xyz"),
                 regexp =  msg)
    expect_error(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = c("xyz","abc")),
                 regexp =  msg)
    expect_error(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = NULL),
                 regexp =  msg)
    ## logIntCol
    msg <- "logIntCol to be a singular value of class character and must be in contrast data."
    expect_error(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", logIntCol = "xyz"),
                 regexp =  msg)
    expect_error(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", logIntCol = c("xyz","abc")),
                 regexp =  msg)
    expect_error(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", logIntCol = NULL),
                 regexp =  msg)
    ## pvalCol
    msg <- "pvalCol to be a singular value of class character and must be in contrast data."
    expect_error(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", pvalCol = "xyz"),
                 regexp = msg)
    expect_error(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", pvalCol = c("xyz","abc")),
                 regexp =  msg)
    expect_error(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", pvalCol = NULL),
                 regexp = msg)
    ## geneNameCol
    msg <- "geneNameCol to be a singular value of class character and must be in contrast data."
    expect_error(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", geneNameCol = NULL),
                 regexp = msg)
    expect_error(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", geneNameCol = c("xyz","abc")),
                 regexp =  msg)
    expect_error(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", geneNameCol = "xyz"),
                 regexp = msg)
    ## pthreshold
    msg <- "pthreshold must be a singular numeric value. Assigning default value 0.01"
    expect_warning(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", pthreshold = NULL),
                   regexp = msg)
    expect_warning(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", pthreshold = "0.1"),
                   regexp = msg)
    expect_warning(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", pthreshold = c(0.1, 0.1)),
                   regexp = msg)
    ## foldChangeThreshold
    msg <- "foldChangeThreshold must be a singular numeric value. Assigning default value log2(1.5)"
    expect_warning(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", foldChangeThreshold = NULL),
                   regexp = msg,
                   fixed = TRUE)
    expect_warning(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", foldChangeThreshold = "0.1"),
                   regexp = msg,
                   fixed = TRUE)
    expect_warning(profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", logRatioCol = "adj.P.Val", foldChangeThreshold = c(0.1, 0.1)),
                   regexp = msg,
                   fixed = TRUE)
    ## title
    msg <- "title must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", title = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", title = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", xlab = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", xlab = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    ## ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", ylab = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", ylab = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))

    ## referenceLine
    msg <- "referenceLine must be a singular value of class character or 'NULL' to disable. Assigning default value 'darkgoldenrod1'."
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", referenceLine = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", referenceLine = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", referenceLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", referenceLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", referenceLine = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", referenceLine = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    msg <- "Color specified is not valid. Assigning default value 'darkgoldenrod1'."
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", referenceLine = "abc"),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", referenceLine = "abc"),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))

    ## sizeBySignificance
    msg <- "sizeBySignificance must be a singular logical value. Assigning default value FALSE"
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", sizeBySignificance = "123"),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", sizeBySignificance = "123"),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", sizeBySignificance = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", sizeBySignificance = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", sizeBySignificance = NULL),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", sizeBySignificance = NULL),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))

    ## lineFitType
    msg <- "lineFitType must be one of 'glm', 'lm', 'loess', 'gam' or NULL to disable. Assigning default value 'loess'."
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", lineFitType = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", lineFitType = 123),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", lineFitType = c("123", "456")),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", lineFitType = c("loess", "loess")),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", lineFitType = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(pPlot <- profilePlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", lineFitType = c(123, 456)),
                   regexp = msg)
    expect_s3_class(pPlot , c("gg", "ggplot"))

})
