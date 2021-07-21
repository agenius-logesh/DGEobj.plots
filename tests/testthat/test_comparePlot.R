context("DGEobj.plots - tests for comparePlot.R functions")


test_that("comparePlot.R: comparePlot()", {
    suppressWarnings(skip_if(is.null(getType(t_obj1, "topTable"))))

    # prepare testing data
    DGEdata = t_obj1
    contrastOne = "BDL_vs_Sham"
    contrastTwo = "EXT1024_vs_BDL"

    # testing plot with significance measures supplied and default parameters
    cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo,
                         plotType = "ggplot")
    expect_s3_class(cPlot , c("gg", "ggplot"))

    # testing plot without significance measures supplied and default parameters
    cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, sigMeasurePlot = FALSE)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, sigMeasurePlot = FALSE,
                          plotType = "ggplot")
    expect_s3_class(cPlot , c("gg", "ggplot"))

    # testing aesthetics of plots with significance measures
    cPlot <- comparePlot(DGEdata,
                         contrastOne,
                         contrastTwo,
                         pThreshold = 0.001,
                         title    = "MyPlot",
                         xlab     = "xaxis-title",
                         ylab     = "yaxis-title",
                         referenceLine = "darkgoldenrod1")
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(DGEdata,
                         contrastOne,
                         contrastTwo,
                         plotType = "ggplot",
                         pThreshold = 0.001,
                         title    = "MyPlot",
                         xlab     = "xaxis-title",
                         ylab     = "yaxis-title",
                         referenceLine = "darkgoldenrod1")
    expect_setequal(unlist(cPlot$labels[c("title","y", "x")]), c("MyPlot", "yaxis-title", "xaxis-title"))
    expect_setequal(cPlot$layers[[2]]$aes_params$colour, "grey50")

    # testing aesthetics of plots without significance measures
    cPlot <- comparePlot(DGEdata,
                         contrastOne,
                         contrastTwo,
                         sigMeasurePlot = FALSE,
                         pThreshold = 0.001,
                         title    = "MyPlot",
                         xlab     = "xaxis-title",
                         ylab     = "yaxis-title",
                         referenceLine = "darkgoldenrod1")
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(DGEdata,
                         contrastOne,
                         contrastTwo,
                         sigMeasurePlot = FALSE,
                         plotType = "ggplot",
                         pThreshold = 0.001,
                         title    = "MyPlot",
                         xlab     = "xaxis-title",
                         ylab     = "yaxis-title",
                         referenceLine = "darkgoldenrod1")
    expect_setequal(unlist(cPlot$labels[c("title","y", "x")]), c("MyPlot", "yaxis-title", "xaxis-title"))
    expect_setequal(cPlot$layers[[2]]$aes_params$colour, "grey50")

    # testing asserts
    ## DGEdata
    msg <- "DGEdata must be specified as class of DGEobj."
    expect_error(comparePlot(),
                 regexp = msg)
    expect_error(comparePlot(NULL),
                 regexp = msg)
    expect_error(comparePlot(123),
                 regexp = msg)
    expect_error(comparePlot("123"),
                 regexp = msg)
    expect_error(comparePlot("xyz"),
                 regexp = msg)
    ## contrastOne
    msg = "contrastOne to be a singular value of class character and must be one from DGEdata with logFC and P.value columns."
    expect_error(comparePlot(DGEdata),
                 regexp = msg)
    expect_error(comparePlot(DGEdata, contrastOne = NULL),
                 regexp = msg)
    expect_error(comparePlot(DGEdata, contrastOne = "123"),
                 regexp = msg)
    expect_error(comparePlot(DGEdata, contrastOne = 123),
                 regexp = msg)
    expect_error(comparePlot(DGEdata, contrastOne = "xyz"),
                 regexp = msg)
    ## contrastTwo
    msg = "contrastTwo to be a singular value of class character and must be one from DGEdata with logFC and P.value columns."
    expect_error(comparePlot(DGEdata, contrastOne),
                 regexp = msg)
    expect_error(comparePlot(DGEdata, contrastOne, contrastTwo = NULL),
                 regexp = msg)
    expect_error(comparePlot(DGEdata, contrastOne, contrastTwo = "123"),
                 regexp = msg)
    expect_error(comparePlot(DGEdata, contrastOne, contrastTwo = 123),
                 regexp = msg)
    expect_error(comparePlot(DGEdata, contrastOne, contrastTwo = "xyz"),
                 regexp = msg)

    #plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'."
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "cx"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = 1),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = c("canvasxpress","ggplot")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))

    ## sigMeasurePlot
    msg <- "sigMeasurePlot must be a singular logical value. Assigning default value TRUE"
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, sigMeasurePlot = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", sigMeasurePlot = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, sigMeasurePlot = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", sigMeasurePlot = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, sigMeasurePlot = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", sigMeasurePlot = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    ## pThreshold
    msg <- "pThreshold must be a singular value of class numeric. Assigning default value '0.01'."
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, pThreshold = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", pThreshold = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, pThreshold = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", pThreshold = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, pThreshold = c("abc", "123")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", pThreshold = c("abc", "123")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, pThreshold = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", pThreshold = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## title
    msg <- "title must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, title = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", title = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, xlab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", xlab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, ylab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", ylab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## referenceLine
    msg <- "referenceLine must be a singular value of class character or 'NULL' to disable. Assigning default value 'darkgoldenrod1'."
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, referenceLine = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", referenceLine = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, referenceLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", referenceLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, referenceLine = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", referenceLine = c(123, 456)),
                   regexp = msg)
    msg <- "Color specified is not valid. Assigning default value 'darkgoldenrod1'."
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, referenceLine = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata, contrastOne, contrastTwo, plotType = "ggplot", referenceLine = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

})

test_that("comparePlot.R: comparePrep()", {
    suppressWarnings(skip_if(is.null(getType(t_obj1, "topTable"))))

    contrastList <- getType(t_obj1, "topTable")[1:2]
    # Capture the default logFC and P.Value
    compareDat <- comparePrep(contrastList)
    expect_s3_class(compareDat,"data.frame")

    expect_error(comparePrep(contrastList[[1]]),
                 regexp = "contrastList must be a named list of length 2 where both items are of class 'data.frame'.")
    expect_error(comparePrep(contrastList, valueCol = "P.val"),
                 regexp = "The valueCol must be included in the colnames of both items of contrastList.")
    expect_error(comparePrep(contrastList, significanceCol = "P.val"),
                 regexp = "The significanceCol must be included in the colnames of both items of contrastList.")
    contrastList_uncommon_ids <- list("BDL_vs_Sham" = contrastList$BDL_vs_Sham[1:10,], "EXT1024_vs_BDL" = contrastList$EXT1024_vs_BDL[21:30,])
    expect_error(comparePrep(contrastList_uncommon_ids),
                 regexp = "No common gene IDs were found between the two dataframes in contrastList.")
})
