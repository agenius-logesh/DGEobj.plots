context("DGEobj.plots - tests for comparePlot.R functions")


test_that("comparePlot.R: comparePlot()", {
    suppressWarnings(skip_if(is.null(getType(t_obj1, "topTable"))))

    # prepare testing data
    # testing plot with significance measures supplied and default parameters
    cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"))
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))

    cPlot <- comparePlot(dgeObj = t_obj1,
                         contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"),
                         plotType = "ggplot")
    expect_s3_class(cPlot , c("gg", "ggplot"))

    # testing plot without significance measures supplied and default parameters
    cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), sigMeasurePlot = FALSE)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))

    cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), sigMeasurePlot = FALSE,
                          plotType = "ggplot")
    expect_s3_class(cPlot , c("gg", "ggplot"))

    # testing aesthetics of plots with significance measures
    cPlot <- comparePlot(dgeObj = t_obj1,
                         contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"),
                         pThreshold = 0.001,
                         title    = "MyPlot",
                         xlab     = "xaxis-title",
                         ylab     = "yaxis-title",
                         referenceLine = "darkgoldenrod1")
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(dgeObj = t_obj1,
                         contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"),
                         plotType = "ggplot",
                         pThreshold = 0.001,
                         title    = "MyPlot",
                         xlab     = "xaxis-title",
                         ylab     = "yaxis-title",
                         referenceLine = "darkgoldenrod1")
    expect_setequal(unlist(cPlot$labels[c("title","y", "x")]), c("MyPlot", "yaxis-title", "xaxis-title"))
    expect_setequal(cPlot$layers[[2]]$aes_params$colour, "grey50")

    # testing aesthetics of plots without significance measures
    cPlot <- comparePlot(dgeObj = t_obj1,
                         contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"),
                         sigMeasurePlot = FALSE,
                         pThreshold = 0.001,
                         title    = "MyPlot",
                         xlab     = "xaxis-title",
                         ylab     = "yaxis-title",
                         referenceLine = "darkgoldenrod1")
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(dgeObj = t_obj1,
                         contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"),
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
    ## dgeObj
    msg <- "dgeObj must be specified and must belong to DGEobj class."
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
    ## contrasts
    msg = "contrasts must be a class of character and must be two of the top tables in the dgeObj. with logFC and P.value columns."
    expect_error(comparePlot(dgeObj = t_obj1),
                 regexp = msg)
    expect_error(comparePlot(dgeObj = t_obj1, contrasts = NULL),
                 regexp = msg)
    expect_error(comparePlot(dgeObj = t_obj1, contrasts = "123"),
                 regexp = msg)
    expect_error(comparePlot(dgeObj = t_obj1, contrasts = c(123,234)),
                 regexp = msg)
    expect_error(comparePlot(dgeObj = t_obj1, contrasts = "xyz"),
                 regexp = msg)

    #plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'."
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "cx"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = 1),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = c("canvasxpress","ggplot")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))

    ## sigMeasurePlot
    msg <- "sigMeasurePlot must be a singular logical value. Assigning default value TRUE"
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), sigMeasurePlot = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", sigMeasurePlot = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), sigMeasurePlot = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", sigMeasurePlot = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), sigMeasurePlot = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", sigMeasurePlot = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    ## pThreshold
    msg <- "pThreshold must be a singular value of class numeric. Assigning default value '0.01'."
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), pThreshold = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", pThreshold = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), pThreshold = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", pThreshold = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), pThreshold = c("abc", "123")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", pThreshold = c("abc", "123")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), pThreshold = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", pThreshold = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## title
    msg <- "title must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), title = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", title = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), xlab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", xlab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), ylab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", ylab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## referenceLine
    msg <- "referenceLine must be a singular value of class character or 'NULL' to disable. Assigning default value 'darkgoldenrod1'."
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), referenceLine = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", referenceLine = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), referenceLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", referenceLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), referenceLine = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", referenceLine = c(123, 456)),
                   regexp = msg)
    msg <- "Color specified is not valid. Assigning default value 'darkgoldenrod1'."
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), referenceLine = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(dgeObj = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", referenceLine = "abc"),
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
