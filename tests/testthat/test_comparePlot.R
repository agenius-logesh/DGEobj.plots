context("DGEobj.plots - tests for comparePlot.R functions")


test_that("comparePlot.R: comparePlot()", {
    suppressWarnings(skip_if(is.null(getType(t_obj1, "topTable"))))

    # prepare testing data
    # testing plot with significance measures supplied and default parameters
    cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"))
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"),
                         plotType = "ggplot", symbolSize = c(4, 4, 4, 2))
    expect_s3_class(cPlot , c("gg", "ggplot"))

    # testing plot without significance measures supplied and default parameters
    cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), sigMeasurePlot = FALSE)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), sigMeasurePlot = FALSE,
                          plotType = "ggplot", symbolSize = c(4, 4, 4, 2))
    expect_s3_class(cPlot , c("gg", "ggplot"))

    # testing aesthetics of plots with significance measures
    cPlot <- comparePlot(DGEdata = t_obj1,
                         contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"),
                         pThreshold = 0.001,
                         title    = "MyPlot",
                         xlab     = "xaxis-title",
                         ylab     = "yaxis-title",
                         symbolSize = c(5, 5, 2, 2),
                         transparency = 0.5,
                         crosshair = "grey50",
                         referenceLine = "darkgoldenrod1",
                         refLineThickness = 1,
                         legendPosition = "right",
                         footnote = "This is my footnote")
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(DGEdata = t_obj1,
                         contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"),
                         plotType = "ggplot",
                         pThreshold = 0.001,
                         title    = "MyPlot",
                         xlab     = "xaxis-title",
                         ylab     = "yaxis-title",
                         symbolSize = c(5, 5, 2, 2),
                         transparency = 0.5,
                         crosshair = "grey50",
                         referenceLine = "darkgoldenrod1",
                         refLineThickness = 1,
                         legendPosition = "right",
                         footnote = "This is my footnote")
    expect_setequal(unlist(cPlot$labels[c("title","y", "x")]), c("MyPlot", "yaxis-title", "xaxis-title"))
    expect_setequal(cPlot$layers[[2]]$aes_params$colour, "grey50")

    # testing aesthetics of plots without significance measures
    cPlot <- comparePlot(DGEdata = t_obj1,
                         contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"),
                         sigMeasurePlot = FALSE,
                         pThreshold = 0.001,
                         title    = "MyPlot",
                         xlab     = "xaxis-title",
                         ylab     = "yaxis-title",
                         symbolSize = c(5, 5, 2, 2),
                         transparency = 0.5,
                         crosshair = "grey50",
                         referenceLine = "darkgoldenrod1",
                         refLineThickness = 1,
                         legendPosition = "right",
                         footnote = "This is my footnote")
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(DGEdata = t_obj1,
                         contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"),
                         sigMeasurePlot = FALSE,
                         plotType = "ggplot",
                         pThreshold = 0.001,
                         title    = "MyPlot",
                         xlab     = "xaxis-title",
                         ylab     = "yaxis-title",
                         symbolSize = c(5, 5, 2, 2),
                         transparency = 0.5,
                         crosshair = "grey50",
                         referenceLine = "darkgoldenrod1",
                         refLineThickness = 1,
                         legendPosition = "right",
                         footnote = "This is my footnote")
    expect_setequal(unlist(cPlot$labels[c("title","y", "x")]), c("MyPlot", "yaxis-title", "xaxis-title"))
    expect_setequal(cPlot$layers[[2]]$aes_params$colour, "grey50")

    # testing asserts
    ## DGEdata
    msg <- "DGEdata must be specified and must belong to DGEobj class."
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
    msg = "contrasts must be a class of character and must be two of the top tables in the DGEdata. with logFC and P.value columns."
    expect_error(comparePlot(DGEdata),
                 regexp = msg)
    expect_error(comparePlot(DGEdata, contrasts = NULL),
                 regexp = msg)
    expect_error(comparePlot(DGEdata, contrasts = "123"),
                 regexp = msg)
    expect_error(comparePlot(DGEdata, contrasts = c(123,234)),
                 regexp = msg)
    expect_error(comparePlot(DGEdata, contrasts = "xyz"),
                 regexp = msg)

    ## sigMeasurePlot
    msg <- "sigMeasurePlot must be a singular logical value. Assigning default value TRUE"
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), sigMeasurePlot = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", sigMeasurePlot = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), sigMeasurePlot = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", sigMeasurePlot = c(TRUE, TRUE)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), sigMeasurePlot = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", sigMeasurePlot = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    ## pThreshold
    msg <- "pThreshold must be a singular value of class numeric. Assigning default value '0.01'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), pThreshold = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", pThreshold = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), pThreshold = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", pThreshold = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), pThreshold = c("abc", "123")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", pThreshold = c("abc", "123")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), pThreshold = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", pThreshold = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## title
    msg <- "title must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), title = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", title = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), xlab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", xlab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), ylab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", ylab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## symbolShape
    msg <- "symbolShape must be a vector of 4 charcter values. Assigning default values 'circle', 'circle', 'circle', 'circle'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), symbolShape = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", symbolShape = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), symbolShape = 1),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", symbolShape = 1),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), symbolShape = c(1, 2)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", symbolShape = c(1, 2)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), symbolShape = c("cube", "cube", "cube", "cube")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", symbolShape = c("cube", "cube", "cube", "cube")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## symbolSize
    msg <- "symbolSize must be a vector of 4 integer values. Assigning default values 7, 7, 7, 3."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), symbolSize = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", symbolSize = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), symbolSize = 1),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", symbolSize = 1),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), symbolSize = c(1, 2)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", symbolSize = c(1, 2)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), symbolSize = c("1", "2", "3", "4")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", symbolSize = c("1", "2", "3", "4")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## symbolColor
    msg <- "symbolColor must be a vector of 4 character values. Assigning default values 'darkgoldenrod1', 'deepskyblue4', 'red3', 'grey25'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), symbolColor = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", symbolColor = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), symbolColor = "black"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", symbolColor = "black"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), symbolColor = c("black", "grey0")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", symbolColor = c("black", "grey0")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), symbolColor = c(1, 2, 3, 4)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", symbolColor = c(1, 2, 3, 4)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), symbolColor = c("abc", "abc", "abc", "abc")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", symbolColor = c("abc", "abc", "abc", "abc")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## transparency
    msg <- "transparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.5'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), transparency = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", transparency = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), transparency = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", transparency = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), transparency = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", transparency = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## crosshair
    msg <- "crosshair must be a singular value of class character or 'NULL' to disable. Assigning default value 'grey50'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), crosshair = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", crosshair = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), crosshair = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", crosshair = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), crosshair = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", crosshair = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), crosshair = NULL)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", crosshair = NULL)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    msg <- "Color specified is not valid. Assigning default value 'grey50'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), crosshair = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", crosshair = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    ## referenceLine
    msg <- "referenceLine must be a singular value of class character or 'NULL' to disable. Assigning default value 'darkgoldenrod1'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), referenceLine = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", referenceLine = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), referenceLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", referenceLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), referenceLine = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", referenceLine = c(123, 456)),
                   regexp = msg)
    msg <- "Color specified is not valid. Assigning default value 'darkgoldenrod1'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), referenceLine = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", referenceLine = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## refLineThickness
    msg <- "refLineThickness must be a singular value of class numeric Assigning default value '1'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), refLineThickness = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", refLineThickness = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), refLineThickness = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", refLineThickness = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), refLineThickness = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", refLineThickness = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## legendPosition
    msg <- "legendPosition must be one value from 'top', 'bottom', 'left', 'right', 'ne', 'se', 'nw', 'sw' or 'NULL' to disable. Assigning default value 'right'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), legendPosition = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", legendPosition = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), legendPosition = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", legendPosition = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), legendPosition = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", legendPosition = c(123, 456)),
                   regexp = msg)
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", legendPosition = "xyz"),
                   regexp = msg)

    ## footnote
    msg <- "footnote must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", footnote = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", footnote = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", footnote = c(123, 456)),
                   regexp = msg)


    ## footnote
    msg <- "footnote must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", footnote = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", footnote = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", footnote = c(123, 456)),
                   regexp = msg)

    ## footnoteColor
    msg <- "footnoteColor must be a singular value of class character. Assigning default value 'black'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = "notes", footnoteColor = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = "notes", plotType = "ggplot", footnoteColor = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = "notes", footnoteColor = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = "notes", plotType = "ggplot", footnoteColor = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = "notes", footnoteColor = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = "notes", plotType = "ggplot", footnoteColor = c(123, 456)),
                   regexp = msg)

    ## footnoteSize
    msg <- "footnoteSize must be a singular value of class numeric. Assigning default value '3'."
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = "notes", footnoteSize = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = "notes", plotType = "ggplot", footnoteSize = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = "notes", footnoteSize = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = "notes", plotType = "ggplot", footnoteSize = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = "notes", footnoteSize = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), footnote = "notes", plotType = "ggplot", footnoteSize = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), crosshair = NULL)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "ggplot", crosshair = NULL)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    # testing assert statement
    expect_error(comparePlot(DGEdata = t_obj1, contrasts = c("BDL_vs_Sham", "EXT1024_vs_BDL"), plotType = "cx"),
                 regexp = "Plot type must be either canvasXpress or ggplot.")
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
