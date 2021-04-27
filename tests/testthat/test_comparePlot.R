context("DGEobj.plots - tests for comparePlot.R functions")


test_that("comparePlot.R: comparePlot()", {
    suppressWarnings(skip_if(is.null(getType(t_obj1, "topTable"))))

    # prepare testing data
    contrastList <- getType(t_obj1, "topTable")[1:2]
    compareDat <- comparePrep(contrastList)

    # testing plot with significance measures supplied and default parameters
    cPlot <- comparePlot(compareDat)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(compareDat, plotType = "ggplot", symbolSize = c(4, 4, 4, 2))
    expect_s3_class(cPlot , c("gg", "ggplot"))

    # testing plot without significance measures supplied and default parameters
    cPlot <- comparePlot(compareDat[, 1:2])
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(compareDat[,1:2], plotType = "ggplot", symbolSize = c(4, 4, 4, 2))
    expect_s3_class(cPlot , c("gg", "ggplot"))

    # testing aesthetics of plots with significance measures
    cPlot <- comparePlot(compareDat,
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
    cPlot <- comparePlot(compareDat,
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
    cPlot <- comparePlot(compareDat[, 1:2],
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
    cPlot <- comparePlot(compareDat[, 1:2],
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
    ## compareDF
    msg <- "Need at least two numeric columns in compareDF."
    expect_error(comparePlot(),
                 regexp = msg)
    expect_error(comparePlot(NULL),
                 regexp = msg)
    expect_error(comparePlot(compareDat[, 1]),
                 regexp = msg)
    expect_error(comparePlot(compareDat %>% as.list()),
                 regexp = msg)
    ## pThreshold
    msg <- "pThreshold must be a singular value of class numeric. Assigning default value '0.01'."
    expect_warning(cPlot <- comparePlot(compareDat, pThreshold = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", pThreshold = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, pThreshold = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", pThreshold = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, pThreshold = c("abc", "123")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", pThreshold = c("abc", "123")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, pThreshold = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", pThreshold = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## title
    msg <- "title must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(compareDat, title = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", title = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", title = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", title = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(compareDat, xlab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", xlab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", xlab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", xlab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(compareDat, ylab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", ylab = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", ylab = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", ylab = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## symbolShape
    msg <- "symbolShape must be a vector of 4 charcter values. Assigning default values 'circle', 'circle', 'circle', 'circle'."
    expect_warning(cPlot <- comparePlot(compareDat, symbolShape = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", symbolShape = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, symbolShape = 1),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", symbolShape = 1),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, symbolShape = c(1, 2)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", symbolShape = c(1, 2)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, symbolShape = c("cube", "cube", "cube", "cube")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", symbolShape = c("cube", "cube", "cube", "cube")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## symbolSize
    msg <- "symbolSize must be a vector of 4 integer values. Assigning default values 7, 7, 7, 3."
    expect_warning(cPlot <- comparePlot(compareDat, symbolSize = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", symbolSize = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, symbolSize = 1),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", symbolSize = 1),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, symbolSize = c(1, 2)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", symbolSize = c(1, 2)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, symbolSize = c("1", "2", "3", "4")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", symbolSize = c("1", "2", "3", "4")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## symbolColor
    msg <- "symbolColor must be a vector of 4 character values. Assigning default values 'darkgoldenrod1', 'deepskyblue4', 'red3', 'grey25'."
    expect_warning(cPlot <- comparePlot(compareDat, symbolColor = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", symbolColor = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, symbolColor = "black"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", symbolColor = "black"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, symbolColor = c("black", "grey0")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", symbolColor = c("black", "grey0")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, symbolColor = c(1, 2, 3, 4)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", symbolColor = c(1, 2, 3, 4)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, symbolColor = c("abc", "abc", "abc", "abc")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", symbolColor = c("abc", "abc", "abc", "abc")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## transparency
    msg <- "transparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.5'."
    expect_warning(cPlot <- comparePlot(compareDat, transparency = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", transparency = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, transparency = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", transparency = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, transparency = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", transparency = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## crosshair
    msg <- "crosshair must be a singular value of class character or 'NULL' to disable. Assigning default value 'grey50'."
    expect_warning(cPlot <- comparePlot(compareDat, crosshair = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", crosshair = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, crosshair = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", crosshair = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, crosshair = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", crosshair = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    cPlot <- comparePlot(compareDat, crosshair = NULL)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(compareDat, plotType = "ggplot", crosshair = NULL)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    msg <- "Color specified is not valid. Assigning default value 'grey50'."
    expect_warning(cPlot <- comparePlot(compareDat, crosshair = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", crosshair = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    ## referenceLine
    msg <- "referenceLine must be a singular value of class character or 'NULL' to disable. Assigning default value 'darkgoldenrod1'."
    expect_warning(cPlot <- comparePlot(compareDat, referenceLine = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", referenceLine = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, referenceLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", referenceLine = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, referenceLine = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", referenceLine = c(123, 456)),
                   regexp = msg)
    msg <- "Color specified is not valid. Assigning default value 'darkgoldenrod1'."
    expect_warning(cPlot <- comparePlot(compareDat, referenceLine = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", referenceLine = "abc"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## refLineThickness
    msg <- "refLineThickness must be a singular value of class numeric Assigning default value '1'."
    expect_warning(cPlot <- comparePlot(compareDat, refLineThickness = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", refLineThickness = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, refLineThickness = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", refLineThickness = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, refLineThickness = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", refLineThickness = NULL),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    ## legendPosition
    msg <- "legendPosition must be one value from 'top', 'bottom', 'left', 'right', 'ne', 'se', 'nw', 'sw' or 'NULL' to disable. Assigning default value 'right'."
    expect_warning(cPlot <- comparePlot(compareDat, legendPosition = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", legendPosition = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, legendPosition = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", legendPosition = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, legendPosition = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", legendPosition = c(123, 456)),
                   regexp = msg)
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", legendPosition = "xyz"),
                   regexp = msg)

    ## footnote
    msg <- "footnote must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(compareDat, footnote = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", footnote = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, footnote = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", footnote = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, footnote = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", footnote = c(123, 456)),
                   regexp = msg)


    ## footnote
    msg <- "footnote must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'."
    expect_warning(cPlot <- comparePlot(compareDat, footnote = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", footnote = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, footnote = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", footnote = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, footnote = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, plotType = "ggplot", footnote = c(123, 456)),
                   regexp = msg)

    ## footnoteColor
    msg <- "footnoteColor must be a singular value of class character. Assigning default value 'black'."
    expect_warning(cPlot <- comparePlot(compareDat, footnote = "notes", footnoteColor = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, footnote = "notes", plotType = "ggplot", footnoteColor = 123),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, footnote = "notes", footnoteColor = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, footnote = "notes", plotType = "ggplot", footnoteColor = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, footnote = "notes", footnoteColor = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, footnote = "notes", plotType = "ggplot", footnoteColor = c(123, 456)),
                   regexp = msg)

    ## footnoteSize
    msg <- "footnoteSize must be a singular value of class numeric. Assigning default value '3'."
    expect_warning(cPlot <- comparePlot(compareDat, footnote = "notes", footnoteSize = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, footnote = "notes", plotType = "ggplot", footnoteSize = "123"),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, footnote = "notes", footnoteSize = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, footnote = "notes", plotType = "ggplot", footnoteSize = c(123, 456)),
                   regexp = msg)
    expect_s3_class(cPlot , c("gg", "ggplot"))
    expect_warning(cPlot <- comparePlot(compareDat, footnote = "notes", footnoteSize = c("123", "456")),
                   regexp = msg)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    expect_warning(cPlot <- comparePlot(compareDat, footnote = "notes", plotType = "ggplot", footnoteSize = c("123", "456")),
                   regexp = msg)


    expect_s3_class(cPlot , c("gg", "ggplot"))
    cPlot <- comparePlot(compareDat, crosshair = NULL)
    expect_s3_class(cPlot , c("canvasXpress", "htmlwidget"))
    cPlot <- comparePlot(compareDat, plotType = "ggplot", crosshair = NULL)
    expect_s3_class(cPlot , c("gg", "ggplot"))

    # testing assert statement
    expect_error(comparePlot(compareDat[, 1, drop = FALSE]),
                 regexp = "Need at least two numeric columns in compareDF.")
    expect_error(comparePlot(t_obj1$design),
                 regexp = "Need at least two numeric columns in compareDF.")
    expect_error(comparePlot(compareDat, plotType = "cx"),
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
