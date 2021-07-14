context("DGEobj.plots - tests for cdfPlot.R functions")


test_that("cdfPlot.R: cdfPlot()", {
    skip_if(!("BDL_vs_Sham" %in% names(t_obj1)))

    DGEdata = t_obj1
    contrast = "BDL_vs_Sham"
    # testing plot with default values.
    plot <- cdfPlot(DGEdata, contrast, referenceLine = "blue")
    expect_type(plot, "list")
    expect_s3_class(plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(plot$inset, c("canvasXpress", "htmlwidget"))

    plot <- cdfPlot(DGEdata, contrast, plotType = "ggplot", referenceLine = "blue")
    expect_s3_class(plot$main, c("gg", "ggplot"))
    expect_s3_class(plot$inset, c("gg", "ggplot"))
    expect_s3_class(plot$combined, c("gg", "ggplot"))

    #without reference line
    plot <- cdfPlot(DGEdata, contrast)
    expect_type(plot, "list")
    expect_s3_class(plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(plot$inset, c("canvasXpress", "htmlwidget"))

    plot <- cdfPlot(DGEdata, contrast, plotType = "ggplot")
    expect_s3_class(plot$main, c("gg", "ggplot"))
    expect_s3_class(plot$inset, c("gg", "ggplot"))
    expect_s3_class(plot$combined, c("gg", "ggplot"))

    #testing plot with optional parameters
    plot <- cdfPlot(DGEdata,
                    contrast,
                    referenceLine = "blue",
                    pThreshold = 4.114957e-97,
                    pvalMax = 5.77e-96,
                    symbolSize     = c(30, 20),
                    symbolShape    = c("circle", "square"),
                    symbolColor    = c("green", "deepskyblue4"),
                    transparency   = 1.0,
                    refLineThickness = 2)
    expect_type(plot, "list")
    expect_s3_class(plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(plot$inset, c("canvasXpress", "htmlwidget"))


    plot <- cdfPlot(DGEdata,
                    contrast,
                    referenceLine = "blue",
                    pThreshold = 4.114957e-97,
                    pvalMax = 5.77e-96,
                    symbolSize     = c(3, 2),
                    symbolShape    = c("circle", "square"),
                    symbolColor    = c("green", "deepskyblue4"),
                    transparency   = 1.0,
                    refLineThickness = 2,
                    plotType = "ggplot")
    expect_type(plot, "list")
    expect_s3_class(plot$main, c("gg", "ggplot"))
    expect_s3_class(plot$inset, c("gg", "ggplot"))
    expect_s3_class(plot$combined, c("gg", "ggplot"))

    # testing plot with customized aesthetics.
    plot_with_aes <- cdfPlot(DGEdata,
                             contrast,
                             insetTitle    = "Sub plot title",
                             xlab          = "xaxis-title",
                             ylab          = "yaxis-title",
                             title         = "MyPlot",
                             referenceLine = "blue",
                             footnote      = "this is footnote of the plot")

    expect_type(plot_with_aes, "list")
    expect_s3_class(plot_with_aes$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(plot_with_aes$inset, c("canvasXpress", "htmlwidget"))

    plot_with_aes <- cdfPlot(DGEdata,
                             contrast,
                             plotType      = "ggplot",
                             insetTitle    = "Sub plot title",
                             xlab          = "xaxis-title",
                             ylab          = "yaxis-title",
                             title         = "MyPlot",
                             referenceLine = "blue",
                             footnote      = "this is footnote of the plot",
                             viewportX     = 0,
                             viewportY     = 1,
                             viewportWidth = 0.45)
    expect_type(plot_with_aes, "list")
    expect_s3_class(plot_with_aes$main, c("gg", "ggplot"))
    expect_s3_class(plot_with_aes$inset, c("gg", "ggplot"))
    expect_s3_class(plot_with_aes$combined, c("gg", "ggplot"))

    expect_setequal(unlist(plot_with_aes$main$labels[c("title", "y", "x")]), c("MyPlot", "yaxis-title", "xaxis-title"))
    expect_setequal(plot_with_aes$inset$labels$title, "Sub plot title")
    expect_equal(plot_with_aes$main$layers[[2]]$geom_params$colour, "blue")
    expect_equal(plot_with_aes$main$layers[[3]]$geom_params$label, "this is footnote of the plot")

    #DGEdata
    msg <- "DGEdata must be specified as class of DGEobj."
    expect_error(cdf_plot <- cdfPlot(DGEdata = NULL),
                 regexp = msg)
    expect_error(cdf_plot <- cdfPlot(DGEdata = "xyz"),
                 regexp = msg)
    expect_error(cdf_plot <- cdfPlot(),
                 regexp = msg)
    expect_error(cdf_plot <- cdfPlot(DGEdata = 123),
                 regexp = msg)
    expect_error(cdfPlot(data.frame()),
                 regexp = msg)
    #contrast
    msg <- "contrast to be a singular value of class character and must be one from DGEdata with LogIntensity and LogRatio columns and optionally a p-value."
    expect_error(cdf_plot <- cdfPlot(DGEdata),
                 regexp = msg)
    expect_error(cdf_plot <- cdfPlot(DGEdata, contrast = NULL),
                 regexp = msg)
    expect_error(cdf_plot <- cdfPlot(DGEdata, contrast = "xyz"),
                 regexp = msg)
    expect_error(cdfPlot(DGEdata, contrast = 123),
                 regexp = msg)

    #plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, plotType = "cx"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, plotType = NULL),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, plotType = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, plotType = c("canvasxpress","ggplot")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #pvalCol
    msg <- "pvalCol column not found in contrast data."
    expect_error(cdf_plot <- cdfPlot(DGEdata, contrast, pvalCol = "notacolumn"),
                 regexp = msg)
    expect_error(cdf_plot <- cdfPlot(DGEdata, contrast, pvalCol = NULL),
                 regexp = msg)

    #pThreshold
    msg <- "pthreshold must be a singular numeric value. Assigning default value 0.01."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, pThreshold = "notavalue"),
                 regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, pThreshold = NULL),
                 regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, pThreshold = c(1,2)),
                 regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, pThreshold = "notavalue"),
                 regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))


    #title
    msg <- "title must be a singular value of class character. Assigning default value NULL."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, title = c("title","title")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, title = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #insetTitle
    msg <- "insetTitle must be a singular value of class character. Assigning default value NULL."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, insetTitle = c("title","title")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, insetTitle = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'Rank' as the label."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, xlab = c("xlab","xlab")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, xlab = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'pvalCol' as the label."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, ylab = c("ylab","ylab")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, ylab = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #symbolSize
    msg <- "symbolSize must be a vector of 2 integer values. Assigning default values 2,1."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, symbolSize = "notavalidvalue"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, symbolSize = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, symbolSize = c(1,2,3)),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, symbolSize = NULL),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #symbolShape
    msg <- "symbolShape must be a vector of 2 charcter values. Assigning default values 'circle'."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, symbolShape = c("notavalidvalue","symbolColor")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, symbolShape = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, symbolShape = c("circle","circle","circle")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, symbolShape = NULL),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #symbolColor
    msg <- "symbolColor must be a vector of 2 character values. Assigning default values 'red3', 'deepskyblue4'."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, symbolColor = c("notavalidvalue","symbolColor")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, symbolColor = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, symbolColor = c("red","red","red")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, symbolColor = NULL),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #transparency
    msg <- "transparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value 0.7."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, transparency = NULL),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, transparency = c(1,2)),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, transparency = -1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdfPlot(DGEdata, contrast, transparency = "notavalidvalue"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdfPlot(DGEdata, contrast, transparency = 2),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #referenceLine
    msg <- "referenceLine must be a singular value of class character or NULL to disable. Assigning default value NULL."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, referenceLine = c("blue", "blue")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, referenceLine = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, referenceLine = "notavalidvalue"),
                   regexp = "Color specified is not valid. Assigning default value NULL.")
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #refLineThickness
    msg <- "refLineThickness must be a singular value of class numeric Assigning default value 1."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, refLineThickness = c(1,2)),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, refLineThickness = -1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, refLineThickness = NULL),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, refLineThickness = "notavalidvalue"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #viewportX
    msg <- "viewportX must be a singular value of class numeric and must be greater than 0. Assigning default value 0.15."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, viewportX = c(1,2), plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, viewportX = "notavalidvalue", plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, viewportX = NULL, plotType = "ggplot"),
                    regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    #viewportY
    msg <- "viewportY must be a singular value of class numeric and must be greater than 0. Assigning default value 0.85."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, viewportY = c(1,2), plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, viewportY = "notavalidvalue", plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, viewportY = NULL, plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    #viewportWidth
    msg <- "viewportWidth must be a singular value of class numeric. Assigning default value 0.35."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, viewportWidth = c(1,2), plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, viewportWidth = "notavalidvalue", plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, viewportWidth = NULL, plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, viewportWidth = -2, plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    #pvalMax
    msg <- "pvalMax must be a singular numeric value. Assigning default value 0.1."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, pvalMax = c(1,2)),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, pvalMax = NULL),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, pvalMax = "notavalidvalue"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #footnote
    msg <- "footnote must be a singular value of class character or NULL to disable. Assigning default value NULL."
    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, footnote = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(DGEdata, contrast, footnote = c("footnote","footnote")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))
})

