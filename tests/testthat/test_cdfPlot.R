context("DGEobj.plots - tests for cdfPlot.R functions")


test_that("cdfPlot.R: cdfPlot()", {
    skip_if(!("BDL_vs_Sham" %in% names(t_obj1)))

    # testing plot with default values.
    plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", referenceLine = "blue")
    expect_type(plot, "list")
    expect_s3_class(plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(plot$inset, c("canvasXpress", "htmlwidget"))

    plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot", referenceLine = "blue")
    expect_s3_class(plot$main, c("gg", "ggplot"))
    expect_s3_class(plot$inset, c("gg", "ggplot"))
    expect_s3_class(plot$combined, c("gg", "ggplot"))

    #without reference line
    plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham")
    expect_type(plot, "list")
    expect_s3_class(plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(plot$inset, c("canvasXpress", "htmlwidget"))

    plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "ggplot")
    expect_s3_class(plot$main, c("gg", "ggplot"))
    expect_s3_class(plot$inset, c("gg", "ggplot"))
    expect_s3_class(plot$combined, c("gg", "ggplot"))

    #testing plot with optional parameters
    plot <- cdfPlot(dgeObj = t_obj1,
                    contrast = "BDL_vs_Sham",
                    referenceLine = "blue",
                    pThreshold = 0.3,
                    pvalMax = 0.5)
    expect_type(plot, "list")
    expect_s3_class(plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(plot$inset, c("canvasXpress", "htmlwidget"))


    plot <- cdfPlot(dgeObj = t_obj1,
                    contrast = "BDL_vs_Sham",
                    referenceLine = "blue",
                    pThreshold = 0.3,
                    pvalMax = 0.5,
                    plotType = "ggplot")
    expect_type(plot, "list")
    expect_s3_class(plot$main, c("gg", "ggplot"))
    expect_s3_class(plot$inset, c("gg", "ggplot"))
    expect_s3_class(plot$combined, c("gg", "ggplot"))

    # testing plot with customized aesthetics.
    plot_with_aes <- cdfPlot(dgeObj = t_obj1,
                             contrast = "BDL_vs_Sham",
                             viewportTitle    = "Sub plot title",
                             xlab          = "xaxis-title",
                             ylab          = "yaxis-title",
                             title         = "MyPlot",
                             referenceLine = "blue")

    expect_type(plot_with_aes, "list")
    expect_s3_class(plot_with_aes$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(plot_with_aes$inset, c("canvasXpress", "htmlwidget"))

    plot_with_aes <- cdfPlot(dgeObj = t_obj1,
                             contrast = "BDL_vs_Sham",
                             plotType      = "ggplot",
                             viewportTitle    = "Sub plot title",
                             xlab          = "xaxis-title",
                             ylab          = "yaxis-title",
                             title         = "MyPlot",
                             referenceLine = "blue",
                             viewportX     = 0,
                             viewportY     = 1,
                             viewportWidth = 0.45)
    expect_type(plot_with_aes, "list")
    expect_s3_class(plot_with_aes$main, c("gg", "ggplot"))
    expect_s3_class(plot_with_aes$inset, c("gg", "ggplot"))
    expect_s3_class(plot_with_aes$combined, c("gg", "ggplot"))

    expect_setequal(unlist(plot_with_aes$main$labels[c("title", "y", "x")]), c("MyPlot", "yaxis-title", "xaxis-title"))
    expect_setequal(plot_with_aes$inset$labels$title, "Sub plot title")

    #dgeObj
    msg <- "dgeObj must be specified and must belong to DGEobj class."
    expect_error(cdf_plot <- cdfPlot(dgeObj = NULL),
                 regexp = msg)
    expect_error(cdf_plot <- cdfPlot(dgeObj = "xyz"),
                 regexp = msg)
    expect_error(cdf_plot <- cdfPlot(),
                 regexp = msg)
    expect_error(cdf_plot <- cdfPlot(dgeObj = 123),
                 regexp = msg)
    expect_error(cdfPlot(data.frame()),
                 regexp = msg)
    #contrast
    msg <- "contrast must be a singular value of class character and must be one of the top tables in the dgeObj."
    expect_error(cdf_plot <- cdfPlot(dgeObj = t_obj1),
                 regexp = msg)
    expect_error(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = NULL),
                 regexp = msg)
    expect_error(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "xyz"),
                 regexp = msg)
    expect_error(cdfPlot(dgeObj = t_obj1, contrast = 123),
                 regexp = msg)

    #plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'."
    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = "cx"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = NULL),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", plotType = c("canvasxpress","ggplot")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #pvalCol
    msg <- "pvalCol must to be a singular value of class character and must be in contrast data. Assigning default value 'P.Value'."
    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", pvalCol = "notacolumn"),
                   regexp = msg)
    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", pvalCol = c("notacolumn","abc")),
                   regexp = msg)
    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", pvalCol = NULL),
                   regexp = msg)

    #pThreshold
    msg <- "pthreshold must be a singular numeric value. Assigning default value 0.01."
    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", pThreshold = "notavalue"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", pThreshold = NULL),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", pThreshold = c(1,2)),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", pThreshold = "notavalue"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))


    #title
    msg <- "title must be a singular value of class character. Assigning default value NULL."
    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", title = c("title","title")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", title = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #viewportTitle
    msg <- "viewportTitle must be a singular value of class character. Assigning default value NULL."
    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1,
                                       contrast = "BDL_vs_Sham",
                                       viewportTitle = c("title","title")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1,
                                       contrast = "BDL_vs_Sham",
                                       viewportTitle = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'Rank' as the label."
    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", xlab = c("xlab","xlab")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", xlab = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'pvalCol' as the label."
    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", ylab = c("ylab","ylab")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", ylab = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #referenceLine
    msg <- "referenceLine must be a singular value of class character or NULL to disable. Assigning default value NULL."
    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", referenceLine = c("blue", "blue")),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", referenceLine = 1),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", referenceLine = "notavalidvalue"),
                   regexp = "Color specified is not valid. Assigning default value NULL.")
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    #viewportX
    msg <- "viewportX must be a singular value of class numeric and must be greater than 0. Assigning default value 0.15."
    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", viewportX = c(1,2), plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", viewportX = "notavalidvalue", plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", viewportX = NULL, plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    #viewportY
    msg <- "viewportY must be a singular value of class numeric and must be greater than 0. Assigning default value 0.85."
    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", viewportY = c(1,2), plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", viewportY = "notavalidvalue", plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", viewportY = NULL, plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    #viewportWidth
    msg <- "viewportWidth must be a singular value of class numeric. Assigning default value 0.35."
    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", viewportWidth = c(1,2), plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", viewportWidth = "notavalidvalue", plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", viewportWidth = NULL, plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", viewportWidth = -2, plotType = "ggplot"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$inset, c("gg", "ggplot"))
    expect_s3_class(cdf_plot$combined, c("gg", "ggplot"))

    #pvalMax
    msg <- "pvalMax must be a singular numeric value. Assigning default value 0.1."
    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", pvalMax = c(1,2)),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", pvalMax = NULL),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))

    expect_warning(cdf_plot <- cdfPlot(dgeObj = t_obj1, contrast = "BDL_vs_Sham", pvalMax = "notavalidvalue"),
                   regexp = msg)
    expect_type(cdf_plot, "list")
    expect_s3_class(cdf_plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(cdf_plot$inset, c("canvasXpress", "htmlwidget"))
})
