context("DGEobj.plots - tests for plotDispersion.R functions")


test_that("plotDispersion.R: plotDispersion()", {
    skip_if(is.null(t_obj1))

    # Testing dispersion plots with input DGEobj
    plot_disp <- plotDispersion(dgeObj  = t_obj1)
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj  = t_obj1,
                                plotType = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    # Testing dispersion plots with counts data.
    plot_disp <- plotDispersion(dgeObj    = t_obj1,
                                counts     = TRUE)
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj    = t_obj1,
                                plotType   = "ggplot",
                                counts     = TRUE)
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    #Testing BCV plots with input DGEobj.
    plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                plotCategory = "BCV")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                plotCategory = "BCV",
                                plotType     = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    #Testing BCV plots with counts data
    plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                counts        = TRUE,
                                plotCategory  = "BCV")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                plotType      = "ggplot",
                                counts        = TRUE,
                                plotCategory  = "BCV")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    #Testing parameter - LineFit
    plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                lineFit      = "loess")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                lineFit      = "loess",
                                plotType     = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))


    #Testing symbol parameters
    plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                counts        = TRUE,
                                symbolSize    = 10,
                                symbolShape   = "triangle",
                                symbolColor   = "red",
                                symbolTransparency = 0.2)
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                plotType      = "ggplot",
                                counts        = TRUE,
                                plotCategory  = "BCV",
                                symbolSize    = 3,
                                symbolShape   = "square",
                                symbolColor  = "red",
                                symbolTransparency = 0.2)
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    #Testing lineFit parameters
    plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                lineFit      = "loess",
                                lineType     = "dotted",
                                linefitColor = "yellow")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                lineFit      = "loess",
                                lineType     = "dotted",
                                plotType     = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                lineFit      = "loess",
                                lineType     = "longdash",
                                linefitColor = "yellow",
                                plotType     = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                lineFit      = "lm")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                lineFit      = "glm")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                lineFit      = "gam")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    # testing assert statements
    #dgeObj
    msg <- "dgeObj must be specified and must belong to DGEobj class."
    expect_error(plotDispersion(dgeObj = NULL),
                 regexp = msg)
    expect_error(plotDispersion(dgeObj = "xyz"),
                 regexp = msg)
    expect_error(plotDispersion(),
                 regexp = msg)
    expect_error(plotDispersion(dgeObj = 123),
                 regexp = msg)
    expect_error(plotDispersion(data.frame()),
                 regexp = msg)
    #counts
    msg = "counts must be a singular logical value. Assigning default value FALSE."
    expect_warning(plotDispersion(dgeObj = t_obj1, counts = NULL),
                   regexp = msg)
    expect_warning(plotDispersion(dgeObj = t_obj1, counts = "123"),
                   regexp = msg)
    expect_warning(plotDispersion(dgeObj = t_obj1, counts = c(123,234)),
                   regexp = msg)
    expect_warning(plotDispersion(dgeObj = t_obj1, counts = "xyz"),
                   regexp = msg)

    ## ReplicateGroupCol
    msg <- "ReplicateGroupCol to be a singular value of class character and must be in design data. Assigning default value 'ReplicateGroup'."
    expect_warning(plotDispersion(dgeObj  = t_obj1,
                                  ReplicateGroupCol = "xyz"),
                   regexp = msg)

    expect_warning(plotDispersion(dgeObj  = t_obj1,
                                  ReplicateGroupCol = NULL),
                   regexp = msg)

    expect_warning(plotDispersion(dgeObj  = t_obj1,
                                  ReplicateGroupCol = 123),
                   regexp = msg)

    expect_warning(plotDispersion(dgeObj  = t_obj1,
                                  ReplicateGroupCol = c("xyz", "abc")),
                   regexp = msg)

    ## plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'."
    expect_warning(plotDispersion(dgeObj  = t_obj1,
                                  plotType = "xyz"),
                   regexp = msg)

    expect_warning(plotDispersion(dgeObj  = t_obj1,
                                  plotType = NULL),
                   regexp = msg)

    expect_warning(plotDispersion(dgeObj  = t_obj1,
                                  plotType = 123),
                   regexp = msg)

    expect_warning(plotDispersion(dgeObj  = t_obj1,
                                  plotType = c("canvasXpress", "ggplot")),
                   regexp = msg)

    #plotCategory
    msg = "plotCategory must be either dispersion or bcv. Assigning default value 'dispersion'."
    expect_warning(plotDispersion(dgeObj = t_obj1, plotCategory = NULL),
                   regexp = msg)
    expect_warning(plotDispersion(dgeObj = t_obj1, plotCategory = "123"),
                   regexp = msg)
    expect_warning(plotDispersion(dgeObj = t_obj1, plotCategory = c(123,234)),
                   regexp = msg)
    expect_warning(plotDispersion(dgeObj = t_obj1, plotCategory = "xyz"),
                   regexp = msg)

    #testing warning messages
    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               lineFit   = "abc"),
                   regexp = "lineFit must be one value from 'glm', 'lm', 'loess', 'gam' or 'NULL' to disable. Assigning default value 'NULL'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               symbolSize   = "a"),
                   regexp = "symbolSize must be a singular numeric value. Assigning a default value of 6.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               symbolSize   = c(1,2)),
                   regexp = "symbolSize must be a singular numeric value. Assigning a default value of 6.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               symbolShape  = 1),
                   regexp = "symbolShape must be a singular value of class 'character'. Assigning default value = 'circle'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               symbolShape  = c(1,2)),
                   regexp = "symbolShape must be a singular value of class 'character'. Assigning default value = 'circle'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               symbolColor  = c(1,2)),
                   regexp = "symbolColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'darkblue'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               symbolColor  = 1),
                   regexp = "symbolColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'darkblue'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               symbolTransparency  = 1.2),
                   regexp = "symbolTransparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.5'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               symbolTransparency  = c(1,2)),
                   regexp = "symbolTransparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.5'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               symbolShape  = c(1,2),
                                               plotType     = "ggplot"),
                   regexp = "symbolShape must be a singular value of class 'character'. Assigning default value = 'circle'.")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               plotType     = "ggplot",
                                               symbolTransparency  = 1.2),
                   regexp = "symbolTransparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.5'.")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               symbolSize   = 'a',
                                               plotType     = "ggplot"),
                   regex = "symbolSize must be a singular numeric value. Assigning a default value of 6.")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               symbolColor  = 1,
                                               plotType     = "ggplot"),
                   regex = "symbolColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'darkblue'.")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               lineFit      = "loess",
                                               lineType     = 4),
                   regexp = "lineType must be a singular value of class character. Refer help section for the list of line types supported. Assigning default value 'solid'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               lineFit      = "loess",
                                               linefitColor = 4),
                   regexp = "linefitColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'red'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               lineFit      = "loess",
                                               lineType     = c(2,3),
                                               plotType     = "ggplot"),
                   regexp = "lineType must be a singular value of class character. Refer help section for the list of line types supported. Assigning default value 'solid'.")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               lineFit      = "loess",
                                               linefitColor = 4,
                                               plotType     = "ggplot"),
                   regexp = "linefitColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'red'.")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_warning(plot_disp <- plotDispersion(dgeObj      = t_obj1,
                                               lineFit      = "loess",
                                               linefitColor = c(2,3),
                                               plotType     = "ggplot"),
                   regexp = "linefitColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'red'.")
    expect_s3_class(plot_disp, c("gg", "ggplot"))
})
