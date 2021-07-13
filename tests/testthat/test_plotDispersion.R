context("DGEobj.plots - tests for plotDispersion.R functions")


test_that("plotDispersion.R: plotDispersion()", {
    skip_if(is.null(t_obj1$DGEList))

    # creating designMatrix and dgelist
    dgelist <- t_obj1$DGEList
    designMatrix <- stats::model.matrix(~ 0 + ReplicateGroup, getItem(t_obj1, "design"))

    # Testing dispersion plots with input DGEList
    plot_disp <- plotDispersion(DGEdata      = dgelist,
                                designMatrix = designMatrix)
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata      = dgelist,
                                designMatrix = designMatrix,
                                plotType     = "canvasXpress")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata      = dgelist,
                                designMatrix = designMatrix,
                                plotType     = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    # Testing dispersion plots with input countMatrix
    plot_disp <- plotDispersion(DGEdata       = t_obj1$counts,
                                designMatrix  = designMatrix)
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata       = t_obj1$counts,
                                plotType      = "ggplot",
                                designMatrix  = designMatrix)
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    #Testing BCV plots with input DGEList
    plot_disp <- plotDispersion(DGEdata      = dgelist,
                                designMatrix = designMatrix,
                                plotCategory = "BCV")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata      = dgelist,
                                designMatrix = designMatrix,
                                plotCategory = "BCV",
                                plotType     = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    #Testing BCV plots with input countMatrix
    plot_disp <- plotDispersion(DGEdata       = t_obj1$counts,
                                designMatrix  = designMatrix,
                                plotCategory  = "BCV")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata       = t_obj1$counts,
                                plotType      = "ggplot",
                                designMatrix  = designMatrix,
                                plotCategory  = "BCV")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    #Testing parameter - LineFit
    plot_disp <- plotDispersion(DGEdata      = dgelist,
                                designMatrix = designMatrix,
                                lineFit      = "loess")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata      = dgelist,
                                designMatrix = designMatrix,
                                lineFit      = "loess",
                                plotType     = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))


    #Testing symbol parameters
    plot_disp <- plotDispersion(DGEdata       = t_obj1$counts,
                                designMatrix  = designMatrix,
                                symbolSize    = 10,
                                symbolShape   = "triangle",
                                symbolColor   = "red",
                                symbolTransparency = 0.2)
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata       = t_obj1$counts,
                                plotType      = "ggplot",
                                designMatrix  = designMatrix,
                                plotCategory  = "BCV",
                                symbolSize    = 3,
                                symbolShape   = "square",
                                symbolColor  = "red",
                                symbolTransparency = 0.2)
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    #Testing lineFit parameters
    plot_disp <- plotDispersion(DGEdata      = dgelist,
                                designMatrix = designMatrix,
                                lineFit      = "loess",
                                lineType     = "dotted",
                                linefitColor = "yellow")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata      = dgelist,
                                designMatrix = designMatrix,
                                lineFit      = "loess",
                                lineType     = "dotted",
                                plotType     = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    plot_disp <- plotDispersion(DGEdata      = dgelist,
                                designMatrix = designMatrix,
                                lineFit      = "loess",
                                lineType     = "longdash",
                                linefitColor = "yellow",
                                plotType     = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    plot_disp <- plotDispersion(DGEdata      = dgelist,
                                designMatrix = designMatrix,
                                lineFit      = "lm")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata      = dgelist,
                                designMatrix = designMatrix,
                                lineFit      = "glm")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata      = dgelist,
                                designMatrix = designMatrix,
                                lineFit      = "gam")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    # testing assert statements
    expect_error(plotDispersion(),
                 regexp = "Both DGEdata and designMatrix must be specified.")
    expect_error(plotDispersion(DGEdata = t_obj1$counts),
                 regexp = "Both DGEdata and designMatrix must be specified.")
    expect_warning(plotDispersion(DGEdata = t_obj1$counts, designMatrix = designMatrix, plotCategory = "cx"),
                 regexp = "plotCategory must be either dispersion or bcv. Assigning default value 'dispersion'.")
    expect_warning(plotDispersion(DGEdata = t_obj1$counts, designMatrix = designMatrix, plotType = "cx"),
                 regexp = "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'.")

    #testing warning messages
    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               lineFit   = "abc"),
                   regexp = "lineFit must be one value from 'glm', 'lm', 'loess', 'gam' or 'NULL' to disable. Assigning default value 'NULL'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               symbolSize   = "a"),
                   regexp = "symbolSize must be a singular numeric value. Assigning a default value of 6.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               symbolSize   = c(1,2)),
                   regexp = "symbolSize must be a singular numeric value. Assigning a default value of 6.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               symbolShape  = 1),
                   regexp = "symbolShape must be a singular value of class 'character'. Assigning default value = 'circle'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               symbolShape  = c(1,2)),
                   regexp = "symbolShape must be a singular value of class 'character'. Assigning default value = 'circle'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               symbolColor  = c(1,2)),
                   regexp = "symbolColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'darkblue'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               symbolColor  = 1),
                   regexp = "symbolColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'darkblue'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               symbolTransparency  = 1.2),
                   regexp = "symbolTransparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.5'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               symbolTransparency  = c(1,2)),
                   regexp = "symbolTransparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.5'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               symbolShape  = c(1,2),
                                               plotType     = "ggplot"),
                   regexp = "symbolShape must be a singular value of class 'character'. Assigning default value = 'circle'.")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               plotType     = "ggplot",
                                               symbolTransparency  = 1.2),
                   regexp = "symbolTransparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.5'.")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               symbolSize   = 'a',
                                               plotType     = "ggplot"),
                   regex = "symbolSize must be a singular numeric value. Assigning a default value of 6.")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               symbolColor  = 1,
                                               plotType     = "ggplot"),
                   regex = "symbolColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'darkblue'.")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               lineFit      = "loess",
                                               lineType     = 4),
                   regexp = "lineType must be a singular value of class character. Refer help section for the list of line types supported. Assigning default value 'solid'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               lineFit      = "loess",
                                               linefitColor = 4),
                   regexp = "linefitColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'red'.")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               lineFit      = "loess",
                                               lineType     = c(2,3),
                                               plotType     = "ggplot"),
                   regexp = "lineType must be a singular value of class character. Refer help section for the list of line types supported. Assigning default value 'solid'.")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               lineFit      = "loess",
                                               linefitColor = 4,
                                               plotType     = "ggplot"),
                   regexp = "linefitColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'red'.")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               lineFit      = "loess",
                                               linefitColor = c(2,3),
                                               plotType     = "ggplot"),
                   regexp = "linefitColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'red'.")
    expect_s3_class(plot_disp, c("gg", "ggplot"))
})
