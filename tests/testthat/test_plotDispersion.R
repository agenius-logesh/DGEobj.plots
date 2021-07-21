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


    #Testing lineFit parameters
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
                                               lineFit      = 123),
                   regexp = "lineFit must be one value from 'glm', 'lm', 'loess', 'gam' or 'NULL' to disable. Assigning default value 'NULL'")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               lineFit      = "abc",
                                               plotType     = "ggplot"),
                   regexp = "lineFit must be one value from 'glm', 'lm', 'loess', 'gam' or 'NULL' to disable. Assigning default value 'NULL'")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_warning(plot_disp <- plotDispersion(DGEdata      = dgelist,
                                               designMatrix = designMatrix,
                                               lineFit      = 123,
                                               plotType     = "ggplot"),
                   regexp = "lineFit must be one value from 'glm', 'lm', 'loess', 'gam' or 'NULL' to disable. Assigning default value 'NULL'")
    expect_s3_class(plot_disp, c("gg", "ggplot"))
})
