context("DGEobj.plots - tests for plotDispersion.R functions")


test_that("plotDispersion.R: plotDispersion()", {
    skip_if(!all(c("DGEList","counts") %in% names(t_obj1)))

    # Testing dispersion plots with input DGEobj
    plot_disp <- plotDispersion(dgeObj  = t_obj1)
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj   = t_obj1,
                                plotType = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    # Testing dispersion plots with counts data.
    plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                countsMatrix = FALSE)
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                plotType     = "ggplot",
                                countsMatrix = FALSE)
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    #Testing BCV plots with input DGEobj.
    plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                plotCategory = "BCV")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                plotCategory = "BCV",
                                plotType     = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    #Testing BCV plots with counts data
    plot_disp <- plotDispersion(dgeObj        = t_obj1,
                                countsMatrix  = FALSE,
                                plotCategory  = "BCV")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj        = t_obj1,
                                plotType      = "ggplot",
                                countsMatrix  = FALSE,
                                plotCategory  = "BCV")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    #Testing parameter - LineFit
    plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                lineFit      = "loess")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                lineFit      = "loess",
                                plotType     = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                lineFit      = "lm")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                lineFit      = "glm")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(dgeObj       = t_obj1,
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

    #No countsMatrix
    msg <- "dgeObj needs to have exactly one counts matrix."
    t_obj_no_counts <- DGEobj::rmItem(t_obj1, "counts")
    expect_error(plotDispersion(dgeObj = t_obj_no_counts),
                 regexp = msg)

    #No DGEList
    msg <- "dgeObj needs to have exactly one DGEList."
    t_obj_no_DGEList <- DGEobj::rmItem(t_obj1, "DGEList")
    expect_error(plotDispersion(dgeObj = t_obj_no_DGEList,
                                countsMatrix = FALSE),
                 regexp = msg)
    #counts
    msg = "countsMatrix must be a singular logical value. Assigning default value TRUE."
    expect_warning(plotDispersion(dgeObj        = t_obj1,
                                  countsMatrix  = NULL),
                   regexp = msg)
    expect_warning(plotDispersion(dgeObj        = t_obj1,
                                  countsMatrix  = "123"),
                   regexp = msg)
    expect_warning(plotDispersion(dgeObj        = t_obj1,
                                  countsMatrix  = c(123,234)),
                   regexp = msg)
    expect_warning(plotDispersion(dgeObj = t_obj1,
                                  countsMatrix  = "xyz"),
                   regexp = msg)

    ## ReplicateGroupCol
    msg <- "replicateGroupCol must be a singular value of class character and must be a column name in design data. Assigning default value 'ReplicateGroup'."
    expect_warning(plotDispersion(dgeObj  = t_obj1,
                                  replicateGroupCol = "xyz"),
                   regexp = msg)

    expect_warning(plotDispersion(dgeObj  = t_obj1,
                                  replicateGroupCol = NULL),
                   regexp = msg)

    expect_warning(plotDispersion(dgeObj  = t_obj1,
                                  replicateGroupCol = 123),
                   regexp = msg)

    expect_warning(plotDispersion(dgeObj  = t_obj1,
                                  replicateGroupCol = c("xyz", "abc")),
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

    expect_warning(plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                               lineFit      = 123),
                   regexp = "lineFit must be one value from 'glm', 'lm', 'loess', 'gam' or 'NULL' to disable. Assigning default value 'NULL'")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_warning(plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                               lineFit      = "abc",
                                               plotType     = "ggplot"),
                   regexp = "lineFit must be one value from 'glm', 'lm', 'loess', 'gam' or 'NULL' to disable. Assigning default value 'NULL'")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_warning(plot_disp <- plotDispersion(dgeObj       = t_obj1,
                                               lineFit      = 123,
                                               plotType     = "ggplot"),
                   regexp = "lineFit must be one value from 'glm', 'lm', 'loess', 'gam' or 'NULL' to disable. Assigning default value 'NULL'")
    expect_s3_class(plot_disp, c("gg", "ggplot"))
})
