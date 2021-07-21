context("DGEobj.plots - tests for plotNorm.R functions")


test_that("plotNorm.R: plotNorm()", {
    ######### box test####################
    # testing with DGEobj count matrix and plotType cx - plotCategory box
    norm_plot <- plotNorm(dgeObj = t_obj1)
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    # testing with DGEobj count matrix and plotType ggplot - plotCategory box
    norm_plot <- plotNorm(dgeObj = t_obj1, plotType = "ggplot")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    # testing with DGEobj object and plotType cx - plotCategory box
    norm_plot <- plotNorm(dgeObj = t_obj1)
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    # testing with DGEobj object and plotType ggplot - plotCategory box
    norm_plot <- plotNorm(dgeObj = t_obj1, plotType = "ggplot")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    # testing with different normalization methods
    norm_plot <- plotNorm(dgeObj = t_obj1, normalize = "RLE")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(dgeObj = t_obj1, plotType = "ggplot", normalize = "RLE")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    norm_plot <- plotNorm(dgeObj = t_obj1, normalize = "upperquartile")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(dgeObj = t_obj1, plotType = "ggplot", normalize = "upperquartile")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    norm_plot <- plotNorm(dgeObj = t_obj1, normalize = "none")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(dgeObj = t_obj1, plotType = "ggplot", normalize = "none")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    ######### density test####################
    # testing with DGEobj count matrix and plotType cx - plotCategory density
    norm_plot <- plotNorm(dgeObj = t_obj1,  plotCategory = "density")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    # testing with DGEobj count matrix and plotType ggplot - plotCategory density
    norm_plot <- plotNorm(dgeObj = t_obj1, plotType = "ggplot", plotCategory = "density")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    # testing with DGEobj object and plotType cx - plotCategory density
    norm_plot <- plotNorm(dgeObj = t_obj1,  plotCategory = "density")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    # testing with DGEobj object and plotType ggplot - plotCategory density
    norm_plot <- plotNorm(dgeObj = t_obj1, plotType = "ggplot",  plotCategory = "density")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    # testing with different normalization methods
    norm_plot <- plotNorm(dgeObj = t_obj1,  plotCategory = "density", normalize = "RLE")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(dgeObj = t_obj1, plotType = "ggplot", plotCategory = "density",
                          normalize = "RLE")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    norm_plot <- plotNorm(dgeObj = t_obj1,  plotCategory = "density", normalize = "upperquartile")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(dgeObj = t_obj1, plotType = "ggplot", plotCategory = "density",
                          normalize = "upperquartile")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    norm_plot <- plotNorm(dgeObj = t_obj1,  plotCategory = "density", normalize = "none")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(dgeObj = t_obj1, plotType = "ggplot", plotCategory = "density",
                          normalize = "none")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    ######### testing assert statements ####################
    expect_error(plotNorm(),
                 regexp = "dgeObj must be specified and must belong to DGEobj class.")

    expect_error(plotNorm(NULL),
                 regexp = "dgeObj must be specified and must belong to DGEobj class.")

    expect_warning(norm_plot <- plotNorm(dgeObj = t_obj1, plotCategory = "heatmap"),
                 regexp = "plotCategory must be one of 'box' or 'density'.")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(norm_plot <- plotNorm(dgeObj = t_obj1, normalize = "xyz"),
                 regexp = "normalize must be one of 'TMM', 'RLE', 'upperquartile', or 'none'.")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(norm_plot <- plotNorm(dgeObj = t_obj1, plotType = "myplot"),
                 regexp = "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'.")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))
})
