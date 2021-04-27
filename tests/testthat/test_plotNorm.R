context("DGEobj.plots - tests for plotNorm.R functions")


test_that("plotNorm.R: plotNorm()", {
    ######### box test####################
    # testing with DGEobj count matrix and plotType cx - plotCategory box
    norm_plot <- plotNorm(t_obj1$counts)
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    # testing with DGEobj count matrix and plotType ggplot - plotCategory box
    norm_plot <- plotNorm(t_obj1$counts, plotType = "ggplot")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    # testing with DGEobj object and plotType cx - plotCategory box
    norm_plot <- plotNorm(t_obj1)
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    # testing with DGEobj object and plotType ggplot - plotCategory box
    norm_plot <- plotNorm(t_obj1, plotType = "ggplot")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    # testing with different normalization methods
    norm_plot <- plotNorm(t_obj1$counts, normalize = "RLE")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(t_obj1, plotType = "ggplot", normalize = "RLE")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    norm_plot <- plotNorm(t_obj1$counts, normalize = "upperquartile")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(t_obj1, plotType = "ggplot", normalize = "upperquartile")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    norm_plot <- plotNorm(t_obj1$counts, normalize = "none")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(t_obj1, plotType = "ggplot", normalize = "none")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    ######### density test####################
    # testing with DGEobj count matrix and plotType cx - plotCategory density
    norm_plot <- plotNorm(t_obj1$counts,  plotCategory = "density")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    # testing with DGEobj count matrix and plotType ggplot - plotCategory density
    norm_plot <- plotNorm(t_obj1$counts, plotType = "ggplot", plotCategory = "density")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    # testing with DGEobj object and plotType cx - plotCategory density
    norm_plot <- plotNorm(t_obj1,  plotCategory = "density")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    # testing with DGEobj object and plotType ggplot - plotCategory density
    norm_plot <- plotNorm(t_obj1, plotType = "ggplot",  plotCategory = "density")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    # testing with different normalization methods
    norm_plot <- plotNorm(t_obj1$counts,  plotCategory = "density", normalize = "RLE")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(t_obj1$counts, plotType = "ggplot", plotCategory = "density",
                          normalize = "RLE")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    norm_plot <- plotNorm(t_obj1$counts,  plotCategory = "density", normalize = "upperquartile")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(t_obj1$counts, plotType = "ggplot", plotCategory = "density",
                          normalize = "upperquartile")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    norm_plot <- plotNorm(t_obj1$counts,  plotCategory = "density", normalize = "none")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(t_obj1$counts, plotType = "ggplot", plotCategory = "density",
                          normalize = "none")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    ######### testing assert statements ####################
    expect_error(plotNorm(),
                 regexp = "DGEdata must be of either class 'matrix' or 'DGEobj'.")
    expect_error(plotNorm(NULL),
                 regexp = "DGEdata must be of either class 'matrix' or 'DGEobj'.")
    expect_error(plotNorm(t_obj1, plotCategory = "heatmap"),
                 regexp = "plotCategory must be one of 'box' or 'density'.")
    expect_error(plotNorm(t_obj1, normalize = "xyz"),
                 regexp = "normalize must be one of 'TMM', 'RLE', 'upperquartile', or 'none'.")
    expect_error(plotNorm(t_obj1, plotType = "myplot"),
                 regexp = "plotType must be either canvasXpress or ggplot.")
})
