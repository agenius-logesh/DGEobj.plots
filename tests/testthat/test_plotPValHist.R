context("DGEobj.plots - tests for plotPValHist.R functions")


test_that("plotPValHist.R: plotPvalHist()", {

    # testing plotPvalHist with facet = TRUE
    DGEdata = t_obj1
    pval_plot <- plotPvalHist(DGEdata)
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    pval_plot <- plotPvalHist(DGEdata, plotType = "ggplot")
    expect_s3_class(pval_plot, c("gg","ggplot"))

    # testing plotPvalHist with facet = FALSE
    pval_plot <- plotPvalHist(DGEdata, facet     = FALSE)
    expect_length(pval_plot, 4)
    expect_s3_class(pval_plot[[1]], c("canvasXpress", "htmlwidget"))

    pval_plot <- plotPvalHist(DGEdata,
                              plotType = "ggplot",
                              facet    = FALSE)
    expect_length(pval_plot, 4)
    expect_s3_class(pval_plot[[1]], c("gg","ggplot"))

    #testing assert statements
    #DGEdata
    msg <- "DGEdata must be specified as class of DGEobj."
    expect_error(pval_plot <- plotPvalHist(DGEdata = NULL),
                 regexp = msg)
    expect_error(pval_plot <- plotPvalHist(DGEdata = "xyz"),
                 regexp = msg)
    expect_error(pval_plot <- plotPvalHist(),
                 regexp = msg)
    expect_error(pval_plot <- plotPvalHist(DGEdata = 123),
                 regexp = msg)
    expect_error(plotPvalHist(data.frame()),
                 regexp = msg)
    #testing optional parameter
    #P.Val
    msg <- "P.Val must be a singular value of class character. Assigning default value 'P.Value'."
    expect_warning(pval_plot <- plotPvalHist(DGEdata, P.Val = NULL),
                 regexp = msg)
    expect_warning(plotPvalHist(DGEdata, P.Val = 123),
                 regexp = msg)

    pval_plot <- plotPvalHist(DGEdata, binWidth = 0.2, color = "red", transparency = 0.2)
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    pval_plot <- plotPvalHist(DGEdata, plotType = "ggplot", binWidth = 0.2, color = "red", transparency = 0.2)
    expect_s3_class(pval_plot, c("gg","ggplot"))

    #testing invalid optional parameters

    #binWidth
    expect_warning(pval_plot <- plotPvalHist(DGEdata, binWidth = 1.2),
                   regexp = "binWidth must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.02'.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(pval_plot <- plotPvalHist(DGEdata, binWidth = c(1,2)),
                   regexp = "binWidth must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.02'.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    #color
    expect_warning(pval_plot <- plotPvalHist(DGEdata, color = c(1,2)),
                   regexp = "color must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'dodgerblue3'.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(pval_plot <- plotPvalHist(DGEdata, color = 1),
                   regexp = "color must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'dodgerblue3'.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    #facet
    expect_warning(pval_plot <- plotPvalHist(DGEdata, facet = "true"),
                   regexp = "facet must be a singular logical value. Assigning default value TRUE.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(pval_plot <- plotPvalHist(DGEdata, facet = c(TRUE,TRUE)),
                   regexp = "facet must be a singular logical value. Assigning default value TRUE.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    #transparency
    expect_warning(pval_plot <- plotPvalHist(DGEdata, transparency = 1.2),
                   regexp = "transparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.6'.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(pval_plot <- plotPvalHist(DGEdata, transparency = c(1,2)),
                   regexp = "transparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.6'.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))
})
