context("DGEobj.plots - tests for plotPValHist.R functions")


test_that("plotPValHist.R: plotPvalHist()", {

    # testing plotPvalHist with facet = TRUE
    pval_plot <- plotPvalHist(dgeObj = t_obj1)
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    pval_plot <- plotPvalHist(dgeObj = t_obj1, plotType = "ggplot")
    expect_s3_class(pval_plot, c("gg","ggplot"))

    # testing plotPvalHist with facet = FALSE
    pval_plot <- plotPvalHist(dgeObj = t_obj1, facet     = FALSE)
    expect_length(pval_plot, 4)
    expect_s3_class(pval_plot[[1]], c("canvasXpress", "htmlwidget"))

    pval_plot <- plotPvalHist(dgeObj = t_obj1,
                              plotType = "ggplot",
                              facet    = FALSE)
    expect_length(pval_plot, 4)
    expect_s3_class(pval_plot[[1]], c("gg","ggplot"))

    #testing assert statements
    #dgeObj
    msg <- "dgeObj must be specified and must belong to DGEobj class."
    expect_error(pval_plot <- plotPvalHist(dgeObj = NULL),
                 regexp = msg)
    expect_error(pval_plot <- plotPvalHist(dgeObj = "xyz"),
                 regexp = msg)
    expect_error(pval_plot <- plotPvalHist(),
                 regexp = msg)
    expect_error(pval_plot <- plotPvalHist(dgeObj = 123),
                 regexp = msg)
    expect_error(plotPvalHist(data.frame()),
                 regexp = msg)
    #testing optional parameter
    #P.Val
    msg <- "P.Val must be a singular value of class character. Assigning default value 'P.Value'."
    expect_warning(pval_plot <- plotPvalHist(dgeObj = t_obj1, P.Val = NULL),
                 regexp = msg)
    expect_warning(plotPvalHist(dgeObj = t_obj1, P.Val = 123),
                 regexp = msg)

    #testing invalid optional parameters
    #binWidth
    expect_warning(pval_plot <- plotPvalHist(dgeObj = t_obj1, binWidth = 1.2),
                   regexp = "binWidth must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.02'.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(pval_plot <- plotPvalHist(dgeObj = t_obj1, binWidth = c(1,2)),
                   regexp = "binWidth must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.02'.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    #facet
    expect_warning(pval_plot <- plotPvalHist(dgeObj = t_obj1, facet = "true"),
                   regexp = "facet must be a singular logical value. Assigning default value TRUE.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(pval_plot <- plotPvalHist(dgeObj = t_obj1, facet = c(TRUE,TRUE)),
                   regexp = "facet must be a singular logical value. Assigning default value TRUE.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

})
