context("DGEobj.plots - tests for logRatioPlot.R functions")


test_that("logRatioPlot.R: logRatioPlot()", {
    #subset t_obj1
    t_obj1_subset <- subset(t_obj1, row = c(1:10))

    # Simple barplot
    plot <- logRatioPlot(dgeObj          = t_obj1_subset,
                         plotType        = "canvasXpress",
                         xColname        = "Contrast")
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ## axis free
    plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                         plotType     = "ggplot",
                         facetColname = "rgd_symbol",
                         xColname     = "Contrast",
                         title = "test")
    expect_s3_class(plot, c("gg", "ggplot"))
    ## axis free disabled
    plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                         plotType     = "ggplot",
                         facetColname = "rgd_symbol",
                         xColname     = "Contrast",
                         title = "test",
                         axisFree = FALSE)
    expect_s3_class(plot, c("gg", "ggplot"))
    # point plot with some options
    log_ratio_plot <- logRatioPlot(dgeObj            = t_obj1_subset,
                                   plotType          = "canvasXpress",
                                   plotCategory      = "point",
                                   facetColname      = "rgd_symbol",
                                   xColname          = "Contrast",
                                   axisFree          = FALSE,
                                   facet             = FALSE,
                                   title             = "Test",
                                   labelAngle        =  60)
    expect_type(log_ratio_plot, "list")
    expect_s3_class(log_ratio_plot[[1]], c("canvasXpress", "htmlwidget"))

    log_ratio_plot <- logRatioPlot(dgeObj       = t_obj1_subset,
                                   plotType     = "ggplot",
                                   plotCategory = "point",
                                   facetColname = "rgd_symbol",
                                   xColname     = "Contrast",
                                   axisFree     = FALSE,
                                   facet        = FALSE,
                                   title        = "Test",
                                   labelAngle   =  60)
    expect_type(log_ratio_plot, "list")
    expect_s3_class(log_ratio_plot[[1]], c("gg", "ggplot"))

    # Testing asserts
    ## dgeObj
    msg <- "dgeObj must be specified and should be of class DGEobj"
    expect_error(logRatioPlot(dgeObj = 1:10),
                 regexp = msg)
    expect_error(logRatioPlot(),
                 regexp = msg)
    ## complete dgeobj
    expect_warning(plot <- logRatioPlot(t_obj1,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast"),
                   regexp = "A large number of charts/facets has/have been requested and may take significant time to generate. It is suggested that less than 40 charts/facets are requested at a time.")
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))

    ## facetColname
    msg <- "facetColname must be one of geneData data columns. Setting default value 'rgd_symbol'"
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = c("rgd_symbol", "rgd_symbol"),
                                        xColname     = "Contrast"),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plott <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "abc",
                                         xColname     = "Contrast"),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = NULL,
                                        xColname     = "Contrast"),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    geneData <- DGEobj::getType(t_obj1_subset, "geneData")[[1]]
    geneData <- geneData %>% dplyr::rename(gSymbols = rgd_symbol)
    t_obj1_subset2 <- DGEobj::addItems(t_obj1_subset,
                                       list(geneData = geneData),
                                       list("geneData"),
                                       list("geneData"),
                                       overwrite = TRUE)
    expect_error(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                      plotType     = "canvasXpress",
                                      facetColname = "rgd_symbol",
                                      xColname     = "Contrast"),
                 regexp = "facetColname must be one of geneData data columns.")
    plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                        plotType     = "canvasXpress",
                        facetColname = "gSymbols",
                        xColname     = "Contrast")
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))

    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = c("rgd_symbol", "rgd_symbol"),
                                        xColname     = "Contrast"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plott <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "abc",
                                         xColname     = "Contrast"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = NULL,
                                        xColname     = "Contrast"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_error(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                      plotType     = "ggplot",
                                      facetColname = "rgd_symbol",
                                      xColname     = "Contrast"),
                 regexp = "facetColname must be one of geneData data columns.")
    plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                         plotType     = "ggplot",
                         facetColname = "gSymbols",
                         xColname     = "Contrast")
    expect_s3_class(plot, c("gg", "ggplot"))

    ## xColname
    msg <- "xColname must be single string value. Setting default value 'Contrast'."
    expect_warning(logRatioPlot(dgeObj  = t_obj1_subset,
                                facetColname = "rgd_symbol",
                                xColname = c("Contrast", "Contrast")),
                   regexp = msg)
    expect_warning(logRatioPlot(dgeObj  = t_obj1_subset,
                                facetColname = "rgd_symbol",
                                xColname = NULL),
                   regexp = msg)

    ## yColname
    msg <- "yColname must be one of toptables data columns. Setting default value 'logFC'"
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = c("Contrast", "Contrast")),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plott <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         yColname     = "abc"),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    topTables <- DGEobj::getType(t_obj1_subset, "topTable")
    topTables <- lapply(topTables, function(x) {
        x <- x %>% dplyr::rename(MyLog = logFC,
                                 MyCI.L = CI.L,
                                 MyCI.R = CI.R)
        x
    })
    t_obj1_subset2 <- DGEobj::addItems(t_obj1_subset,
                                       topTables,
                                       list("topTable", "topTable", "topTable", "topTable"),
                                       list("topTable", "topTable", "topTable", "topTable"),
                                       overwrite = TRUE)
    expect_error(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                      plotType     = "canvasXpress",
                                      facetColname = "rgd_symbol",
                                      xColname     = "Contrast"),
                 regexp = "yColname must be one of toptables data columns.")
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))

    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = c("Contrast", "Contrast")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plott <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         yColname     = "abc"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_error(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                      plotType     = "ggplot",
                                      facetColname = "rgd_symbol",
                                      xColname     = "Contrast"),
                 regexp = "yColname must be one of toptables data columns.")
    expect_s3_class(plot, c("gg", "ggplot"))

    ## CI.R_colname
    msg <- "CI.R_colname must be one of toptables data columns. Setting default value 'CI.R'"
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = c("CI.R", "CI.R")),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname =  "abc"),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = NULL),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    topTables <- DGEobj::getType(t_obj1_subset, "topTable")
    topTables <- lapply(topTables, function(x) {
        x <- x %>% dplyr::rename(MyCI.R = CI.R)
        x
    })
    t_obj1_subset2 <- DGEobj::addItems(t_obj1_subset,
                                       topTables,
                                       list("topTable", "topTable", "topTable", "topTable"),
                                       list("topTable", "topTable", "topTable", "topTable"),
                                       overwrite = TRUE)
    ## faceted bar plot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC"),
                   regexp = "CI.R_colname must be one of toptables data columns. Disabling confidenece limits",
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))

    ## faceted point plot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        plotCategory = "point"),
                   regexp = "CI.R_colname must be one of toptables data columns. Disabling confidenece limits",
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))

    ## single point plot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        plotCategory = "point",
                                        facet = FALSE),
                   regexp = "CI.R_colname must be one of toptables data columns. Disabling confidenece limits",
                   fixed = TRUE)
    expect_type(plot, "list")
    expect_s3_class(plot[[1]], c("canvasXpress", "htmlwidget"))
    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = c("CI.R", "CI.R")),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname =  "abc"),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = NULL),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC"),
                   regexp = "CI.R_colname must be one of toptables data columns. Disabling confidenece limits",
                   fixed = TRUE)
    expect_s3_class(plot, c("gg", "ggplot"))

    ## CI.L_colname
    msg <- "CI.L_colname must be one of toptables data columns. Setting default value 'CI.L'"
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = "CI.R",
                                        CI.L_colname = c("CI.L", "CI.L")),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = "CI.R",
                                        CI.L_colname =  "abc"),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = "CI.R",
                                        CI.L_colname = NULL),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    topTables <- DGEobj::getType(t_obj1_subset, "topTable")
    topTables <- lapply(topTables, function(x) {
        x <- x %>% dplyr::rename(MyCI.L = CI.L)
        x
    })
    t_obj1_subset2 <- DGEobj::addItems(t_obj1_subset,
                                       topTables,
                                       list("topTable", "topTable", "topTable", "topTable"),
                                       list("topTable", "topTable", "topTable", "topTable"),
                                       overwrite = TRUE)
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = "CI.R"),
                   regexp = "CI.L_colname must be one of toptables data columns. Disabling confidenece limits.",
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))

    ## ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = "CI.R",
                                        CI.L_colname = c("CI.L", "CI.L")),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = "CI.R",
                                        CI.L_colname =  "abc"),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = "CI.R",
                                        CI.L_colname = NULL),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = "CI.R"),
                   regexp = "CI.L_colname must be one of toptables data columns. Disabling confidenece limits.",
                   fixed = TRUE)
    expect_s3_class(plot, c("gg", "ggplot"))
    ## plotCategory
    msg <- "plotCategory must be either 'bar' or 'point'. Setting default value 'bar'"
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        plotCategory = "heatmap"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        plotCategory = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        plotCategory = 123),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        plotCategory = c("bar", "point")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    ## refLine
    msg <- "refLine must be singular logical value. Setting default value 'TRUE'."
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        refLine      = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        refLine      = 123),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        refLine      = "FALSE"),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        refLine      = c("TRUE", "FALSE")),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))

    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        refLine      = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        refLine      = 123),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        refLine      = "FALSE"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        refLine      = c("TRUE", "FALSE")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))

    ## title
    msg <- "title must be a singular value of class character. Assigning default value 'NULL'."
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        title        = 123),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        title        = c(123, 345)),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        title        = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        title        = 123),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        title        = c(123, 345)),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        title        = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    ## xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'NULL'."
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        xlab         = 123),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        xlab         = c(123, 345)),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        xlab         = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        xlab         = 123),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        xlab         = c(123, 345)),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        xlab         = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    ## ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'NULL'."
    ### cxolot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        ylab         = 123),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        ylab         = c(123, 345)),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        ylab         = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ### ggolot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        ylab         = 123),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        ylab         = c(123, 345)),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        ylab         = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))

    ## labelAngle
    msg <- "labelAngle must be a singular value of class numeric between 0 and 90. Assigning default value '45'."
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        labelAngle   = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        labelAngle   = "0.1"),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        labelAngle   = -45),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        labelAngle   = 145),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        labelAngle   = c(45, 90)),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        labelAngle   = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        labelAngle   = "0.1"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        labelAngle   = -45),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        labelAngle   = 145),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        labelAngle   = c(45, 90)),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    ## plotType
    msg <- "plotType must be either canvasXpress or ggplot. Setting default value 'canvasXpress'"
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        plotType     = NULL),
                   regexp = msg)
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        facetColname = "rgd_symbol",
                                        xColname     = "Contrast",
                                        plotType     = 123),
                   regexp = msg)
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         plotType     = c("canvasXpress", "ggplot")),
                    regexp = msg)
     ## axisFree
     msg <- "axisFree must be a singular logical value. Assigning default value TRUE."
     ### cxplot
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         axisFree     = NULL),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         axisFree     = 123),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         axisFree     = "FALSE"),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         axisFree     = c("TRUE", "FALSE")),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     ### ggplot
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         axisFree     = NULL),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         axisFree     = 123),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         axisFree     = "FALSE"),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         axisFree     = c("TRUE", "FALSE")),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))

     ## facet
     msg <- "facet must be singular logical value. Setting default value 'TRUE'."
     ### cxplot
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         facet     = NULL),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         facet     = 123),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         facet     = "FALSE"),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         facet     = c("TRUE", "FALSE")),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     ### ggplot
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         facet     = NULL),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         facet     = 123),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         facet     = "FALSE"),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "rgd_symbol",
                                         xColname     = "Contrast",
                                         facet     = c("TRUE", "FALSE")),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))
})
