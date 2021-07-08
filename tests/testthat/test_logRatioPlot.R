context("DGEobj.plots - tests for logRatioPlot.R functions")


test_that("logRatioPlot.R: logRatioPlot()", {
    #subset t_obj1
    t_obj1_subset <- subset(t_obj1, row = c(1:10))

    # Simple barplot
    plot <- logRatioPlot(dgeObj          = t_obj1_subset,
                         plotType        = "canvasXpress",
                         facetColname    = "GeneSymbol",
                         xColname        = "Contrast",
                         barColor        = "blue",
                         barTransparency = 0.5,
                         facetCol        = 3)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ## axis free
    plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                         plotType     = "ggplot",
                         facetColname = "GeneSymbol",
                         xColname     = "Contrast",
                         facetCol     = 3,
                         title = "test")
    expect_s3_class(plot, c("gg", "ggplot"))
    ## axis free disabled
    plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                         plotType     = "ggplot",
                         facetColname = "GeneSymbol",
                         xColname     = "Contrast",
                         facetCol     = 3,
                         title = "test",
                         axisFree = FALSE)
    expect_s3_class(plot, c("gg", "ggplot"))
    # point plot with some options
    log_ratio_plot <- logRatioPlot(dgeObj            = t_obj1_subset,
                                   plotType          = "canvasXpress",
                                   plotCategory      = "point",
                                   facetColname      = "GeneSymbol",
                                   xColname          = "Contrast",
                                   facetCol          = 4,
                                   axisFree          = FALSE,
                                   facet             = FALSE,
                                   title             = "Test",
                                   pointSize         = 4,
                                   pointColor        = "red",
                                   pointTransparency = 0.4,
                                   labelAngle        =  60)
    expect_type(log_ratio_plot, "list")
    expect_s3_class(log_ratio_plot[[1]], c("canvasXpress", "htmlwidget"))

    log_ratio_plot <- logRatioPlot(dgeObj       = t_obj1_subset,
                                   plotType     = "ggplot",
                                   plotCategory = "point",
                                   facetColname = "GeneSymbol",
                                   xColname     = "Contrast",
                                   facetCol     = 4,
                                   axisFree     = FALSE,
                                   facet        = FALSE,
                                   title        = "Test",
                                   pointSize    = 4,
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
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast"),
                   regexp = "A large number of charts/facets has/have been requested and may take significant time to generate. It is suggested that less than 40 charts/facets are requested at a time.")
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))

    ## facetColname
    msg <- "facetColname must be one of toptables data columns."
    expect_error(logRatioPlot(dgeObj  = t_obj1_subset,
                              facetColname = c("GeneSymbol", "GeneSymbol")),
                   regexp = msg)
    expect_error(logRatioPlot(dgeObj  = t_obj1_subset,
                              facetColname = "abc"),
                 regexp = msg)
    expect_error(logRatioPlot(dgeObj  = t_obj1_subset,
                              facetColname = NULL),
                 regexp = msg)
    expect_error(logRatioPlot(dgeObj  = t_obj1_subset),
                 regexp = msg)

    ## xColname
    msg <- "xColname must be one of toptables columns."
    expect_error(logRatioPlot(dgeObj  = t_obj1_subset,
                              facetColname = "GeneSymbol",
                              xColname = c("Contrast", "Contrast")),
                 regexp = msg)
    expect_error(logRatioPlot(dgeObj  = t_obj1_subset,
                              facetColname = "GeneSymbol",
                              xColname = "abc"),
                 regexp = msg)
    expect_error(logRatioPlot(dgeObj  = t_obj1_subset,
                              facetColname = "GeneSymbol",
                              xColname = NULL),
                 regexp = msg)
    expect_error(logRatioPlot(dgeObj = t_obj1_subset,
                              facetColname = "GeneSymbol"),
                 regexp = msg)

    ## yColname
    msg <- "yColname must be one of toptables data columns. Setting default value 'logFC'"
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = c("Contrast", "Contrast")),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plott <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         yColname     = "abc"),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
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
                                      facetColname = "GeneSymbol",
                                      xColname     = "Contrast"),
                 regexp = "yColname must be one of toptables data columns.")
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))

    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = c("Contrast", "Contrast")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plott <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         yColname     = "abc"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_error(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                      plotType     = "ggplot",
                                      facetColname = "GeneSymbol",
                                      xColname     = "Contrast"),
                 regexp = "yColname must be one of toptables data columns.")
    expect_s3_class(plot, c("gg", "ggplot"))

    ## CI.R_colname
    msg <- "CI.R_colname must be one of toptables data columns. Setting default value 'CI.R'"
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = c("CI.R", "CI.R")),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname =  "abc"),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
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
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC"),
                   regexp = "CI.R_colname must be one of toptables data columns. Disabling confidenece limits",
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))

    ## faceted point plot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        plotCategory = "point"),
                   regexp = "CI.R_colname must be one of toptables data columns. Disabling confidenece limits",
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))

    ## single point plot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
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
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = c("CI.R", "CI.R")),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname =  "abc"),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = NULL),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
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
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = "CI.R",
                                        CI.L_colname = c("CI.L", "CI.L")),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = "CI.R",
                                        CI.L_colname =  "abc"),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
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
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = "CI.R"),
                   regexp = "CI.L_colname must be one of toptables data columns. Disabling confidenece limits.",
                   fixed = TRUE)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))

    ## ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = "CI.R",
                                        CI.L_colname = c("CI.L", "CI.L")),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = "CI.R",
                                        CI.L_colname =  "abc"),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        yColname     = "logFC",
                                        CI.R_colname = "CI.R",
                                        CI.L_colname = NULL),
                   regexp = msg,
                   fixed = TRUE)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset2,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
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
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        facetCol     = 2,
                                        plotCategory = "heatmap"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        facetCol     = 2,
                                        plotCategory = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        facetCol     = 2,
                                        plotCategory = 123),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        facetCol     = 2,
                                        plotCategory = c("bar", "point")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    ## refLine
    msg <- "refLine must be singular logical value. Setting default value 'TRUE'."
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLine      = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLine      = 123),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLine      = "FALSE"),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLine      = c("TRUE", "FALSE")),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))

    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLine      = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLine      = 123),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLine      = "FALSE"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLine      = c("TRUE", "FALSE")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))

    ## refLineColor
    msg <- "refLineColor must be a singular value of class character. Assigning default value 'red'."

    # cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLineColor = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLineColor = 123),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLineColor = "FALSE"),
                   regexp = "Color specified is not valid. Assigning default value 'red'.")
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLineColor = c("red", "red")),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))

    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLineColor = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLineColor = 123),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLineColor = "FALSE"),
                   regexp = "Color specified is not valid. Assigning default value 'red'.")
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        refLineColor = c("red", "red")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))

    ## title
    msg <- "title must be a singular value of class character. Assigning default value 'NULL'."
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        title        = 123),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        title        = c(123, 345)),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        title        = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        title        = 123),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        title        = c(123, 345)),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        title        = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    ## xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'NULL'."
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        xlab         = 123),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        xlab         = c(123, 345)),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        xlab         = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        xlab         = 123),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        xlab         = c(123, 345)),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        xlab         = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    ## ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'NULL'."
    ### cxolot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        ylab         = 123),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        ylab         = c(123, 345)),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        ylab         = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ### ggolot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        ylab         = 123),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        ylab         = c(123, 345)),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        ylab         = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))

    ## barColor
    msg <- "barColor must be a singular value of class character. Assigning default value 'dodgerblue4'."
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barColor     = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barColor     = 123),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barColor     = "abc"),
                   regexp = "Color specified is not valid. Assigning default value 'dodgerblue4'.")
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barColor    = c("dodgerblue4", "dodgerblue4")),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barColor     = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barColor     = 123),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barColor     = "abc"),
                   regexp = "Color specified is not valid. Assigning default value 'dodgerblue4'.")
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barColor    = c("dodgerblue4", "dodgerblue4")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))

    ## barTransparency
    msg <- "barTransparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '1'."
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj     = t_obj1_subset,
                                        plotType        = "canvasXpress",
                                        facetColname    = "GeneSymbol",
                                        xColname        = "Contrast",
                                        barTransparency = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barTransparency     = "0.9"),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barTransparency     = c(0.9, 0.9)),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barTransparency     = 0),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barTransparency     = 9),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj     = t_obj1_subset,
                                        plotType        = "ggplot",
                                        facetColname    = "GeneSymbol",
                                        xColname        = "Contrast",
                                        barTransparency = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barTransparency     = "0.9"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barTransparency     = c(0.9, 0.9)),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barTransparency     = 0),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        barTransparency     = 9),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))

    ## pointColor
    msg <- "pointColor must be a singular value of class character. Assigning default value 'dodgerblue4'."
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointColor   = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointColor   = 123),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointColor   = "abc"),
                   regexp = "Color specified is not valid. Assigning default value 'dodgerblue4'.")
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointColor   = c("dodgerblue4", "dodgerblue4")),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointColor   = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointColor   = 123),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointColor   = "abc"),
                   regexp = "Color specified is not valid. Assigning default value 'dodgerblue4'.")
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointColor   = c("dodgerblue4", "dodgerblue4")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))

    ## pointShape
    msg <- "pointShape must be a singular charcter values. Assigning default values 'circle'."
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointShape   = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointShape   = 123),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointShape   = "abc"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointShape   = c("circle", "circle")),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    ## pointTransparency
    msg <- "pointTransparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '1'."
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj       = t_obj1_subset,
                                        plotType          = "canvasXpress",
                                        facetColname      = "GeneSymbol",
                                        xColname          = "Contrast",
                                        plotCategory = "point",
                                        pointTransparency = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj       = t_obj1_subset,
                                        plotType          = "canvasXpress",
                                        facetColname      = "GeneSymbol",
                                        xColname          = "Contrast",
                                        plotCategory = "point",
                                        pointTransparency = "0.9"),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj       = t_obj1_subset,
                                        plotType          = "canvasXpress",
                                        facetColname      = "GeneSymbol",
                                        xColname          = "Contrast",
                                        plotCategory = "point",
                                        pointTransparency = c(0.9, 0.9)),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj       = t_obj1_subset,
                                        plotType          = "canvasXpress",
                                        facetColname      = "GeneSymbol",
                                        xColname          = "Contrast",
                                        plotCategory = "point",
                                        pointTransparency = 0),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj       = t_obj1_subset,
                                        plotType          = "canvasXpress",
                                        facetColname      = "GeneSymbol",
                                        xColname          = "Contrast",
                                        plotCategory = "point",
                                        pointTransparency = 9),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj       = t_obj1_subset,
                                        plotType          = "ggplot",
                                        facetColname      = "GeneSymbol",
                                        xColname          = "Contrast",
                                        plotCategory = "point",
                                        pointTransparency = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj       = t_obj1_subset,
                                        plotType          = "ggplot",
                                        facetColname      = "GeneSymbol",
                                        xColname          = "Contrast",
                                        plotCategory = "point",
                                        pointTransparency = "0.9"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj       = t_obj1_subset,
                                        plotType          = "ggplot",
                                        facetColname      = "GeneSymbol",
                                        xColname          = "Contrast",
                                        plotCategory = "point",
                                        pointTransparency = c(0.9, 0.9)),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj       = t_obj1_subset,
                                        plotType          = "ggplot",
                                        facetColname      = "GeneSymbol",
                                        xColname          = "Contrast",
                                        plotCategory = "point",
                                        pointTransparency = 0),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj       = t_obj1_subset,
                                        plotType          = "ggplot",
                                        facetColname      = "GeneSymbol",
                                        xColname          = "Contrast",
                                        plotCategory = "point",
                                        pointTransparency = 9),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    ## pointSize
    msg <- "pointSize must be a singular value of class numeric. Assigning default value '2'."
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointSize    = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointSize    = "0.1"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointSize    = -0.1),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotCategory = "point",
                                        pointSize    = c(0.1, 0.1)),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    ## labelAngle
    msg <- "labelAngle must be a singular value of class numeric between 0 and 90. Assigning default value '45'."
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        labelAngle   = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        labelAngle   = "0.1"),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        labelAngle   = -45),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        labelAngle   = 145),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        labelAngle   = c(45, 90)),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        labelAngle   = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        labelAngle   = "0.1"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        labelAngle   = -45),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        labelAngle   = 145),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        labelAngle   = c(45, 90)),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    ## facetCol
    msg <- "facetCol must be a singular value of class numeric. Assigning default value."
    ### cxplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        facet        = TRUE,
                                        facetCol     = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        facet        = TRUE,
                                        facetCol     = "4"),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        facet        = TRUE,
                                        facetCol     = -4),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    expect_warning(plot <- logRatioPlot(dgeObj = t_obj1_subset,
                                        plotType     = "canvasXpress",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        facet        = TRUE,
                                        facetCol     = c(4, 5)),
                   regexp = msg)
    expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
    ### ggplot
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        facet        = TRUE,
                                        facetCol     = NULL),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        facet        = TRUE,
                                        facetCol     = "4"),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        facet        = TRUE,
                                        facetCol     = -4),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    expect_warning(plot <- logRatioPlot(dgeObj = t_obj1_subset,
                                        plotType     = "ggplot",
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        facet        = TRUE,
                                        facetCol     = c(4, 5)),
                   regexp = msg)
    expect_s3_class(plot, c("gg", "ggplot"))
    ## plotType
    msg <- "plotType must be either canvasXpress or ggplot. Setting default value 'canvasXpress'"
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotType     = NULL),
                   regexp = msg)
    expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                        facetColname = "GeneSymbol",
                                        xColname     = "Contrast",
                                        plotType     = 123),
                   regexp = msg)
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         plotType     = c("canvasXpress", "ggplot")),
                    regexp = msg)
     ## axisFree
     msg <- "axisFree must be a singular logical value. Assigning default value TRUE."
     ### cxplot
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         axisFree     = NULL),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         axisFree     = 123),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         axisFree     = "FALSE"),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         axisFree     = c("TRUE", "FALSE")),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     ### ggplot
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         axisFree     = NULL),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         axisFree     = 123),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         axisFree     = "FALSE"),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         axisFree     = c("TRUE", "FALSE")),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))

     ## facet
     msg <- "facet must be singular logical value. Setting default value 'TRUE'."
     ### cxplot
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         facet     = NULL),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         facet     = 123),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         facet     = "FALSE"),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "canvasXpress",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         facet     = c("TRUE", "FALSE")),
                    regexp = msg)
     expect_s3_class(plot, c("canvasXpress", "htmlwidget"))
     ### ggplot
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         facet     = NULL),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         facet     = 123),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         facet     = "FALSE"),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))
     expect_warning(plot <- logRatioPlot(dgeObj  = t_obj1_subset,
                                         plotType     = "ggplot",
                                         facetColname = "GeneSymbol",
                                         xColname     = "Contrast",
                                         facet     = c("TRUE", "FALSE")),
                    regexp = msg)
     expect_s3_class(plot, c("gg", "ggplot"))
})
