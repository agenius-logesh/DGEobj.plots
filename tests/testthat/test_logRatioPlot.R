context("DGEobj.plots - tests for logRatioPlot.R functions")


test_that("logRatioPlot.R: logRatioPlot()", {
    suppressWarnings(skip_if(is.null(getType(t_obj1, "topTable"))))

    tidyDat <- tidyContrasts(t_obj1,
                             rownameColumn = "EnsgID",
                             includeColumns = c("logFC", "CI.R", "CI.L"))

    # Add gene symbols from geneData and select small set of genes for plotting
    ens2genesym <- data.frame("EnsgID" = row.names(t_obj1$geneData), t_obj1$geneData, row.names = NULL)
    ens2genesym <- ens2genesym[, c("EnsgID", "rgd_symbol")]
    colnames(ens2genesym) <- c("EnsgID", "GeneSymbol")

    tidyDat <- dplyr::left_join(tidyDat, ens2genesym) %>% head(10)

    # Simple barplot
    log_ratio_plot <- logRatioPlot(contrastsDF  = tidyDat,
                                   facetColname = "GeneSymbol",
                                   xColname     = "Contrast",
                                   facetCol     = 2)
    expect_s3_class(log_ratio_plot, c("gg", "ggplot"))

    # Lineplot with some options
    log_ratio_plot <- logRatioPlot(contrastsDF  = tidyDat,
                                   plotCategory = "point",
                                   facetColname = "GeneSymbol",
                                   xColname     = "Contrast",
                                   facetCol     = 4,
                                   scales       = "fixed",
                                   facet        = FALSE,
                                   title        = "Test",
                                   pointSize    = 4,
                                   lineLayer    = TRUE,
                                   lineSize     = 0.1,
                                   xAngle       = 60)
    expect_type(log_ratio_plot, "list")
    expect_s3_class(log_ratio_plot[[1]], c("gg", "ggplot"))

    # Testing asserts
    ## contrastsDF
    msg <- "contrastsDF must be specified and should be of class 'data.frame'."
    expect_error(logRatioPlot(),
                 regexp = msg)
    expect_error(logRatioPlot(NULL),
                 regexp = msg)
    expect_error(logRatioPlot(data.frame()),
                 regexp = msg)
    expect_error(logRatioPlot(tidyDat %>% as.matrix()),
                 regexp = msg)
    ## facetColname
    msg <- "facetColname must be one of contrastsDF columns."
    expect_error(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = c("GeneSymbol", "GeneSymbol")),
                   regexp = msg)
    expect_error(logRatioPlot(contrastsDF  = tidyDat,
                              facetColname = "abc"),
                 regexp = msg)
    expect_error(logRatioPlot(contrastsDF  = tidyDat,
                              facetColname = NULL),
                 regexp = msg)
    expect_error(logRatioPlot(contrastsDF  = tidyDat),
                 regexp = msg)
    ## xColname
    msg <- "xColname must be one of contrastsDF columns."
    expect_error(logRatioPlot(contrastsDF  = tidyDat,
                              facetColname = "GeneSymbol",
                              xColname = c("Contrast", "Contrast")),
                 regexp = msg)
    expect_error(logRatioPlot(contrastsDF  = tidyDat,
                              facetColname = "GeneSymbol",
                              xColname = "abc"),
                 regexp = msg)
    expect_error(logRatioPlot(contrastsDF  = tidyDat,
                              facetColname = "GeneSymbol",
                              xColname = NULL),
                 regexp = msg)
    expect_error(logRatioPlot(contrastsDF = tidyDat,
                              facetColname = "GeneSymbol"),
                 regexp = msg)
    ## yColname
    msg <- "yColname must be one of contrastsDF columns. Setting default value 'logFC'"
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                yColname     = c("Contrast", "Contrast")),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                yColname     = "abc"),
                 regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                              facetColname   = "GeneSymbol",
                              xColname       = "Contrast",
                              yColname       = NULL),
                 regexp = msg)
    tidyDat2 <- tidyDat %>% dplyr::rename(MyLog = logFC)
    expect_error(logRatioPlot(contrastsDF = tidyDat2,
                              facetColname = "GeneSymbol",
                              xColname     = "Contrast"),
                 regexp = "yColname must be one of contrastsDF columns.")
    ## CI.R_colname
    msg <- "CI.R_colname must be one of contrastsDF columns. Setting default value 'CI.R'"
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                yColname     = "logFC",
                                CI.R_colname = c("CI.R", "CI.R")),
                   regexp = msg,
                   fixed = TRUE)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                yColname     = "logFC",
                                CI.R_colname =  "abc"),
                   regexp = msg,
                   fixed = TRUE)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                yColname     = "logFC",
                                CI.R_colname = NULL),
                   regexp = msg,
                   fixed = TRUE)
    tidyDat2 <- tidyDat %>% dplyr::rename(MyCI.R = CI.R)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat2,
                              facetColname = "GeneSymbol",
                              xColname     = "Contrast",
                              yColname     = "logFC"),
                 regexp = "CI.R_colname must be one of contrastsDF columns. Disabling confidenece limits.",
                 fixed = TRUE)
    ## CI.L_colname
    msg <- "CI.L_colname must be one of contrastsDF columns. Setting default value 'CI.L'"
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                yColname     = "logFC",
                                CI.R_colname = "CI.R",
                                CI.L_colname = c("CI.L", "CI.L")),
                   regexp = msg,
                   fixed = TRUE)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                yColname     = "logFC",
                                CI.R_colname = "CI.R",
                                CI.L_colname =  "abc"),
                   regexp = msg,
                   fixed = TRUE)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                yColname     = "logFC",
                                CI.R_colname = "CI.R",
                                CI.L_colname = NULL),
                   regexp = msg,
                   fixed = TRUE)
    tidyDat2 <- tidyDat %>% dplyr::rename(MyCI.L = CI.L)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat2,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                yColname     = "logFC",
                                CI.R_colname = "CI.R"),
                   regexp = "CI.L_colname must be one of contrastsDF columns. Disabling confidenece limits.",
                   fixed = TRUE)
    ## plotCategory
    msg <- "plotCategory must be either 'bar' or 'point'. Setting default value 'bar'"
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                              facetColname = "GeneSymbol",
                              xColname     = "Contrast",
                              facetCol     = 2,
                              plotCategory = "heatmap"),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                facetCol     = 2,
                                plotCategory = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                facetCol     = 2,
                                plotCategory = 123),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                facetCol     = 2,
                                plotCategory = c("bar", "point")),
                   regexp = msg)
})
