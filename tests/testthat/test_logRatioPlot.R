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
                                   labelAngle   =  60)
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
    ## refLine
    msg <- "refLine must be singular logical value. Setting default value 'TRUE'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                refLine      = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                refLine      = 123),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                refLine      = "FALSE"),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                refLine      = c("TRUE", "FALSE")),
                   regexp = msg)
    ## refLineColor
    msg <- "refLineColor must be a singular value of class character. Assigning default value 'red'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                refLineColor = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                refLineColor = 123),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                refLineColor = "FALSE"),
                   regexp = "Color specified is not valid. Assigning default value 'red'.")
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                refLineColor = c("red", "red")),
                   regexp = msg)
    ## title
    msg <- "title must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                title        = 123),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                title        = c(123, 345)),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                title        = c("title1", "title2")),
                   regexp = msg)
    ## xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                xlab         = 123),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                xlab         = c(123, 345)),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                xlab         = c("title1", "title2")),
                   regexp = msg)
    ## ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                ylab         = 123),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                ylab         = c(123, 345)),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                ylab         = c("title1", "title2")),
                   regexp = msg)
    ## barColor
    msg <- "barColor must be a singular value of class character. Assigning default value 'dodgerblue4'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barColor     = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barColor     = 123),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barColor     = "abc"),
                   regexp = "Color specified is not valid. Assigning default value 'dodgerblue4'.")
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barColor    = c("dodgerblue4", "dodgerblue4")),
                   regexp = msg)
    ## barSize
    msg <- "barSize must be a singular value of class numeric. Assigning default value '0.1'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barSize      = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barSize      = "0.1"),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barSize      = -0.1),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barSize      = c(0.1, 0.1)),
                   regexp = msg)
    ## barWidth
    msg <- "barWidth must be a singular value of class numeric. Assigning default value '0.9'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barWidth     = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barWidth     = "0.9"),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barWidth     = -0.9),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barWidth     = c(0.9, 0.9)),
                   regexp = msg)
    ## barTransparency
    msg <- "barTransparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '1'."
    expect_warning(logRatioPlot(contrastsDF     = tidyDat,
                                facetColname    = "GeneSymbol",
                                xColname        = "Contrast",
                                barTransparency = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barTransparency     = "0.9"),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barTransparency     = c(0.9, 0.9)),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barTransparency     = 0),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                barTransparency     = 9),
                   regexp = msg)
    ## pointColor
    msg <- "pointColor must be a singular value of class character. Assigning default value 'dodgerblue4'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointColor   = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointColor   = 123),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointColor   = "abc"),
                   regexp = "Color specified is not valid. Assigning default value 'dodgerblue4'.")
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointColor   = c("dodgerblue4", "dodgerblue4")),
                   regexp = msg)
    ## pointShape
    msg <- "pointShape must be a singular charcter values. Assigning default values 'circle'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointShape   = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointShape   = 123),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointShape   = "abc"),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointShape   = c("circle", "circle")),
                   regexp = msg)
    ## pointTransparency
    msg <- "pointTransparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '1'."
    expect_warning(logRatioPlot(contrastsDF     = tidyDat,
                                facetColname    = "GeneSymbol",
                                xColname        = "Contrast",
                                pointTransparency = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointTransparency     = "0.9"),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointTransparency     = c(0.9, 0.9)),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointTransparency     = 0),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointTransparency     = 9),
                   regexp = msg)
    ## pointSize
    msg <- "pointSize must be a singular value of class numeric. Assigning default value '2'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointSize    = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointSize    = "0.1"),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointSize    = -0.1),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                pointSize    = c(0.1, 0.1)),
                   regexp = msg)
    ## lineLayer
    msg <- "lineLayer must be a singular value of class logical. Assigning default value 'FALSE'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineLayer    = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineLayer    = "0.1"),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineLayer    = 0.1),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineLayer    = c(FALSE, TRUE)),
                   regexp = msg)
    ## lineColor
    msg <- "lineColor must be a singular value of class character. Assigning default value 'dodgerblue4'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineColor    = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineColor    = 123),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineColor    = "abc"),
                   regexp = "Color specified is not valid. Assigning default value 'dodgerblue4'.")
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineColor    = c("dodgerblue4", "dodgerblue4")),
                   regexp = msg)
    ## lineSize
    msg <- "lineSize must be a singular value of class numeric. Assigning default value '1'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineSize     = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineSize     = "0.1"),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineSize     = -0.1),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineSize     = c(0.1, 0.1)),
                   regexp = msg)
    ## lineFit
    msg <- "lineFit must be one of 'glm', 'lm', 'loess', 'gam' or NULL to disable. Assigning default value 'loess'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineFit      = 123),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineFit      = "abc"),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                lineFit      = c("loess", "loess")),
                   regexp = msg)
    ## facet
    msg <- "facet must be singular logical value. Setting default value 'TRUE'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                facet        = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                facet        = 123),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                facet        = "FALSE"),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                facet        = c("TRUE", "FALSE")),
                   regexp = msg)
    ## labelAngle
    msg <- "labelAngle must be a singular value of class numeric between 0 and 90. Assigning default value '45'."
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                labelAngle   = NULL),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                labelAngle   = "0.1"),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                labelAngle   = -45),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                labelAngle   = 145),
                   regexp = msg)
    expect_warning(logRatioPlot(contrastsDF  = tidyDat,
                                facetColname = "GeneSymbol",
                                xColname     = "Contrast",
                                labelAngle   = c(45, 90)),
                   regexp = msg)
})
