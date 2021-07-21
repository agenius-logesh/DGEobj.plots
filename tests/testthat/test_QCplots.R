context("DGEobj.plots - tests for QCplots.R functions")

test_that("QCplots.R: QCplots()", {
    getLocation <- "http://ftp.ncbi.nlm.nih.gov/geo/series/GSE120nnn/GSE120804/suppl"
    qcFile      <- "GSE120804_qc_metrics.txt.gz"

    temp <- tempfile()
    path <- paste0(getLocation,"/", qcFile)
    if (download.file(path, destfile = temp, mode = 'wb')) {
        stop("Alignment QC Download Failed")
    }
    alignmentQC <- read.delim(temp, stringsAsFactors = FALSE)
    rownames(alignmentQC) <- alignmentQC$Metric
    alignmentQC <- alignmentQC %>%
        dplyr::select(-Metric) %>%
        t() %>%
        as.data.frame()

    t_obj1 <- addItem(t_obj1,
                      item     = alignmentQC,
                      itemName = "AlignmentQC",
                      itemType = "alignQC")
    metrics <- colnames(alignmentQC[1:5])

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "bar")
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "bar",
                       plotType     = "ggplot")
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("gg", "ggplot"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "point")
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "point",
                       plotType     = "ggplot")
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("gg", "ggplot"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "pointline")
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "pointline",
                       plotType     = "ggplot")
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("gg", "ggplot"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "histogram")
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "histogram",
                       plotType     = "ggplot")
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("gg", "ggplot"))

    #multiple plotCategories
    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = c("bar", "bar", "point", "pointline", "histogram"))
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = c("bar", "bar", "point", "pointline", "histogram"),
                       plotType     = "ggplot")
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("gg", "ggplot"))



    #Optional parameters
    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "bar",
                       labelAngle   = 30,
                       hlineSD      = 4,
                       winsorize    = FALSE)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "bar",
                       plotType     = "ggplot",
                       labelAngle   = 30,
                       hlineSD      = 4,
                       winsorize    = FALSE)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("gg", "ggplot"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "point",
                       labelAngle   = 30,
                       hlineSD      = 4,
                       winsorize    = FALSE)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "point",
                       plotType     = "ggplot",
                       labelAngle   = 30,
                       hlineSD      = 4,
                       winsorize    = FALSE)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("gg", "ggplot"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "pointline",
                       labelAngle   = 30,
                       hlineSD      = 4,
                       winsorize    = FALSE)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "pointline",
                       plotType     = "ggplot",
                       labelAngle   = 30,
                       hlineSD      = 4,
                       winsorize    = FALSE)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("gg", "ggplot"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "histogram",
                       labelAngle   = 30,
                       hlineSD      = 4,
                       winsorize    = FALSE)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    qc_plot <- QCplots(t_obj1,
                       metricNames  = metrics,
                       plotCategory = "histogram",
                       plotType     = "ggplot",
                       labelAngle   = 30,
                       hlineSD      = 4,
                       winsorize    = FALSE)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("gg", "ggplot"))


    #assert statements
    msg <- "DGEdata must be specified and must be of class 'DGEobj'."
    expect_error(QCplots(DGEdata = c(1,2)),
                 regexp = msg)
    expect_error(QCplots(),
                 regexp = msg)
    expect_error(QCplots(DGEdata = NULL),
                 regexp = msg)

    t_obj2 <- readRDS(system.file("exampleObj.RDS", package = "DGEobj", mustWork = TRUE))
    expect_error(QCplots(t_obj2),
                 regexp = "Dgedata must have a single alignQC item.")

    #metricNames
    msg <- "All of the specified metricNames must be present in the colnames of qcdata."
    expect_error(qc_plot <- QCplots(t_obj1,
                                      metricNames  = "invalid",
                                      plotCategory = "invalidcategory"),
                   regexp = msg)

    expect_error(qc_plot <- QCplots(t_obj1,
                                      metricNames  = c("invalidmetric", "not a validmetric"),
                                      plotCategory = "invalidcategory"),
                   regexp = msg)

    expect_error(qc_plot <- QCplots(t_obj1,
                                      metricNames  = NULL,
                                      plotCategory = "invalidcategory"),
                   regexp = msg)

    expect_error(qc_plot <- QCplots(t_obj1,
                                      plotCategory = "invalidcategory"),
                   regexp = msg)

    #plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'."
    expect_warning(qc_plot <- QCplots(t_obj1,
                                      metricNames = metrics,
                                      plotType    = NULL),
                   regexp = msg)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    expect_warning(qc_plot <- QCplots(t_obj1,
                                      metricNames = metrics,
                                      plotType    = "cx"),
                   regexp = msg)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    expect_warning(qc_plot <- QCplots(t_obj1,
                                      metricNames = metrics,
                                      plotType    = c("canvasXpress", "ggplot")),
                   regexp = msg)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    #plotCategory
    msg <- "plotCategory must be one of 'bar', 'point', 'pointline', or 'histogram'. Either one category or as many categories as the number of metric names can be specified. Assigning default value 'bar'"
    expect_warning(qc_plot <- QCplots(t_obj1,
                                      metricNames  = metrics,
                                      plotCategory = NULL),
                   regexp = msg)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    expect_warning(qc_plot <- QCplots(t_obj1,
                                      metricNames  = metrics,
                                      plotCategory = "invalidcategory"),
                   regexp = msg)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    expect_warning(qc_plot <- QCplots(t_obj1,
                                      metricNames  = metrics,
                                      plotCategory = c("bar", "point")),
                   regexp = msg)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    #labelAngle
    msg <- "labelAngle must be a numeric value. Assigning default values 30."
    expect_warning(qc_plot <- QCplots(t_obj1,
                                      metricNames = metrics,
                                      labelAngle  = NULL),
                   regexp = msg)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    expect_warning(qc_plot <- QCplots(t_obj1,
                                      metricNames = metrics,
                                      labelAngle  = "invalid"),
                   regexp = msg)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    expect_warning(qc_plot <- QCplots(t_obj1,
                                      metricNames = metrics,
                                      labelAngle  = c(30, 60)),
                   regexp = msg)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    #hlineSD
    msg <- "hlineSD needs to be a single numeric value. Assigning default value 3."
    expect_warning(qc_plot <- QCplots(t_obj1,
                                      metricNames = metrics,
                                      hlineSD     = "invalid"),
                   regexp = msg)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    expect_warning(qc_plot <- QCplots(t_obj1,
                                      metricNames = metrics,
                                      hlineSD     = c(30, 60)),
                   regexp = msg)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    #winsorize
    msg <- "winsorize must be a singular logical value. Assigning default value TRUE."
    expect_warning(qc_plot <- QCplots(t_obj1,
                                      metricNames = metrics,
                                      winsorize   = NULL),
                   regexp = msg)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    expect_warning(qc_plot <- QCplots(t_obj1,
                                      metricNames = metrics,
                                      winsorize   = c(TRUE, FALSE)),
                   regexp = msg)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))

    expect_warning(qc_plot <- QCplots(t_obj1,
                                      metricNames = metrics,
                                      winsorize   = "invalid"),
                   regexp = msg)
    expect_length(qc_plot, length(metrics))
    expect_s3_class(qc_plot[[1]], c("canvasXpress", "htmlwidget"))
})
