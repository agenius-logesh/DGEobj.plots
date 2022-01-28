#' Create QC metric plots
#'
#' Takes a DGEobject which has an item of type "alignQC" (qcdata).
#'
#' qcdata is a dataframe with metric names as column names and samples as row name.
#'
#' Returns a list of QC plots for the metricNames provided as input.
#'
#' By default, horizontal reference lines are drawn at the median and +/- n SDs based on the
#' hlineSD argument. These are statistical reference points, NOT pass/fail limits.
#'
#' @param DGEdata A DGEobj object with an item of type "alignQC" (required)
#' @param metricNames A list of metrics to plot. Values must exist in column names of the item
#' of type AlignQC. (required)
#' @param plotType Plot type must be canvasXpress or ggplot (default = canvasXpress).
#' @param plotCategory One of "bar", "point", "pointline" or "histogram". For a different
#'   plot type for each metric, pass a list of plotCategories with length equal to
#'   length(metricNames). (default = "bar")
#' @param labelAngle Angle (0 - 90) to set the sample labels on the X axis (default = 30)
#' @param hlineSD Draw two reference lines: 1) at the median value 2) the number of
#'   SDs defined by the value of hlineSD. (default = 3). Set to 0 to disable the reference lines.
#' @param winsorize This implements a robust method to calculate standard
#'   deviations.  It is used to calculate the standard deviation for the
#'   placement of horizontal reference lines (hlineSD argument).  The adaptive
#'   winsorization used here only trims extreme values when normality is
#'   violated. See https://www.r-bloggers.com/winsorization/ for details.
#'   (default = TRUE).
#'
#' @return canvasXpress or ggplot object if one plot is specified.
#' A list of canvasXpress or ggplot objects if 2 or more metrics specified.
#'
#' @examples
#' \dontrun{
#'   someFaveMetrics <- c("Alignment_MappedRate",
#'                        "Alignment_PairedRate",
#'                        "Source_rRNA",
#'                        "Strand_Read1AntiSense",
#'                        "Strand_ReadPairAntiSense",
#'                        "Profile_ExonRate",
#'                        "Profile_InterGene_FPK")
#'   # All defaults
#'   MyQCplots <- QCplots(qcdata, metricNames = someFaveMetrics)
#'   # Draw the first plot
#'   print(MyQCplots[[1]])
#'
#'   # ggplot example
#'   MyQCplots <- QCplots(t_obj1, metricNames = someFaveMetrics, plotType = "ggplot")
#'   print(MyQCplots[[1]])
#' }
#'
#' @import ggplot2 magrittr
#' @importFrom assertthat assert_that
#' @importFrom stringr str_c
#' @importFrom stats median sd mad
#' @importFrom dplyr select arrange mutate_all
#' @importFrom rlang sym
#'
#' @export
QCplots <- function(DGEdata,
                    metricNames,
                    plotType     = "canvasXpress",
                    plotCategory = "bar",
                    labelAngle   = 30,
                    hlineSD      = 3,
                    winsorize    = TRUE) {

    assertthat::assert_that(!missing(DGEdata),
                            !is.null(DGEdata),
                            class(DGEdata)[1] %in% c("DGEobj"),
                            msg = "DGEdata must be specified and must be of class 'DGEobj'.")

    qcdata <- suppressWarnings(names(DGEobj::getType(DGEdata,"alignQC")))
    assertthat::assert_that(!is.null(qcdata),
                            length(qcdata) == 1,
                            msg = "Dgedata must have a single alignQC item.")

    qcdata <- DGEobj::getType(DGEdata,"alignQC")[[1]]
    qcdata[["Sample"]] <- rownames(qcdata)

    #metricNames
    assertthat::assert_that(!missing(metricNames),
                            !is.null(metricNames),
                            all(metricNames %in% colnames(qcdata)),
                            msg = "All of the specified metricNames must be present in the colnames of qcdata.")

    #plotType
    plotType <- tolower(plotType)
    if (any(is.null(plotType),
            !is.character(plotType),
            length(plotType) != 1,
            !plotType %in% c("canvasxpress", "ggplot"))) {
        warning("plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'.")
        plotType <- "canvasxpress"
    }

    #plotCategory
    plotCategory <- tolower(plotCategory)
    if (any(is.null(plotCategory),
            !(length(plotCategory) == 1 || length(plotCategory) == length(metricNames)),
            !(all(plotCategory %in% c("bar", "point", "pointline", "histogram"))))) {
        warning("plotCategory must be one of 'bar', 'point', 'pointline', or 'histogram'. Either one category or as many categories as the number of metric names can be specified. Assigning default value 'bar'.")
        plotCategory <- "bar"
    }

    #hlineSD
    if (is.null(hlineSD)) {
        hlineSD <- 0
    } else if (!is.null(hlineSD) &&
              any(!is.numeric(hlineSD),
                  length(hlineSD) != 1)) {
        warning("hlineSD needs to be a single numeric value. Assigning default value 3.")
        hlineSD <- 3
    }

    #labelAngle
    if (any(is.null(labelAngle),
            !is.numeric(labelAngle),
            length(labelAngle)  != 1)) {
        warning("labelAngle must be a numeric value. Assigning default values 30.")
        labelAngle <- 30
    }

    #winsorize
    if (any(is.null(winsorize),
            length(winsorize) != 1,
            !is.logical(winsorize))) {
        warning("winsorize must be a singular logical value. Assigning default value TRUE.")
        winsorize <- TRUE
    }

    # Replace "." in column 2-n with NA and convert columns 2-n to numeric
    dotIdx <- qcdata == "."
    if (sum(dotIdx, na.rm = TRUE) > 0) {
        qcdata[dotIdx] <- NA
        qcdata <- dplyr::mutate_all(qcdata, function(x) as.numeric(as.character(x)))
        for (columnName in colnames(qcdata)[2:ncol(qcdata)])
            qcdata[columnName] <- as.numeric(qcdata[[columnName]])
    }

    # If only one plotCategory, apply to all plots
    if (length(plotCategory) == 1) {
        plotCategory <- rep(plotCategory, length(metricNames))
    }

    plots <- list()
    for (metric in metricNames) {
        idx <- metric == metricNames
        plot_metric <- plotCategory[idx]

        # Calculate mean and sd for hline yintercepts
        if (winsorize == TRUE) {
            thisMetric <- .winsorize(qcdata[,metric], removeNA = TRUE)
        } else {
            thisMetric <- qcdata[, metric, drop = TRUE]
        }

        metricMedian <- median(as.numeric(thisMetric[-1]), na.rm = TRUE)
        metricMean   <- mean(as.numeric(thisMetric[-1]), na.rm = TRUE)
        metricSD     <- sd(as.numeric(thisMetric[-1]), na.rm = TRUE)
        SD           <- metricSD * hlineSD

        if (plotType == "canvasxpress") {
            cx.data <- qcdata %>%
                dplyr::select(Sample, !!rlang::sym(metric)) %>%
                dplyr::arrange(Sample)
            rownames(cx.data) <- cx.data[["Sample"]]
            cx.data <- cx.data %>%
                dplyr::select(-Sample) %>%
                t() %>%
                as.data.frame()

            decorations = list()

            if (hlineSD > 0 && plot_metric == "histogram") {
                decorations <- .getCxPlotDecorations(decorations = decorations,
                                                     color = .rgbaConversion("grey70", alpha = 1),
                                                     width = 2,
                                                     x = metricMedian)

                decorations <- .getCxPlotDecorations(decorations = decorations,
                                                     color = .rgbaConversion("firebrick3", alpha = 1),
                                                     width = 1,
                                                     x = metricMean + SD)

                decorations <- .getCxPlotDecorations(decorations = decorations,
                                                     color = .rgbaConversion("firebrick3", alpha = 1),
                                                     width = 1,
                                                     x = metricMean - SD)
            } else if (hlineSD > 0) {
                decorations <- list()

                line <- list(color = .rgbaConversion("grey70", alpha = 1),
                             width = 2,
                             value = metricMedian)
                decorations <- list(line = append(decorations$line, list(line)))

                line <- list(color = .rgbaConversion("firebrick3", alpha = 1),
                             width = 1,
                             type  = "longdash",
                             value = metricMean + SD)
                decorations <- list(line = append(decorations$line, list(line)))

                if (metricMean - SD > 0) {
                    line <- list(color = .rgbaConversion("firebrick3", alpha = 1),
                                 width = 1,
                                 type  = "longdash",
                                 value = metricMean - SD)
                    decorations <- list(line = append(decorations$line, list(line)))
                }
            }

            min.val <- min(min(cx.data[1,]), metricMedian, metricMean + SD, metricMean - SD)
            min.val <- min.val - 0.05 * min.val
            max.val <- max(max(cx.data[1,]), metricMedian, metricMean + SD, metricMean - SD)
            max.val <- max.val + 0.05 * max.val

            cx.params <- list(data             = cx.data,
                              graphOrientation = "vertical",
                              smpTitle         = "Sample",
                              title            = metric,
                              decorations      = decorations,
                              xAxisTitle       = metric,
                              smpLabelRotate   = labelAngle,
                              colorScheme      = "Dark2",
                              showLegend       = FALSE)

            if (plot_metric == "bar") {
                cx.params <- c(cx.params, list(graphType = "Bar",
                                               setMinX = 0))

            } else if (plot_metric == "point") {
                cx.params <- c(cx.params, list( graphType = "Dotplot",
                                                setMinX   = min.val,
                                                setMaxX   = max.val))

            } else if (plot_metric == "pointline") {
                smp.data <- data.frame(rep(metric,ncol(cx.data)))
                rownames(smp.data) <- colnames(cx.data)
                colnames(smp.data) <- "metric"
                cx.params <- c(cx.params, list( graphType = "Dotplot",
                                                smpAnnot  = smp.data,
                                                connectBy = "metric",
                                                setMinX   = min.val,
                                                setMaxX   = max.val))
            } else if (plot_metric == "histogram") {
                cx.data <- qcdata %>%
                    dplyr::select(!!rlang::sym(metric))
                cx.params <- list(data                     = cx.data,
                                  graphType                = "Scatter2D",
                                  colorScheme              = "Dark2",
                                  # decoration will be disabled until related cx plot is resolved
                                  #decorations              = decorations,
                                  showLegend               = FALSE,
                                  xAxisTitle               = metric,
                                  yAxisTitle               = "count",
                                  title                    = metric,
                                  histogramMedianLineStyle = "solid",
                                  showHistogramMedian      = TRUE,
                                  afterRender              = list(list("createHistogram")))
            }
            p <- do.call(canvasXpress::canvasXpress, cx.params)

        } else {
            color <- "dodgerblue3"

            if (plot_metric == "histogram") {
                p <- ggplot2::ggplot(qcdata, aes_string(x = metric))
            } else{
                p <- ggplot2::ggplot(qcdata, aes_string(x = "Sample", y = metric, group = 1))
            }
            p <- switch(plot_metric,
                        bar = {p +
                                ggplot2::geom_bar(stat = "identity", color = color, fill = color)},
                        point = {p + ggplot2::geom_point(color = color)},
                        pointline = {p +
                                ggplot2::geom_point(color = color, fill = color) +
                                ggplot2::geom_line(color = color)},
                        histogram = { p +
                                ggplot2::geom_histogram(color = color, fill = color, bins = 30)}
            )

            # Draw hline xSD above or below the mean
            SD <- metricSD * hlineSD
            if (hlineSD > 0 && plot_metric == "histogram") {

                # Plot vlines for the histogram
                p <- p +
                    ggplot2::geom_vline(xintercept = metricMedian, color = "grey70") +
                    ggplot2::geom_vline(xintercept = metricMean + SD, color = "firebrick3", linetype = "longdash") +
                    ggplot2::geom_vline(xintercept = metricMean - SD, color = "firebrick3", linetype = "longdash")
            } else if (hlineSD > 0) {  # Use hlines for other plot types
                p <- p +
                    ggplot2::geom_hline(yintercept = metricMedian, color = "grey70") +
                    ggplot2::geom_hline(yintercept = metricMean + SD, color = "firebrick3", linetype = "longdash")
                # Most qc plots floor to 0 so no point plotting -SD if it goes below zero.
                if (metricMean - SD > 0) {
                    p <- p + ggplot2::geom_hline(yintercept = metricMean - SD, color = "firebrick3", linetype = "longdash")
                }
            }

            # Set x axis text angle
            hjust = 1
            vjust = 1
            if (labelAngle == 0) {
                hjust = 0.5
            }
            if (labelAngle == 90) {
                vjust = 0.5
            }

            p <- p +
                ggplot2::ggtitle(metric) +
                ggplot2::theme(axis.text.x = element_text(angle = labelAngle, hjust = hjust, vjust = vjust))
        }

        plots[[metric]] <- p
    }

    plots
}

# Helper function
.winsorize <- function(x, removeNA){

    med <- stats::median(x, na.rm = removeNA)
    y <- x - med # Median centered
    sc <- stats::mad(y, center = 0, na.rm = removeNA) * 3
    y[ y > sc ] <- sc
    y[ y < -sc ] <- -sc
    y + med
}
