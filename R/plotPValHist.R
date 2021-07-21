#' Plot histogram analysis of p-value distributions
#'
#' Generate a facet plot (or optionally individual plots) from a dataframe of
#' numbers. Intended to perform histogram analysis of p-value distributions,
#' but should be useful for any dataframe of numeric columns.
#'
#' @param dgeObj DGEobj with a class of DGEobj.
#' @param P.Val p-value column name in the topTables in DGEobj. Default="P.Value".
#' @param plotType Plot type must be canvasXpress or ggplot (default = canvasXpress).
#' @param facet Set to FALSE to print individual plots instead of a faceted plot. (default = TRUE)
#' @param binWidth Value is always between 0 and 1. (default = 0.02)
#'
#' @return A canvasXpress or a ggplot2 object if facet = TRUE or a list of plots if facet = FALSE. (default = TRUE)
#'
#' @examples
#' \dontrun{
#'    # Plot to console (dgeObj is a DGEobj and P.Val is a name of topTable data in DGEobj.)
#'    myplot <- plotPvalHist(dgeObj, P.Val = "P.Value")
#'
#'    myplot <- plotPvalHist(dgeObj, P.Val = "P.Value", plotType = "ggplot")
#' }
#'
#' @import ggplot2
#' @importFrom dplyr filter select
#' @importFrom canvasXpress canvasXpress
#'
#' @export
plotPvalHist <- function(dgeObj,
                         P.Val          = "P.Value",
                         plotType       = "canvasXpress",
                         facet          = TRUE,
                         binWidth       = 0.02) {

    assertthat::assert_that(!missing(dgeObj),
                            !is.null(dgeObj),
                            "DGEobj" %in% class(dgeObj),
                            msg = "dgeObj must be specified and must belong to DGEobj class.")

    if (any(is.null(P.Val),
            !is.character(P.Val),
            length(P.Val) != 1)) {
        warning("P.Val must be a singular value of class character. Assigning default value 'P.Value'.")
        P.Val <- "P.Value"
    }

    P.Val <- DGEobj.utils::extractCol(DGEobj::getType(dgeObj, "topTable"), colName = P.Val, robust = FALSE)

    plotType <- tolower(plotType)
    if (any(is.null(plotType),
            !is.character(plotType),
            length(plotType) != 1,
            !plotType %in% c("canvasxpress", "ggplot"))) {
        warning("plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'.")
        plotType <- "canvasxpress"
    }

    if (any(is.null(facet),
            !is.logical(facet),
            length(facet) != 1)) {
        warning("facet must be a singular logical value. Assigning default value TRUE.")
        facet <- TRUE
    }

    if (any(is.null(binWidth),
            !is.numeric(binWidth),
            length(binWidth) != 1,
            binWidth <= 0,
            binWidth > 1)) {
        warning("binWidth must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.02'.")
        binWidth <- 0.02
    }


    if (is.matrix(P.Val)) {
        P.Val <- P.Val %>%
            as.data.frame
    }

    samples_num  <- ncol(P.Val)
    sample_names <- colnames(P.Val)

    # Set up Tall format
    P.Val$GeneID = rownames(P.Val)
    P.Val <- P.Val %>% tidyr::gather(key = "levels", value = "pval", -GeneID)

    title <- "P-value Histograms"
    plotlist <- list()
    if (plotType == "canvasxpress") {
        events <- htmlwidgets::JS("{'mousemove' : function(o, e, t) {
                      if (o != null && o != false) {
                          count = o.y.data[0][0];
                          bin = o.y.data[0][1];
                          t.showInfoSpan(e, '<b>Count</b>: ' +
                          count + '<br/><b>Bin</b>: '+bin);
                       }; }}")

        if (facet) {
            cx.data <- P.Val %>% dplyr::select(pval)
            var.annot <- P.Val %>% dplyr::select(-pval)
            plotlist  <- canvasXpress::canvasXpress(data                 = cx.data,
                                                    varAnnot             = var.annot,
                                                    histogramData        = TRUE,
                                                    histogramBinWidth    = binWidth,
                                                    graphType            = "Scatter2D",
                                                    colors               = "dodgerblue3",
                                                    title                = title,
                                                    xAxisTitle           = "P-value",
                                                    yAxisTitle           = "Count",
                                                    hideHistogram        = FALSE,
                                                    showHistogramDensity = FALSE,
                                                    showLegend           = FALSE,
                                                    segregateVariablesBy = list("levels"),
                                                    events               = events)
        } else {
            plotlist <- lapply(sample_names, function(sample) {
                pval_subset <- dplyr::filter(P.Val, grepl(sample, levels))
                cx.data <- pval_subset %>% dplyr::select(pval)
                var.annot <- pval_subset %>% dplyr::select(-pval)
                hist_pval <- canvasXpress::canvasXpress(data                 = cx.data,
                                                        varAnnot             = var.annot,
                                                        histogramData        = TRUE,
                                                        histogramBinWidth    = binWidth,
                                                        graphType            = "Scatter2D",
                                                        colors               = "dodgerblue3",
                                                        title                = paste(title, "\n", sample),
                                                        xAxisTitle           = "P-value",
                                                        yAxisTitle           = "Count",
                                                        hideHistogram        = FALSE,
                                                        showHistogramDensity = FALSE,
                                                        showLegend           = FALSE,
                                                        events               = events)
                hist_pval
            })
        }
    } else {
        if (facet) {
            numcol <- 3
            numrow <- (samples_num / numcol) %>% ceiling

            plotlist <- ggplot2::ggplot(data = P.Val, aes(x = pval)) +
                ggplot2::geom_histogram(fill     = "dodgerblue3",
                                        color    = "dodgerblue3",
                                        binwidth = binWidth) +
                ggplot2::xlab("P-value") +
                ggplot2::ylab("Count") +
                ggtitle(title) +
                ggplot2::scale_fill_brewer(palette = "Set1") +
                ggplot2::facet_wrap(~levels, nrow = numrow, scales = "free")
        } else {
            plotlist <- lapply(sample_names, function(sample) {
                pval_subset <- dplyr::filter(P.Val, grepl(sample, levels))

                hist_pval <- ggplot2::ggplot(data = pval_subset, aes(x = pval)) +
                    ggplot2::geom_histogram(fill = "dodgerblue3",
                                            color = "dodgerblue3",
                                            binwidth = binWidth) +
                    ggplot2::xlab("P-value") +
                    ggplot2::ylab("Count") +
                    ggplot2::ggtitle(paste(title, "\n", sample))

                hist_pval
            })
        }
    }
    return(plotlist)
}
