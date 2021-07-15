#' Plot histogram analysis of p-value distributions
#'
#' Generate a facet plot (or optionally individual plots) from a dataframe of
#' numbers. Intended to perform histogram analysis of p-value distributions,
#' but should be useful for any dataframe of numeric columns.
#'
#' @param DGEdata Name of DGEobj with a class of DGEobj.
#' @param P.Val A character vector of a topTable data in DGEobj. Default="P.Value".
#' @param plotType Plot type must be canvasXpress or ggplot (default = canvasXpress).
#' @param facet Set to FALSE to print individual plots instead of a faceted plot. (default = TRUE)
#' @param binWidth Value is always between 0 and 1. (default = 0.02)
#' @param transparency Set the transparency. (default = 0.6)
#' @param color Fill & Outline color for the histogram (default = "dodgerblue3")
#'
#' @return A canvasXpress or a ggplot2 object if facet = TRUE or a list of plots if facet = FALSE. (default = TRUE)
#'
#' @examples
#' \dontrun{
#'    # Print to console using all defaults
#'    plotPvalHist(DGEdata, P.Val)
#'
#'    # Use some custom arguments
#'    myplot <- plotPvalHist(DGEdata, P.Val)
#' }
#'
#' @import ggplot2
#' @importFrom dplyr filter select
#' @importFrom canvasXpress canvasXpress
#'
#' @export
plotPvalHist <- function(DGEdata,
                         P.Val          = "P.Value",
                         plotType       = "canvasXpress",
                         facet          = TRUE,
                         binWidth       = 0.02,
                         transparency   = 0.6,
                         color    = "dodgerblue3") {

    assertthat::assert_that(!missing(DGEdata),
                            !is.null(DGEdata),
                            "DGEobj" %in% class(DGEdata),
                            msg = "DGEdata must be specified and must belong to DGEobj class.")

    if (any(is.null(P.Val),
            !is.character(P.Val),
            length(P.Val) != 1)) {
        warning("P.Val must be a singular value of class character. Assigning default value 'P.Value'.")
        P.Val <- "P.Value"
    }

    P.Val <- extractCol(getType(DGEdata, "topTable"), colName = P.Val, robust = FALSE)

    plotType <- tolower(plotType)
    if (any(is.null(plotType),
            !is.character(plotType),
            length(plotType) != 1,
            !plotType %in% c("canvasxpress", "ggplot"))) {
        warning("plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'.")
        plotType <- "canvasxpress"
    }

    if (any(is.null(color),
            !is.character(color),
            length(color)  != 1,
            length(.validate_colors(color)) != 1)) {
        warning("color must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'dodgerblue3'.")
        color <- "dodgerblue3"
    }

    if (any(is.null(facet),
            !is.logical(facet),
            length(facet) != 1)) {
        warning("facet must be a singular logical value. Assigning default value TRUE.")
        facet = TRUE
    }

    if (any(is.null(binWidth),
            !is.numeric(binWidth),
            length(binWidth) != 1,
            binWidth <= 0,
            binWidth > 1)) {
        warning("binWidth must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.02'.")
        binWidth <- 0.02
    }

    if (any(is.null(transparency),
            !is.numeric(transparency),
            length(transparency) != 1,
            transparency <= 0,
            transparency > 1)) {
        warning("transparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.6'.")
        transparency <- 0.6
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
                                                    colors               = color,
                                                    transparency         = transparency,
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
                                                        colors               = color,
                                                        transparency         = transparency,
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
                ggplot2::geom_histogram(alpha    = transparency,
                                        fill     = color,
                                        color    = color,
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
                    ggplot2::geom_histogram(alpha = transparency,
                                            fill = color,
                                            color = color,
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
