#' Plot log2CPM before and after normalization
#'
#' Takes a DGEobj containing a counts matrix as input. Returns a canvasXpress or ggplot
#' object containing a faceted plot of log2CPM before and after normalization.
#'
#' Either a box plot or density plot category can be chosen.
#'
#' Normalization is performed by edgeR::calcNormFactors. Note TMM is specifically tailored for count-based
#' data.  Thus this function is only appropriate for count-based data.
#'
#' @param dgeObj DGEobj with a counts Matrix ( required)
#' @param plotType Plot type must be canvasXpress or ggplot (Default to canvasXpress).
#' @param plotCategory  One of "box" or "density." (Default = "box")
#' @param normalize Default = "TMM" and invokes TMM normalization. Other allowed
#'   values are: "RLE", "upperquartile", or "none". Invokes edgeR::calcNormFactors for
#'   normalization.
#'
#' @return A faceted canvasXpress/ggplot plot showing before/after log2CPM normalization.
#'
#' @examples
#' \dontrun{
#'    myNormPlotBox <- plotNorm(myDGEobj, plotType = "canvasXpress", plotCategory = "box")
#'    myNormPlotDensity <- plotNorm(counts, plotType = "ggplot", plotCategory = "density")
#' }
#'
#' @import magrittr ggplot2
#' @importFrom stringr str_c
#' @importFrom DGEobj getItem
#' @importFrom DGEobj.utils convertCounts
#' @importFrom assertthat assert_that
#' @importFrom canvasXpress canvasXpress
#' @importFrom htmlwidgets JS
#' @importFrom tidyr gather spread
#' @importFrom dplyr mutate bind_rows select
#'
#' @export
plotNorm <- function(dgeObj,
                     plotType = "canvasXpress",
                     plotCategory = "box",
                     normalize = "tmm") {

    assertthat::assert_that(!missing(dgeObj),
                            !is.null(dgeObj),
                            "DGEobj" %in% class(dgeObj),
                            msg = "dgeObj must be specified and must belong to DGEobj class.")
    countsmatrix_names <- names(DGEobj::getType(dgeObj, "counts"))
    assertthat::assert_that(length(countsmatrix_names) == 1,
                            msg = "Counts matrix should be present in dgeObj.dgeObj should not have more than one counts matrix.")

    plotType <- tolower(plotType)
    if (any(is.null(plotType),
            !is.character(plotType),
            length(plotType) != 1,
            !plotType %in% c("canvasxpress", "ggplot"))) {
        warning("plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'.")
        plotType <- "canvasxpress"
    }

    plotCategory <- tolower(plotCategory)
    if (any(is.null(plotCategory),
            !is.character(plotCategory),
            !length(plotCategory) == 1,
            !plotCategory %in% c("box", "density"))) {
        warning("plotCategory must be one of 'box' or 'density'.")
        plotCategory <- "box"
    }

    normalize    <- tolower(normalize)
    if (any(is.null(normalize),
            !is.character(normalize),
            !length(normalize) == 1,
            !normalize %in% c("tmm", "rle", "upperquartile", "none"))) {
        warning("normalize must be one of 'TMM', 'RLE', 'upperquartile', or 'none'.")
        normalize <- "tmm"
    }

    counts <- DGEobj::getItem(dgeObj, countsmatrix_names[[1]])
    tall <- build_normalized_data(counts)

    if (normalize != "none") {
        tall <- tall %>%
            dplyr::bind_rows(build_normalized_data(counts, toupper(normalize)))
    }


    title <- ifelse(normalize == "none", "Log2CPM Without Normalization",
                    stringr::str_c("Log2CPM before/after", normalize, "normalization", sep = " "))

    resultPlot <- NULL
    if (plotType == "canvasxpress") {
        if (plotCategory == "box") {
            resultPlot <- build_cx_box_plot(tall, title)
        } else {
            resultPlot <- build_cx_density_plot(tall, title)
        }
    } else {
        if (plotCategory == "density") {
            resultPlot <- build_gg_density_plot(tall, title)
        } else {
            resultPlot <- build_gg_box_plot(tall, title)
        }
    }
    resultPlot
}

build_cx_density_plot <- function(data, title) {
    plot.data <- data %>%
        tidyr::spread(SampleID, Log2CPM)
    cx.data <- plot.data %>%
        dplyr::select(!c(GeneID, Normalization))
    var.annot <- plot.data %>%
        dplyr::select(c(GeneID, Normalization))

    xlab <- "Log2CPM"
    ylab <- "density"
    events <- htmlwidgets::JS(
        "{'mousemove' : function(o, e, t) {
                            if (o != null && o != false) {
                                if (o.objectType == 'Density' &&
                                    o.display != null &&
                                    o.display.indexOf('-') > -1) {
                                    sampleID = o.display.split('-')[2];
                                    t.showInfoSpan(e, '<b>Sample ID</b>: ' + sampleID);
                                } else {
                                    t.showInfoSpan(e, o.display);
                                };
                            };
                        }}")

    canvasXpress::canvasXpress(
        data                    = cx.data,
        varAnnot                = var.annot,
        histogramData           = TRUE,
        graphType               = "Scatter2D",
        xAxisTitle              = xlab,
        yAxisTitle              = ylab,
        hideHistogram           = TRUE,
        showHistogramDensity    = TRUE,
        segregateVariablesBy    = list("Normalization"),
        title                   = title,
        showLegend              = FALSE,
        events                  = events
    )
}

build_cx_box_plot <- function(data, title) {
    plot.data <- data %>%
        tidyr::spread(SampleID, Log2CPM)
    cx.data <- plot.data %>%
        dplyr::select(!c(GeneID, Normalization)) %>%
        t() %>%
        as.data.frame()
    smp.data <- plot.data %>%
        dplyr::select(c(GeneID, Normalization))
    rownames(smp.data) <- colnames(cx.data)

    xlab      <- "Log2CPM"
    smp_title <- "SampleID"

    events <- htmlwidgets::JS(
        "{'mousemove' : function(o, e, t) {
                            if (o != null && o != false &&
                                o.w != null && o.w.vars != null) {
                                function round(num, decimals) {
                                     tens = Math.pow(10, decimals);
                                     return Math.round((num + Number.EPSILON) * tens)/tens;
                                }
                                t.showInfoSpan(e, '<h3>' + o.w.vars[0] + '<h3>' +
                                                  '<b>1st IQR</b>: ' + round(o.w.iqr1[0], 2) + '<br>' +
                                                  '<b>1st Qtl</b>: ' + round(o.w.qtl1[0], 2) + '<br>' +
                                                  '<b>Median</b>: ' +  round(o.w.median[0], 2)+ '<br>'+
                                                  '<b>3rd IQR</b>: ' + round(o.w.iqr3[0], 2) + '<br>' +
                                                  '<b>3rd Qtl</b>: ' + round(o.w.qtl3[0], 2) + '<br>' +
                                                  '<b>Outliers</b>: ' + o.w.out[0].length + '<br>' +
                                                  '<b>N-Data</b>: ' + o.w.n[0] + '<br>');
                            } else {
                                t.showInfoSpan(e, o.display);
                            };
                        }}")

    canvasXpress::canvasXpress(
        data                    = cx.data,
        smpAnnot                = smp.data,
        graphType               = "Boxplot",
        title                   = title,
        showLegend              = FALSE,
        xAxisTitle              = xlab,
        smpTitle                = smp_title,
        segregateSamplesBy      = list("Normalization"),
        groupingFactors         = list("Normalization"),
        graphOrientation        = "vertical",
        boxplotOutliersRatio    = "2",
        events                  = events
    )
}


build_gg_density_plot <- function(data, title) {
    data %>%
        ggplot(aes(x = Log2CPM, color = SampleID)) +
        geom_density() +
        facet_grid(~Normalization) +
        ggtitle(title)  +
        theme_gray() +
        theme(legend.position = "none")
}

build_gg_box_plot <- function(data, title) {
    data %>%
        ggplot(aes(x = SampleID, y = Log2CPM, color = SampleID)) +
        geom_boxplot(alpha = 0.5) +
        facet_grid( ~ Normalization) +
        ggtitle(title)  +
        theme_gray() +
        theme(axis.text.x = element_blank(),
              legend.position = "none")
}

build_normalized_data <- function(counts, normalize = "None") {
    DGEobj.utils::convertCounts(counts, unit = "cpm", log = TRUE, normalize = normalize) %>%
        as.data.frame() %>%
        dplyr::mutate(GeneID = rownames(.)) %>%
        tidyr::gather(key = "SampleID", val = "Log2CPM", -GeneID) %>%
        dplyr::mutate(Normalization = normalize)
}

