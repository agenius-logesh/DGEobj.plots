#' Create volcano plot
#'
#' A volcano plot shows Log Ratio data on the X axis and Negative Log P-values (NLP) on the
#' Y axis. This function is intended to show the volcano plot created from a
#' topTable dataframe in a DGEobj. Properly normalized data will generally be
#' centered around LogRatio = 0.
#'
#' By default, the plot places "logFC" on the X axis and Log10 of the "P.Value" on the Y axis.
#' By default, a reference vertical line is drawn at LogRatio = 0 on the X axis.
#' Optionally, additional reference lines will be drawn at +/- a user supplied Log Ratio threshold.
#' The points are color coded using both the significance and fold-change thresholds supplied by the user.
#' By default, the P.Value field is used with a threshold of 0.01 to color code the points and fold-change
#' threshold of +/- 1.5X.
#'
#' \strong{Data Structure for the input DGEobj:}
#'
#' A contrast needs to be specified along with the DGEobj. The top table is extracted from the DGEobj for this contrast.
#' The columns named "logFC" and "P.Value" in the topTable are used by default to generate the volcano plot.
#' By default, the column names will be used for the axis labels, but can be overridden with xlab and ylab arguments.
#'
#' A significance measure (which defaults to P.Value <= 0.01) and LogRatio
#' threshold are used to color code genes that are significantly increased or decreased.
#' Use the appropriate arguments to use an FDR measure instead of p-value.
#'
#' @param dgeObj DGEobj.
#' @param contrast Name of the contrast.
#' @param plotType Plot type must be canvasXpress or ggplot. (default = canvasXpress)
#' @param logRatioCol Name of the LogRatio column. (default = "logFC")
#' @param logIntCol Name of the LogIntensity column. (default = "AveExpr")
#' @param pvalCol Name of the p-value or FDR column. (default = "P.Value")
#' @param xlab X axis label. (Default is the LogIntensity column name)
#' @param ylab Y axis label. (Default is the LogRatio column name)
#' @param title Plot title (optional).
#' @param pthreshold Used to color points. (default = 0.01)
#' @param geneNameCol geneName column in geneData from DGEobj. This column will be used to label significantly changed points.
#' @param pthresholdLine Color for a horizontal line at the p-threshold. (default = NULL (disabled))
#' @param sizeByIntensity If TRUE, creates a column to support sizeByIntensity. (default = TRUE)
#' @param foldChangeThreshold Position of reference vertical lines for fold change. (default = 1.5)
#' @return canvasxpress or ggplot object based on plotType selection.
#'
#' @examples
#' \dontrun{
#'    # Simple plot with custom title (dgeObj is a name of DGEobj and contrast is a name of topTable dataframe)
#'    contrast <- names(DGEobj::getItems(dgeObj, "topTable"))[1]
#'    myPlot <- volcanoPlot(dgeObj,
#'                          contrast,
#'                          title = "Plot Title")
#'
#'    # Some options with a custom datafile
#'    myPlot <- volcanoPlot(dgeObj,
#'                          contrast,
#'                          pthreshold     = 0.1,
#'                          logRatioCol    = "logFC",
#'                          logIntCol      = "AveExpr",
#'                          pvalCol        = "P.Value",
#'                          xlab           = "logFC",
#'                          ylab           = "negLog10p",
#'                          title          = "Volcano Plot Title",
#'                          pthresholdLine = "blue")
#'
#'    myPlot <- volcanoPlot(dgeObj,
#'                          contrast,
#'                          pthreshold     = 0.1,
#'                          logRatioCol    = "logFC",
#'                          logIntCol      = "AveExpr",
#'                          pvalCol        = "P.Value",
#'                          xlab           = "logFC",
#'                          ylab           = "negLog10p",
#'                          title          = "Volcano Plot Title",
#'                          pthresholdLine = "blue",
#'                          plotType       = "ggplot")
#' }
#'
#' @import ggplot2 magrittr
#' @importFrom dplyr rename case_when mutate arrange
#' @importFrom rlang sym
#' @importFrom tibble column_to_rownames
#' @importFrom ggrepel geom_text_repel
#' @importFrom canvasXpress canvasXpress
#' @importFrom htmlwidgets JS
#'
#' @export
volcanoPlot <- function(dgeObj,
                        contrast,
                        plotType        = "canvasXpress",
                        logRatioCol     = "logFC",
                        logIntCol       = "AveExpr",
                        pvalCol         = "P.Value",
                        pthreshold      = 0.01,
                        geneNameCol,
                        xlab            = NULL,
                        ylab            = NULL,
                        title           = NULL,
                        sizeByIntensity = TRUE,
                        pthresholdLine  = NULL,
                        foldChangeThreshold = 1.5) {
    ##### Asserts
    assertthat::assert_that(!missing(dgeObj),
                            !is.null(dgeObj),
                            "DGEobj" %in% class(dgeObj),
                            msg = "dgeObj must be specified and must belong to DGEobj.")

    assertthat::assert_that(!missing(contrast),
                            !is.null(contrast),
                            length(contrast) == 1,
                            contrast %in% names(DGEobj::getType(dgeObj, type = "topTable")),
                            msg = "contrast to be a singular value of class character and must be one of the topTables in dgeObj.")

    contrastDF <- DGEobj::getItems(dgeObj, contrast)

    plotType <- tolower(plotType)
    if (any(is.null(plotType),
            !is.character(plotType),
            length(plotType) != 1,
            !plotType %in% c("canvasxpress", "ggplot"))) {
        warning("plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'.")
        plotType <- "canvasxpress"
    }

    # Make sure specified columns exist
    assertthat::assert_that(!is.null(logRatioCol),
                            length(logRatioCol) == 1,
                            logRatioCol %in% colnames(contrastDF),
                            msg = "logRatioCol to be a singular value of class character and must be in contrast data.")

    assertthat::assert_that(!is.null(logIntCol),
                            length(logIntCol) == 1,
                            logIntCol %in% colnames(contrastDF),
                            msg = "logIntCol to be a singular value of class character and must be in contrast data.")

    assertthat::assert_that(!is.null(pvalCol),
                            length(pvalCol) == 1,
                            pvalCol %in% colnames(contrastDF),
                            msg = "pvalCol to be a singular value of class character and must be in contrast data.")

    if (!missing(geneNameCol)) {
        assertthat::assert_that(!is.null(geneNameCol),
                                length(geneNameCol) == 1,
                                geneNameCol %in% names(DGEobj::getType(dgeObj, type = "geneData")[[1]]),
                                msg = "geneNameCol to be a singular value of class character and must be in contrast data.")
    }

    if (any(is.null(pthreshold),
            !is.numeric(pthreshold),
            length(pthreshold) != 1)) {
        warning("pthreshold must be a singular numeric value. Assigning default value 0.01")
        pthreshold <- 0.01
    }

    if (any(is.null(foldChangeThreshold),
            !is.numeric(foldChangeThreshold),
            length(foldChangeThreshold) != 1)) {
        warning("foldChangeThreshold must be a singular numeric value. Assigning default value 1.5.")
        foldChangeThreshold <- 1.5
    }

    if (!is.null(title) &&
        !all(is.character(title),
             length(title) == 1)) {
        warning("title must be a singular value of class character. Assigning default value 'NULL'.")
        title <- NULL
    }

    if (!is.null(xlab) &&
        !all(is.character(xlab),
             length(xlab) == 1)) {
        warning("xlab must be a singular value of class character. Assigning default value 'NULL'.")
        xlab <- NULL
    }

    if (!is.null(ylab) &&
        !all(is.character(ylab),
             length(ylab) == 1)) {
        warning("ylab must be a singular value of class character. Assigning default value 'NULL'.")
        ylab <- NULL
    }

    if (!is.null(pthresholdLine) &&
        !all(is.character(pthresholdLine),
             length(pthresholdLine) == 1)) {
        warning("pthresholdLine must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'.")
        pthresholdLine <- NULL
    } else if (.rgbaConversion(pthresholdLine) == "invalid value") {
        warning("Color specified is not valid. Assigning default value 'NULL'.")
        pthresholdLine <- NULL
    }

    if (any(is.null(sizeByIntensity),
            !is.logical(sizeByIntensity),
            length(sizeByIntensity) != 1)) {
        warning("sizeByIntensity must be a singular logical value. Assigning default value TRUE")
        sizeByIntensity = TRUE
    }

    if (sizeByIntensity) {
        contrastDF <- contrastDF %>%
            dplyr::mutate(LogInt = dplyr::case_when(
                !!rlang::sym(logIntCol) < 0 ~ 0,
                !!rlang::sym(logIntCol) > 10 ~ 10,
                TRUE ~ floor(!!rlang::sym(logIntCol))))
    }

    contrastDF <- contrastDF %>%
        dplyr::mutate(negLog10P = -log10(!!rlang::sym(pvalCol)),
                      Group = dplyr::case_when(
                          (!!rlang::sym(pvalCol) <= pthreshold) & (!!rlang::sym(logRatioCol) < -log2(foldChangeThreshold)) ~ "Decreased",
                          (!!rlang::sym(pvalCol) <= pthreshold) & (!!rlang::sym(logRatioCol) > log2(foldChangeThreshold)) ~ "Increased",
                          TRUE ~  "No Change")) %>%
            dplyr::arrange(Group)

    if (plotType == "canvasxpress") {

        symbolColor <- sapply(c("deepskyblue4", "red3", "grey25"), .rgbaConversion, alpha = 0.5, USE.NAMES = FALSE)

        group <- c("Decreased", "Increased", "No Change")

        ssc <- data.frame(group, symbolColor, row.names = NULL) %>%
            dplyr::filter(group %in% unique(contrastDF$Group))

        decorations <- list()

        if (!is.null(pthresholdLine)) {
            pthresholdLine <- .rgbaConversion(pthresholdLine, alpha = 0.5)
            decorations   <- .getCxPlotDecorations(decorations = decorations,
                                                   color       = pthresholdLine,
                                                   width       = 2,
                                                   y           = -log10(pthreshold))
        }

        if (!is.null(foldChangeThreshold)) {
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                 color       = symbolColor[2],
                                                 width       = 2,
                                                 x           = log2(foldChangeThreshold))
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                 color       = symbolColor[1],
                                                 width       = 2,
                                                 x           = -log2(foldChangeThreshold))
        }

        events <- htmlwidgets::JS("{ 'mousemove' : function(o, e, t) {
                                                if (o != null && o != false) {
                                                  if (o.y != null &&
                                                      o.y.data != null &&
                                                      o.y.smps != null) {
                                                      if (o.z != null && o.z['GeneName'] != null) {
                                                        info  = '<b>' + o.z['GeneName'] + '</b>'+ ' (' + o.y.vars[0] + ')';
                                                      } else {
                                                        info = '<b>' + o.y.vars[0]  + '</b>' ;
                                                      }
                                                      info = info + '</br>' +
                                                             '<i>' + o.z.Group  + '</i><br/>' +
                                                             'logFC: ' +  o.y.data[0][0] + '<br/>' +
                                                             '-log-pVal: ' +  o.y.data[0][1] ;
                                                    t.showInfoSpan(e, info);
                                                  }
                                                }; }}")

        cx.data <- contrastDF %>%
            dplyr::select(all_of(logRatioCol), negLog10P)

        if (sizeByIntensity) {
            var.annot <- contrastDF %>%
                dplyr::select(Group, LogInt)
            sizeBy <- "LogInt"
            sizes <- c(4, 8, 10, 12, 14)
        } else {
            var.annot <- contrastDF %>%
                dplyr::select(Group)
            sizeBy <- "Group"
            sizes <- c(10, 10, 4)
        }

        if (!missing(geneNameCol)) {
            gene_data <- DGEobj::getItem(dgeObj, "geneData") %>%
                dplyr::select(all_of(geneNameCol))

            var.annot <- merge(var.annot, gene_data, by = 0, all = TRUE, sort = FALSE) %>%
                tibble::column_to_rownames(var = "Row.names") %>%
                dplyr::rename(GeneName = all_of(geneNameCol))
        }

        canvasXpress::canvasXpress( data              = cx.data,
                                    varAnnot          = var.annot,
                                    decorations       = decorations,
                                    graphType         = "Scatter2D",
                                    colorBy           = "Group",
                                    colors            = ssc$symbolColor,
                                    legendPosition    = "right",
                                    showDecorations   = TRUE,
                                    title             = title,
                                    xAxisTitle        = xlab,
                                    yAxisTitle        = ylab,
                                    sizeBy            = sizeBy,
                                    sizes             = sizes,
                                    events            = events)

    } else {

        group <- c("Decreased", "Increased", "No Change")
        symbolColor <- c("deepskyblue4", "red3", "grey25")

        ssc <- data.frame(group, symbolColor, row.names = NULL) %>%
            dplyr::filter(group %in% unique(contrastDF$Group))

        volcanoPlot <- ggplot(contrastDF, aes_string(y = "negLog10P", x = logRatioCol)) +
            aes(shape = Group,
                color = Group,
                fill  = Group) +
            # Scale lines tell it to use the actual values, not treat them as factors
            scale_color_manual(name   = "Group",
                               guide  = "legend",
                               labels = ssc$group,
                               values = ssc$symbolColor) +
            scale_fill_manual(name   = "Group",
                              guide  = "legend",
                              labels = ssc$group,
                              values = ssc$symbolColor) +
            geom_point(alpha = 0.5)

        # Optional Decorations
        if (sizeByIntensity) {
            volcanoPlot <- volcanoPlot + aes(size = LogInt) +
                scale_size_continuous() +
                scale_shape_manual(name   = "Group",
                                   guide  = "legend",
                                   labels = ssc$group,
                                   values = rep("circle", 3))

        } else {
            volcanoPlot <- volcanoPlot + aes(size = Group) +
                scale_size_manual(name   = "Group",
                                  guide  = "legend",
                                  labels = ssc$group,
                                  values = c(4, 4, 2)) +
                scale_shape_manual(name   = "Group",
                                   guide  = "legend",
                                   labels = ssc$group,
                                   values = rep("circle", 3))
        }

        if (!is.null(pthresholdLine)) {
            volcanoPlot <- volcanoPlot +
                geom_hline(yintercept = -log10(pthreshold),
                           color      = pthresholdLine,
                           size       = 2,
                           alpha      = 0.5)
        }

        if (!is.null(foldChangeThreshold)) {
            volcanoPlot <- volcanoPlot +
                geom_vline(xintercept = log2(foldChangeThreshold),
                           color      = "red3",
                           size       = 2,
                           alpha      = 0.5) +
                geom_vline(xintercept = -log2(foldChangeThreshold),
                           color      = "deepskyblue4",
                           size       = 2,
                           alpha      = 0.5)
        }

        # Add axis Labels
        if (is.null(xlab)) {
            volcanoPlot <- volcanoPlot + xlab(logRatioCol)
        } else {
            volcanoPlot <- volcanoPlot + xlab(xlab)
        }
        if (is.null(ylab)) {
            volcanoPlot <- volcanoPlot + ylab("negLog10P")
        } else {
            volcanoPlot <- volcanoPlot + ylab(ylab)
        }
        if (!is.null(title)) {
            volcanoPlot <- volcanoPlot +
                ggtitle(title)
        }

        volcanoPlot + theme(legend.position = "right")
    }
}
