#' Create volcano#' Create volcano plot
#'
#' A volcano plot shows Log Ratio data on the X axis and Negative Log P-values (NLP) on the
#' Y axis. This function is intended to show the volcano plot from a dataframe
#' created by topTable or topTreat. Properly normalized data will generally be
#' centered around LogRatio = 0.
#'
#' By default, the plot places "logFC" on the X axis and Log10 of the "P.Value" on the Y axis.
#' By default, a reference vertical line is drawn at LogRatio = 0 on the X axis.
#' Optionally, additional reference lines will be drawn at +/- a user supplied Log Ratio threshold.
#' The points are color coded using both the significance and fold-change thresholds supplied by the user.
#' By default, the P.Value field is used with a threshold of 0.01 to color code the points and fold-change
#' threshold of +/- 1.5X.
#'
#' \strong{Data Structure for the input dataframe:}
#'
#' The defaults are set for dataframes produced by topTable and topTreat.  The columns named "logFC"
#' and "P.Value" are used by default to accommodate the column
#' names used in topTable/topTreat dataframes.  Any other dataframe
#' can be used with fold-change, intensity, and significance measures, with appropriate
#' arguments to define the column names to use provided. By default, the
#' column names will be used for the axis labels, but can be overridden with xlab and ylab arguments.
#'
#' A significance measure (which defaults to P.Value <= 0.01) and LogRatio
#' threshold are used to color code genes that are significantly increased or decreased.
#' Use the appropriate arguments to use an FDR measure instead of p-value.
#'
#' Sensible defaults are chosen for symbols (Size, Shape,  and Color), but they can be
#' adjusted through the use of optional arguments. A length of 3 is
#' required for these arguments which applies the attributes in this order:
#' Increased, NoChange, Decreased.
#'
#' @param DGEdata Name of DGEobj data with a class of DGEobj.
#' @param contrast A character vector of a topTable data in DGEobj and its a class of dataframe
#'   with LogRatio and LogIntensity columns and optionally a
#'   p-value or FDR column (typically a topTable dataframe).
#' @param plotType Plot type must be canvasXpress or ggplot (Default to canvasXpress).
#' @param logRatioCol Name of the LogRatio column (Default = "logFC")
#' @param logIntCol Name of the LogIntensity column (Default = "AveExpr")
#' @param pvalCol Name of the p-value or FDR column (Default = "P.Value")
#' @param xlab X axis label (Default is the LogIntensity column name)
#' @param ylab Y axis label (Default is the LogRatio column name)
#' @param title Plot title (optional)
#' @param pthreshold Used to color points (Default = 0.01)
#' @param geneSymCol Name of the gene symbol column in geneData from the list of DGEobj data. The gene symbol column is
#'    not in topTable output by default it will be in the geneData output.This column will be used to label
#'    significantly changed points.
#' @param pthresholdLine Color for a horizontal line at the p-threshold (Default
#'   = NULL (disabled))
#' @param sizeByIntensity If TRUE, creates a column to support sizeByIntensity. (Default = TRUE)
#' @param foldChangeLines Position of reference vertical lines for fold change
#'   (Default = log2(1.5); NULL disables)
#'
#' @return canvasxpress or ggplot object based on plotType selection
#'
#' @examples
#' \dontrun{
#'    # Simple plot with custom title (DGEdata is a name of DGEobj and
#'      contrast is a name of topTable dataframe)
#'    myPlot <- volcanoPlot(DGEdata, contrast, title = "Plot Title")
#'
#'    # Some options with a custom datafile
#'    myPlot <- volcanoPlot(DGEdata,
#'                          contrast,
#'                          pthreshold = 0.1,
#'                          logRatioCol = "logFC",
#'                          logIntCol = "AveExpr",
#'                          pvalCol = "P.Value",
#'                          xlab = "logFC", ylab = "negLog10p",
#'                          title = "Volcano Plot Title",
#'                          pthresholdLine = "blue")
#' }
#'
#' @import ggplot2 magrittr
#' @importFrom dplyr left_join
#' @importFrom ggrepel geom_text_repel
#' @importFrom canvasXpress canvasXpress
#' @importFrom htmlwidgets JS
#'
#' @export
volcanoPlot <- function(DGEdata,
                        contrast,
                        plotType = "canvasXpress",
                        logRatioCol = "logFC",
                        logIntCol = "AveExpr",
                        pvalCol = "P.Value",
                        pthreshold = 0.01,
                        geneSymCol,
                        xlab = NULL,
                        ylab = NULL,
                        title = NULL,
                        sizeByIntensity = TRUE,
                        pthresholdLine = NULL,
                        foldChangeLines = log2(1.5)) {
    ##### Asserts
    assertthat::assert_that(!missing(DGEdata),
                            !is.null(DGEdata),
                            "DGEobj" %in% class(DGEdata),
                            msg = "DGEdata must be specified as class of DGEobj.")

    assertthat::assert_that(!missing(contrast),
                            !is.null(contrast),
                            contrast %in% names(DGEobj::getType(DGEdata, type = "topTable")),
                            msg = "contrast to be a singular value of class character and must be one from DGEdata with LogIntensity and LogRatio columns and optionally a p-value.")

    contrastDF <- DGEobj::getItems(DGEdata, contrast)

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
                            logRatioCol %in% colnames(contrastDF),
                            msg = "logRatioCol column not found in contrast data.")

    assertthat::assert_that(!is.null(logIntCol),
                            logIntCol %in% colnames(contrastDF),
                            msg = "logIntCol column not found in contrast data.")

    assertthat::assert_that(!is.null(pvalCol),
                            pvalCol %in% colnames(contrastDF),
                            msg = "pvalCol column not found in contrast data.")

    if (!missing(geneSymCol)) {
        assertthat::assert_that(!is.null(geneSymCol),
                                geneSymCol %in% names(getItems(DGEdata, itemNames = "geneData")),
                                msg = "geneSymCol column not found in geneData from DGEdata.")
    }

    if (any(is.null(pthreshold),
            !is.numeric(pthreshold),
            length(pthreshold) != 1)) {
        warning("pthreshold must be a singular numeric value. Assigning default value 0.01")
        pthreshold <- 0.01
    }

    if (any(is.null(foldChangeLines),
            !is.numeric(foldChangeLines),
            length(foldChangeLines) != 1)) {
        warning("foldChangeLines must be a singular numeric value. Assigning default value log2(1.5)")
        foldChangeLines <- log2(1.5)
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
        !all(is.character(pthresholdLine), length(pthresholdLine) == 1)) {
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
                          (!!rlang::sym(pvalCol) <= pthreshold) & (!!rlang::sym(logRatioCol) < -foldChangeLines) ~ "Decreased",
                          (!!rlang::sym(pvalCol) <= pthreshold) & (!!rlang::sym(logRatioCol) > foldChangeLines) ~ "Increased",
                          TRUE ~  "No Change")) %>%
            dplyr::arrange(Group)

    if (plotType == "canvasxpress") {

        symbolColor <- sapply(c("deepskyblue4", "red3", "grey25"), .rgbaConversion, alpha = 0.5, USE.NAMES = FALSE)

        decorations <- list()

        if (!is.null(pthresholdLine)) {
            pthresholdLine <- .rgbaConversion(pthresholdLine, alpha = 0.5)
            decorations   <- .getCxPlotDecorations(decorations = decorations,
                                                   color = pthresholdLine,
                                                   width = 2,
                                                   y     = -log10(pthreshold))
        }

        if (!is.null(foldChangeLines)) {
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                 color       = "red3",
                                                 width       = 2,
                                                 x           = foldChangeLines)
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                 color       = "deepskyblue4",
                                                 width       = 2,
                                                 x           = -foldChangeLines)
        }

        events <- htmlwidgets::JS("{ 'mousemove' : function(o, e, t) {
                                                if (o != null && o != false) {
                                                  if (o.y != null &&
                                                      o.y.data != null &&
                                                      o.y.smps != null) {
                                                      info = '<b>' + o.y.vars[0]  + '</b>' + '<br/>' +
                                                             '<i>' + o.z.Group  + '</i><br/>' +
                                                             '<b>' + o.y.smps[0]  + '</b>' + ': ' + o.y.data[0][0] + '<br/>' +
                                                             '<b>' + o.y.smps[1]  + '</b>' + ': ' + o.y.data[0][1] ;
                                                      if (o.z != null && o.z['GeneName'] != null) {
                                                        info  = info + '<br/>' +
                                                              '<b> Symbol</b>' + ': ' + o.z['GeneName'] ;
                                                      }
                                                    t.showInfoSpan(e, info);
                                                  }
                                                }; }}")

        cx.data <- contrastDF %>% dplyr::select(all_of(logRatioCol), negLog10P)

        if (sizeByIntensity) {
            var.annot <- contrastDF %>% dplyr::select(Group,LogInt)
            sizeBy <- "LogInt"
        } else {
            var.annot <- contrastDF %>%
                dplyr::select(Group)
            sizeBy <- "Group"
        }

        if (!missing(geneSymCol)) {
            gene_data <- DGEobj::getItem(DGEdata, "geneData") %>%
                dplyr::select(all_of(geneSymCol))

            var.annot <- merge(var.annot, gene_data, by = 0, all = TRUE, sort = FALSE) %>%
                tibble::column_to_rownames(var = "Row.names") %>%
                dplyr::rename(GeneName = all_of(geneSymCol))
        }

        canvasXpress::canvasXpress( data              = cx.data,
                                    varAnnot          = var.annot,
                                    decorations       = decorations,
                                    graphType         = "Scatter2D",
                                    colorBy           = "Group",
                                    colors            = symbolColor,
                                    legendPosition    = "right",
                                    showDecorations   = TRUE,
                                    title             = title,
                                    xAxisTitle        = xlab,
                                    yAxisTitle        = ylab,
                                    sizeBy            = sizeBy,
                                    events            = events)

    } else {

        groupNames <- c("Decreased", "Increased", "No Change")

        ssc  <-  data.frame(group = factor(groupNames, levels = groupNames),
                            symbolColor = c("deepskyblue4", "red3", "grey25"),
                            stringsAsFactors = FALSE)

        volcanoPlot <- ggplot(contrastDF, aes_string(y = "negLog10P" , x = logRatioCol)) +
            aes(shape = Group,
                color = Group,
                fill = Group) +
            # Scale lines tell it to use the actual values, not treat them as factors
            scale_color_manual(name = "Group", guide = "legend", labels = ssc$group,
                               values = ssc$symbolColor) +
            scale_fill_manual(name = "Group", guide = "legend", labels = ssc$group,
                              values = ssc$symbolColor) +
            geom_point(alpha = 0.5)

        # Optional Decorations
        if (sizeByIntensity) {
            volcanoPlot <- volcanoPlot + aes(size = LogInt) +
                scale_size_continuous() +
                scale_shape_manual(name = "Group", guide = "legend", labels = ssc$group,
                                   values = rep("circle", 3))

        } else {
            volcanoPlot <- volcanoPlot + aes(size = Group) +
                scale_size_manual(name = "Group", guide = "legend", labels = ssc$group,
                                  values = c(2, 4, 6)) +
                scale_shape_manual(name = "Group", guide = "legend", labels = ssc$group,
                                   values = rep("circle", 3))
        }

        if (!is.null(pthresholdLine)) {
            volcanoPlot <- volcanoPlot +
                geom_hline(yintercept = -log10(pthreshold),
                           color = pthresholdLine,
                           size = 2,
                           alpha = 0.5)
        }

        if (!is.null(foldChangeLines)) {
            volcanoPlot <- volcanoPlot +
                geom_vline(xintercept = foldChangeLines,
                           color = "red3",
                           size = 2,
                           alpha = 0.5) +
                geom_vline(xintercept = -foldChangeLines,
                           color = "deepskyblue4",
                           size = 2,
                           alpha = 0.5)
        }

        # Add genesym labels to increased, decreased genes
        if (!missing(geneSymCol)) {
            gene_data <- DGEobj::getItem(DGEdata, "geneData") %>%
                dplyr::select(all_of(geneSymCol))

            geneSymLabels_df <- merge(contrastDF, gene_data, by = 0, all = TRUE, sort = FALSE) %>%
                tibble::column_to_rownames(var = "Row.names")

            volcanoPlot <- volcanoPlot +
                ggrepel::geom_text_repel(data = geneSymLabels_df,
                                aes_string(x = logRatioCol, y = "negLog10P", label = geneSymCol),
                                show.legend = TRUE, max.overlaps = dim(geneSymLabels_df)[1]*10)
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
