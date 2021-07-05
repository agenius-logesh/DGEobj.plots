#' Create volcano plot
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
#' Sensible defaults are chosen for symbols (Size, Shape, Color, and Fill), but they can be
#' adjusted through the use of optional arguments. A length of 3 is
#' required for these arguments which applies the attributes in this order:
#' Increased, NoChange, Decreased.
#'
#' @param contrastDF A dataframe with LogRatio and LogIntensity columns and optionally a
#'   p-value or FDR column (typically a topTable dataframe).
#' @param plotType Plot type must be canvasXpress or ggplot (Default to canvasXpress).
#' @param logRatioCol Name of the LogRatio column (Default = "logFC")
#' @param logIntCol Name of the LogIntensity column (Default = "AveExpr")
#' @param pvalCol Name of the p-value or FDR column (Default = "P.Value")
#' @param xlab X axis label (Default is the LogIntensity column name)
#' @param ylab Y axis label (Default is the LogRatio column name)
#' @param title Plot title (optional)
#' @param pthreshold Used to color points (Default = 0.01)
#' @param geneSymLabels A character vector of gene to label (must be the name space of the column
#'   specified by geneSymCol)
#' @param geneSymCol Name of the gene symbol column in contrastDF.  The gene symbol is
#'    not in topTable output by default so the user has to bind this column
#'    to the dataframe in advance.  This column will be used to label
#'    significantly changed points.
#' @param pthresholdLine Color for a horizontal line at the p-threshold (Default
#'   = NULL (disabled))
#' @param symbolSize Size of symbols for Up, no change, and Down. default = c(4,
#'        3.99, 4); Note: All three cannot be the same size. Decimal values are acceptable to help offset that
#'        (e.g. 4, 4.1, 4.2).
#' @param symbolShape Shape of the symbols for Up, no change, and Down; Default =
#'        c(21, 1, 21) (1 = open circle, 21 = fillable open circle); Note: The same symbol shape cannot
#'        be selected for all three symbols. See
#'        \url{http://www.cookbook-r.com/Graphs/Shapes_and_line_types}
#' @param symbolColor c(Up, NoChange, Down); default = c("black", "grey25",
#'   "grey0") See \url{http://research.stowers-institute.org/efg/R/Color/Chart}
#'   Note: Colors cannot be duplicated.
#' @param symbolFill Set the fill color for the symbols. Note only symbols 21-25
#'   are fillable. This will have no effect on other symbols. Default =
#'   c("red3", "grey25", "deepskyblue4") Note: Colors cannot be duplicated.
#' @param alpha Controls the transparency of the plotted points (range: 0-1;
#'   default = 0.7)
#' @param sizeByIntensity If TRUE, creates a column to support sizeByIntensity. (Default = TRUE)
#' @param foldChangeLines Position of reference vertical lines for fold change
#'   (Default = log2(1.5); NULL disables)
#' @param legendPosition One of "top", "bottom", "left", "right", "ne", "se",
#'   "nw", "sw", NULL. top/bottom/left/right place the legend outside the
#'   figure.  ne/se/nw/sw place the figure inside the figure. NULL disables the
#'   legend. Default = "right"
#' @param refLineThickness Set the thickness for all reference lines (Default =
#'   1)
#' @param footnote Optional string placed right justified at bottom of plot.
#' @param footnoteSize Applies to footnote. (Default = 3)
#' @param footnoteColor Applies to footnote. (Default = "black")
#' @param footnoteJust Value 0 - 1. 0 is left justified, 1 is right justified, 0.5 is centered. (Default = 1)
#'
#' @return canvasxpress or ggplot object based on plotType selection
#'
#' @examples
#' \dontrun{
#'    # Simple plot with custom title (contrastDF is a topTable dataframe)
#'    myPlot <- volcanoPlot(contrastDF, title = "Plot Title")
#'
#'    # Some options with a custom datafile
#'    myPlot <- volcanoPlot(contrastDF,
#'                          pthreshold = 0.1,
#'                          logRatioCol = "Log2ratio",
#'                          logIntCol = "AverageIntensity",
#'                          pvalCol = "BHFDR",
#'                          xlab = "Log2 Ratio", ylab = "Log10 BHFDR",
#'                          title = "Profile Plot Title",
#'                          referenceLine = "blue",
#'                          legendPosition = "right")
#' }
#'
#' @import ggplot2 magrittr
#' @importFrom dplyr left_join
#' @importFrom ggrepel geom_text_repel
#' @importFrom canvasXpress canvasXpress
#' @importFrom htmlwidgets JS
#'
#' @export
volcanoPlot <- function(contrastDF,
                        plotType = "canvasXpress",
                        logRatioCol = "logFC",
                        logIntCol = "AveExpr",
                        pvalCol = "P.Value",
                        pthreshold = 0.01,
                        geneSymLabels,
                        geneSymCol,
                        xlab = NULL,
                        ylab = NULL,
                        title = NULL,
                        symbolSize = c(10, 4, 10),
                        symbolShape = c("circle", "circle", "circle"),
                        symbolColor = c("red3", "grey25", "deepskyblue4"),
                        sizeByIntensity = FALSE,
                        transparency = 0.5,
                        referenceLine = NULL,
                        foldChangeLines = log2(1.5),
                        refLineThickness = 2,
                        legendPosition = "right",
                        footnote) {

    ##### Asserts
    assertthat::assert_that(!missing(contrastDF),
                            !is.null(contrastDF),
                            "data.frame" %in% class(contrastDF),
                            nrow(contrastDF) > 0,
                            msg = "contrastDF must be specified as dataframe with LogIntensity and LogRatio columns and optionally a p-value")

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
                            msg = "logRatioCol column not found in contrastDF.")

    assertthat::assert_that(!is.null(logIntCol),
                            logIntCol %in% colnames(contrastDF),
                            msg = "logIntCol column not found in contrastDF.")

    assertthat::assert_that(!is.null(pvalCol),
                            pvalCol %in% colnames(contrastDF),
                            msg = "pvalCol column not found in contrastDF.")

    if (!missing(geneSymCol)) {
        assertthat::assert_that(!is.null(geneSymCol),
                                geneSymCol %in% colnames(contrastDF),
                                msg = "geneSymCol column not found in contrastDF.")
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

    if (any(is.null(symbolSize),
            !is.numeric(symbolSize),
            length(symbolSize)  != 3,
            length(unique(symbolSize)) < 2,
            !all(symbolSize >= 0))) {
        warning("symbolSize must be a vector of 3 integer values, at least 2 of them are different. Assigning default values 10, 4, 10.")
        symbolSize  <-  c(10, 4, 10)

    }

    if (any(is.null(symbolShape),
            !is.character(symbolShape),
            length(symbolShape)  != 3,
            plotType == "canvasxpress" && !is.null(symbolShape) && length(.validate_cx_shapes(symbolShape)) != 3,
            plotType == "ggplot" && !is.null(symbolShape) && length(.validate_gg_shapes(symbolShape)) != 3)) {
        warning("symbolShape must be a vector of 3 charcter values. Assigning default values 'circle', 'circle', 'circle'.")
        symbolShape  <- c("circle", "circle", "circle")

    }

    if (any(is.null(symbolColor),
            !is.character(symbolColor),
            length(symbolColor)  != 3,
            length(.validate_colors(symbolColor)) != 3)) {
        warning("symbolColor must be a vector of 3 character values. Assigning default values 'red3', 'grey25', 'deepskyblue4'.")
        symbolColor <- c("red3", "grey25", "deepskyblue4")
    }

    if (any(is.null(transparency),
            !is.numeric(transparency),
            length(transparency) != 1,
            transparency <= 0,
            transparency > 1)) {
        warning("transparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.5'.")
        transparency <- 0.5
    }

    if (!is.null(referenceLine) &&
        !all(is.character(referenceLine), length(referenceLine) == 1)) {
        warning("referenceLine must be a singular value of class character or 'NULL' to disable. Assigning default value 'darkgoldenrod1'.")
        referenceLine <- "darkgoldenrod1"
    } else if (.rgbaConversion(referenceLine) == "invalid value") {
        warning("Color specified is not valid. Assigning default value 'darkgoldenrod1'.")
        referenceLine <- "darkgoldenrod1"
    }

    if (any(is.null(refLineThickness),
            !is.numeric(refLineThickness),
            length(refLineThickness) != 1,
            refLineThickness < 0)) {
        warning("refLineThickness must be a singular value of class numeric Assigning default value '1'.")
        refLineThickness <- 1
    }

    if (!is.null(legendPosition) &&
        !all(is.character(legendPosition),
             length(legendPosition) == 1,
             legendPosition %in% c("top", "bottom", "left", "right", "ne", "se", "nw", "sw"))) {
        warning("legendPosition must be one value from 'top', 'bottom', 'left', 'right', 'ne', 'se', 'nw', 'sw' or 'NULL' to disable. Assigning default value 'right'.")
        legendPosition <- "right"
    }

    if (missing(footnote)) {
        footnote <- NULL
    } else if (!is.null(footnote) &&
               !all(is.character(footnote),
               length(footnote) == 1)) {
        warning("footnote must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'.")
        footnote <- NULL
    }

    if (any(is.null(sizeByIntensity),
            !is.logical(sizeByIntensity),
            length(sizeByIntensity) != 1)) {
        warning("sizeByIntensity must be a singular logical value. Assigning default value FALSE")
        sizeByIntensity = FALSE
    }

    # # # Columns to plot
    # # # Capture the labels from the colname
    # xlabel <- make.names(logIntCol)
    # ylabel <- make.names(logRatioCol)
    # Group <- NULL
    # negLog10P <- NULL
    # # Now make the columnames suitable for use with aes_string
    # colnames(contrastDF)[colnames(contrastDF) %in% logIntCol] <- xlabel
    # colnames(contrastDF)[colnames(contrastDF) %in% logRatioCol] <- ylabel


    if (sizeByIntensity) {
        contrastDF <- contrastDF %>%
            dplyr::mutate("LogInt"    = dplyr::case_when(
                              AveExpr < 0 ~ 0,
                              AveExpr > 10 ~ 10,
                              TRUE ~ floor(AveExpr)))
    }

    contrastDF <- contrastDF %>%
        dplyr::mutate(negLog10P = -log10(!!sym(pvalCol)),
                          "Group" = dplyr::case_when(
                                (!!rlang::sym(pvalCol) <= pthreshold) & (logFC < -foldChangeLines) ~ "Decreased",
                                (!!rlang::sym(pvalCol) <= pthreshold) & (logFC > foldChangeLines) ~ "Increased",
                                 TRUE ~  "No Change")) %>%
            dplyr::arrange(Group)


    if (plotType == "canvasxpress") {
        symbolColor <- sapply(symbolColor, .rgbaConversion, alpha = transparency, USE.NAMES = FALSE)

        sizes   <- symbolSize[c(3,1,2)]
        colors  <- symbolColor[c(3,1,2)]
        shapes  <- symbolShape[c(3,1,2)]
        decorations <- list()

        if (!is.null(referenceLine)) {
            referenceLine <- .rgbaConversion(referenceLine, alpha = transparency)
            decorations   <- .getCxPlotDecorations(decorations = decorations,
                                                   color = referenceLine,
                                                   width = refLineThickness,
                                                   x     = 0)
        }
        if (!is.null(foldChangeLines)) {
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                 color       = colors[2],
                                                 width       = refLineThickness,
                                                 x           = foldChangeLines)
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                 color       = colors[1],
                                                 width       = refLineThickness,
                                                 x           = -1 * foldChangeLines)
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
                                                      if (o.z != null && o.z['geneSym'] != null) {
                                                        info  = info + '<br/>' +
                                                              '<b> Symbol</b>' + ': ' + o.z['geneSym'] ;
                                                      }
                                                    t.showInfoSpan(e, info);
                                                  }
                                                }; }}")

        cx.data <- contrastDF %>% dplyr::select(c(logRatioCol, negLog10P))

        if (missing(geneSymCol) && sizeByIntensity) {
            var.annot <- contrastDF %>% dplyr::select(Group, LogInt)
            sizeBy <- "LogInt"
            showSizeLegend <- TRUE
        } else if (!missing(geneSymCol) && !sizeByIntensity) {
            var.annot <- contrastDF %>% dplyr::select(c(Group,geneSymCol))
            colnames(var.annot)[colnames(var.annot) %in% geneSymCol] <- "geneSym"
            sizeBy <- "Group"
            showSizeLegend <- TRUE
        } else if (!missing(geneSymCol) && sizeByIntensity) {
            var.annot <- contrastDF %>% dplyr::select(Group, geneSymCol, LogInt)
            colnames(var.annot)[colnames(var.annot) %in% geneSymCol] <- "geneSym"
            sizeBy <- "LogInt"
            showSizeLegend <- TRUE
        } else {
            var.annot <- contrastDF %>% dplyr::select(Group)
            sizeBy  <- "Group"
            showSizeLegend <- TRUE
        }

        cx_params <- list(data             = cx.data,
                          varAnnot         = var.annot,
                          decorations      = decorations,
                          graphType        = "Scatter2D",
                          colorBy          = "Group",
                          colors           = colors,
                          shapes           = shapes,
                          legendPosition   = legendPosition,
                          showDecorations  = TRUE,
                          sizeByShowLegend = showSizeLegend,
                          title            = title,
                          xAxisTitle       = xlab,
                          yAxisTitle       = ylab,
                          sizeBy           = sizeBy,
                          citation         = footnote,
                          events           = events)
        if (sizeBy == "Group") {
            cx_params <- c(cx_params, list(sizes = sizes))
        }
        do.call(canvasXpress::canvasXpress, cx_params)
    } else {
        groupNames <- c("Increased", "No Change", "Decreased")
        names(symbolShape) <-  groupNames
        names(symbolSize)  <-  groupNames
        names(symbolColor) <-  groupNames

        ssc  <-  data.frame(group = factor(groupNames, levels = groupNames),
                            symbolShape = symbolShape,
                            symbolSize = symbolSize,
                            symbolColor = symbolColor,
                            stringsAsFactors = FALSE)

        volcanoPlot <- ggplot(contrastDF, aes_string(y = "negLog10P" , x = logRatioCol)) +
            aes(shape = Group,
                color = Group,
                fill = Group) +
            # Scale lines tell it to use the actual values, not treat them as factors
            scale_shape_manual(name = "Group", guide = "legend", labels = ssc$group,
                               values = ssc$symbolShape) +
            scale_color_manual(name = "Group", guide = "legend", labels = ssc$group,
                               values = ssc$symbolColor) +
            scale_fill_manual(name = "Group", guide = "legend", labels = ssc$group,
                              values = ssc$symbolColor) +
            geom_point(alpha = transparency)


        # Optional Decorations
        if (sizeByIntensity) {
            volcanoPlot <- volcanoPlot + aes(size = LogInt) +
                scale_size_continuous()
        } else {
            volcanoPlot <- volcanoPlot + aes(size = Group) +
                scale_size_manual(name = "Group", guide = "legend", labels = ssc$group,
                                  values = ssc$symbolSize)
        }

        if (!is.null(referenceLine)) {
            volcanoPlot <- volcanoPlot +
                geom_vline(xintercept = 0,
                           color = "green",
                           size = refLineThickness,
                           alpha = transparency)
        }

        if (!is.null(foldChangeLines)) {
            volcanoPlot <- volcanoPlot +
                geom_vline(xintercept = foldChangeLines,
                           color = symbolColor["Increased"],
                           size = refLineThickness,
                           alpha = transparency) +
                geom_vline(xintercept = -foldChangeLines,
                           color = symbolColor["Decreased"],
                           size = refLineThickness,
                           alpha = transparency)
        }

        # Add genesym labels to increased, decreased genes
        if (!missing(geneSymLabels) && !missing(geneSymCol)) {
            idx <- contrastDF[[geneSymCol]] %in% geneSymLabels
            contrastDFsubset <- contrastDF[idx,]
            volcanoPlot <- volcanoPlot +
                ggrepel::geom_text_repel(data = contrastDFsubset,
                                aes_string(x = logRatioCol, y = "negLog10P", label = geneSymCol),
                                show.legend = TRUE)
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

        # Footnote
        if (!missing(footnote)) {
            volcanoPlot <- addFootnote(volcanoPlot,
                                       footnoteText = footnote,
                                       footnoteSize = 3,
                                       footnoteColor = "black",
                                       footnoteJust = 1)
        }
        setLegendPosition(volcanoPlot, legendPosition)
    }
}
