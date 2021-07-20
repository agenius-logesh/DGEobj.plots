#' Plot log intensity versus log ratio
#'
#' A profile plot shows Log Intensity on the X axis and Log Ratio on the Y axis.
#' This function is intended to show the profile plot from a dataframe
#' in DGEobj design objects (i.e BDL_vs_SHAM).  Properly normalized data will generally be
#' centered around LogRatio = 0.
#'
#' By default, the plot places "logFC" on the Y axis and "AveExpr" on the X
#' axis. By default, a reference horizontal line is shown at LogRatio = 0 on the
#' Y axis. Optionally, additional reference lines will be drawn at +/- a user
#' supplied LogRatio threshold. A Loess line fit is drawn through the actual
#' data. The points are color coded using the significance measure (i.e p-value)
#' supplied by the user. By default, the P.Value field is used
#' with a threshold of 0.01 to color code the points.
#'
#' \strong{Data Structure for the input dataframe:}
#'
#' The defaults are set for the contrasts on the DGEobj object (ie in BDL_vs_SHAM).
#' The columns named "logFC", "AveExpr", and "P.Value" are used by default to accommodate
#' the column names used in these dataframes.  Any other dataframe can be used with fold-change,
#' intensity, and significance measures, with appropriate arguments to define the column names
#' to use provided. By default, the column names will be used for the axis labels,
#' but can be overridden with xlab and ylab arguments.
#'
#' A significance measure (which defaults to P.Value <= 0.01) is used to color code genes that
#' are significantly increased or decreased.
#'
#' Sensible defaults are chosen for symbols (Size, Shape, Color, and Fill), but they can be
#' adjusted through the use of optional arguments. A length of 3 is
#' required for these arguments which applies the attributes in this order:
#' Increased, NoChange, Decreased.
#'
#' @param contrastDF dataframe with LogIntensity and LogRatio columns and optionally a p-value.
#' @param plotType Plot type must be canvasXpress or ggplot (default = "canvasXpress").
#' @param logRatioCol Name of the LogRatio column (default = "logFC")
#' @param logIntCol Name of the LogIntensity column (default = "AveExpr")
#' @param pvalCol Name of the p-value (default = "P.Value")
#' @param xlab X axis label (defaults = "LogIntensity column name")
#' @param ylab Y axis label (defaults = "LogRatio column name")
#' @param title Plot title (optional)
#' @param pthreshold Used to color points (default = 0.01)
#' @param geneSymLabels Gene Symbols to be labeled. For canvasXpress, these information will
#'        be displayed for each data point.
#' @param geneSymCol Name of the gene symbol column in contrastDF.  The gene symbol is
#'        not in topTable output by default so the user has to bind this column
#'        to the dataframe in advance.  Then this column will be used to label
#'        significantly changed points for ggplot type. For canvasXpress plot type,
#'        gene information is displayed in the tooltip.
#' @param symbolSize Size of symbols for Up, no change, and Down (default = c(10, 4, 10));
#'        Note: All three cannot be the same size. Decimal values are acceptable to help offset that
#'        (e.g. 4, 4.1, 4.2).
#' @param symbolShape Shape of the symbols for Up, no change, and Down;
#'        (default = c("circle", "circle", "circle").
#'        See \url{http://www.cookbook-r.com/Graphs/Shapes_and_line_types}
#' @param symbolColor c(Up, NoChange, Down) (default = c("red3", "grey25", "deepskyblue4"))
#'        See \url{http://research.stowers-institute.org/efg/R/Color/Chart}
#'        Note: Colors cannot be duplicated.
#' @param transparency Controls the transparency of the plotted points (0-1; default = 0.5)
#' @param sizeBySignificance Set to TRUE to size points by the negative Log10 of the
#'        Significance measure (default = FALSE)
#' @param referenceLine Color for an intercept = 0 horizontal reference line
#'        (default = "darkgoldenrod1"; NULL disables)
#' @param foldChangeLines Position of reference horizontal lines for fold change
#'        (default = log2(1.5); NULL disables)
#' @param refLineThickness Controls size of the horizontal reference line through geom_hline (default = 1)
#' @param lineFitType Enable a line fit through the data (default = "loess";
#'        "lm" produces a linear fit. NULL disables)
#' @param lineFitColor Color for the fit line (default = "goldenrod1")
#' @param legendPosition One of "top", "bottom", "left", "right", "ne", "se", "nw", "sw", NULL.
#'        top/bottom/left/right place the legend outside the figure.  ne/se/nw/sw place the figure
#'        inside the figure. NULL disables the legend. Default = "right"
#' @param footnote Optional string placed right justified at bottom of plot.
#'
#' @return canvasxpress or ggplot object based on plotType selection
#'
#' @examples
#' \dontrun{
#'    # Get DGEObj
#'    t_obj1 <- readRDS(system.file("exampleObj.RDS", package = "DGEobj", mustWork = TRUE))
#'    # Get Contrast Data to plot
#'    contrastDF <- t_obj1$BDL_vs_Sham
#'    myPlot <- profilePlot(contrastDF, title = "BDL_vs_Sham")
#'
#'    # Some options with a custom datafile
#'    myPlot <- profilePlot(contrastDF,
#'                          pthreshold = 0.1,
#'                          title = "BDL_vs_Sham",
#'                          referenceLine = "blue",
#'                          legendPosition = "ne")
#' }
#'
#' @import ggplot2 magrittr
#' @importFrom dplyr left_join filter arrange mutate case_when
#' @importFrom assertthat assert_that
#' @importFrom ggrepel geom_text_repel
#' @importFrom canvasXpress canvasXpress
#' @importFrom htmlwidgets JS
#'
#' @export
profilePlot <- function(contrastDF,
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
                        transparency = 0.5,
                        sizeBySignificance = FALSE,
                        referenceLine = "darkgoldenrod1",
                        foldChangeLines = log2(1.5),
                        refLineThickness = 1,
                        lineFitType = "loess",
                        lineFitColor = "goldenrod1",
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
        !all(is.character(title), length(title) == 1)) {
        warning("title must be a singular value of class character. Assigning default value 'NULL'.")
        title <- NULL
    }

    if (!is.null(xlab) &&
        !all(is.character(xlab), length(xlab) == 1)) {
        warning("xlab must be a singular value of class character. Assigning default value 'NULL'.")
        xlab <- NULL
    }

    if (!is.null(ylab) &&
        !all(is.character(ylab), length(ylab) == 1)) {
        warning("ylab must be a singular value of class character. Assigning default value 'NULL'.")
        ylab <- NULL
    }

    if (!is.null(lineFitType) &&
        any(length(lineFitType) != 1,
            !tolower(lineFitType) %in% c('glm', 'lm', 'loess', 'gam'))) {
        warning("lineFitType must be one of 'glm', 'lm', 'loess', 'gam' or NULL to disable. Assigning default value 'loess'.")
        lineFitType <- "loess"
    }

    if (!is.null(lineFitType) &&
        any(is.null(lineFitColor),
            !is.character(lineFitColor),
            length(lineFitColor) != 1)) {
        warning("lineFitColor must be a singular value of class character. Assigning default value 'goldenrod1'.")
        lineFitColor <- "goldenrod1"
    } else if (.rgbaConversion(lineFitColor) == "invalid value") {
        warning("Color specified is not valid. Assigning default value 'goldenrod1'.")
        lineFitColor <- "goldenrod1"
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
               !all(is.character(footnote), length(footnote) == 1)) {
        warning("footnote must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'.")
        footnote <- NULL
    }

    if (any(is.null(sizeBySignificance),
            !is.logical(sizeBySignificance),
            length(sizeBySignificance) != 1)) {
        warning("sizeBySignificance must be a singular logical value. Assigning default value FALSE")
        sizeBySignificance = FALSE
    }
    # Columns to plot
    # Capture the labels from the colname
    xlabel <- make.names(logIntCol)
    ylabel <- make.names(logRatioCol)
    # Now make the columnames suitable for use with aes_string
    colnames(contrastDF)[colnames(contrastDF) %in% logIntCol] <- xlabel
    colnames(contrastDF)[colnames(contrastDF) %in% logRatioCol] <- ylabel
    if (sizeBySignificance) {
        contrastDF <- contrastDF %>%
            dplyr::mutate(negLog10P = -log10(!!sym(pvalCol)))
    }
    contrastDF <- contrastDF %>%
        dplyr::mutate(Group = dplyr::case_when(!!sym(pvalCol) > pthreshold ~ "No Change",
                                               !!sym(logRatioCol) > 0 ~ "Increased",
                                               TRUE ~"Decreased"),
                      Group = factor(Group,
                                     levels = c("Increased", "No Change", "Decreased")))
    if (plotType == "canvasxpress") {
        symbolColor <- sapply(symbolColor, .rgbaConversion, alpha = transparency, USE.NAMES = FALSE)
        cx.data <- contrastDF %>%
            dplyr::select(c(xlabel, ylabel))
        sizes   <- symbolSize[c(3,1,2)]
        colors  <- symbolColor[c(3,1,2)]
        shapes  <- symbolShape[c(3,1,2)]
        decorations <- list()
        if (!is.null(referenceLine)) {
            referenceLine <- .rgbaConversion(referenceLine, alpha = transparency)
            decorations   <- .getCxPlotDecorations(decorations = decorations,
                                                   color = referenceLine,
                                                   width = refLineThickness,
                                                   y     = 0)
        }
        if (!is.null(foldChangeLines)) {
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                 color       = colors[2],
                                                 width       = refLineThickness,
                                                 y           = foldChangeLines)
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                 color       = colors[1],
                                                 width       = refLineThickness,
                                                 y           = -1 * foldChangeLines)
        }
        events <- htmlwidgets::JS("{ 'mousemove' : function(o, e, t) {
                                                if (o != null && o != false) {
                                                  if (o.y != null &&
                                                      o.y.data != null &&
                                                      o.y.smps != null) {
                                                      info = '<b>' + o.y.vars[0]  + '</b>' + '<br/>' +
                                                             '<b>' + o.y.smps[0]  + '</b>' + ': ' + o.y.data[0][0] + '<br/>' +
                                                             '<b>' + o.y.smps[1]  + '</b>' + ': ' + o.y.data[0][1] ;
                                                      if (o.z != null && o.z['rgd_symbol'] != null) {
                                                        info  = info + '<br/>' +
                                                              '<b> Symbol</b>' + ': ' + o.z['rgd_symbol'] ;
                                                      }
                                                    t.showInfoSpan(e, info);

                                                  }
                                                }; }}")
        if (sizeBySignificance) {
            var.annot <- contrastDF %>%
                dplyr::select(Group, negLog10P)
            showSizeLegend <- TRUE
            sizeBy <- "negLog10P"
        } else {
            var.annot <- contrastDF %>%
                dplyr::select(Group)
            showSizeLegend <- FALSE
            sizeBy  <- "Group"
        }

        if (!missing(geneSymCol)) {
            var.annot <- merge(var.annot, contrastDF %>%
                                   dplyr::select(!!dplyr::sym(geneSymCol)),
                               by = 0)
            rownames(var.annot) <- var.annot$Row.names
            var.annot$Row.names <- NULL
        }

        showLoessFit <- FALSE
        afterRender  <- NULL
        if (!is.null(lineFitType)) {
            lineFitType  <- .cxSupportedLineFit(lineFitType)
            lineFitColor <- .rgbaConversion(lineFitColor, alpha = transparency)

            if (lineFitType == "lm") {
                afterRender <- list(list("addRegressionLine"))
            } else if (lineFitType == "loess") {
                showLoessFit <- TRUE
            }
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
                          showLoessFit     = showLoessFit,
                          fitLineColor     = lineFitColor,
                          sizeByShowLegend = showSizeLegend,
                          title            = title,
                          xAxisTitle       = xlab,
                          yAxisTitle       = ylab,
                          sizeBy           = sizeBy,
                          citation         = footnote,
                          events           = events,
                          afterRender      = afterRender)
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
        profilePlot <- ggplot(contrastDF, aes_string(x = xlabel, y = ylabel)) +
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
        if (sizeBySignificance) {
            profilePlot <- profilePlot + aes(size = negLog10P) +
                scale_size_continuous()
        } else {
            profilePlot <- profilePlot + aes(size = Group) +
                scale_size_manual(name = "Group", guide = "legend", labels = ssc$group,
                                  values = ssc$symbolSize)
        }

        if (!is.null(referenceLine)) {
            profilePlot <- profilePlot +
                geom_hline(yintercept = 0,
                           color = referenceLine,
                           size = refLineThickness,
                           alpha = 0.5)
        }

        if (!is.null(foldChangeLines)) {
            profilePlot <- profilePlot +
                geom_hline(yintercept = foldChangeLines,
                           color = symbolColor["Increased"],
                           size = refLineThickness,
                           alpha = 0.5) +
                geom_hline(yintercept = -foldChangeLines,
                           color = symbolColor["Decreased"],
                           size = refLineThickness,
                           alpha = 0.5)
        }

        if (!is.null(lineFitType)) {
            profilePlot <- profilePlot +
                geom_smooth(formula = 'y ~ x',
                            aes(group = NULL,
                                shape = NULL,
                                size = NULL,
                                color = NULL,
                                fill = NULL),
                            method = tolower(lineFitType),
                            size = refLineThickness,
                            color = lineFitColor,
                            alpha = transparency,
                            se = FALSE,
                            show.legend = FALSE)
        }

        # Add genesym labels to increased, decreased genes
        if (!missing(geneSymLabels) && !missing(geneSymCol)) {
            idx <- contrastDF[[geneSymCol]] %in% geneSymLabels
            contrastDFsubset <- contrastDF[idx,]
            profilePlot <- profilePlot +
                geom_text_repel(data = contrastDFsubset,
                                aes_string(x = xlabel, y = ylabel, label = geneSymCol),
                                show.legend = FALSE)
        }

        # Add axis Labels
        if (is.null(xlab)) {
            profilePlot <- profilePlot + xlab(xlabel)
        } else {
            profilePlot <- profilePlot + xlab(xlab)
        }
        if (is.null(ylab)) {
            profilePlot <- profilePlot + ylab(ylabel)
        } else {
            profilePlot <- profilePlot + ylab(ylab)
        }
        if (!is.null(title)) {
            profilePlot <- profilePlot +
                ggtitle(title)
        }

        # Footnote
        if (!missing(footnote)) {
            profilePlot <- addFootnote(profilePlot,
                                       footnoteText = footnote,
                                       footnoteSize = 3,
                                       footnoteColor = "black")
        }
        setLegendPosition(profilePlot, legendPosition)
    }
}
