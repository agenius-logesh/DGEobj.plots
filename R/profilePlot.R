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
#' @param dgeObj DGEobj.
#' @param contrast Name of the contrast in dgeObj.
#' @param plotType Plot type must be canvasXpress or ggplot (default = "canvasXpress").
#' @param logRatioCol Name of the LogRatio column (default = "logFC")
#' @param logIntCol Name of the LogIntensity column (default = "AveExpr")
#' @param pvalCol Name of the p-value (default = "P.Value")
#' @param xlab X axis label (defaults = "LogIntensity column name")
#' @param ylab Y axis label (defaults = "LogRatio column name")
#' @param title Plot title (optional)
#' @param pthreshold Used to color points (default = 0.01)
#' @param geneNameCol geneName column in geneData from DGEobj. This column will be used to label
#'    significantly changed points.
#' @param sizeBySignificance Set to TRUE to size points by the negative Log10 of the
#'        Significance measure (default = FALSE)
#' @param referenceLine Color for an intercept = 0 horizontal reference line
#'        (default = "darkgoldenrod1"; NULL disables)
#' @param foldChangeThreshold Position of reference horizontal lines for fold change
#'        (default = 1.5)
#' @param lineFitType Enable a line fit through the data (default = "loess";
#'        "lm" produces a linear fit. NULL disables)
#'
#' @return canvasxpress or ggplot object based on plotType selection
#'
#' @examples
#' \dontrun{
#'    # Get DGEObj
#'    t_obj1 <- readRDS(system.file("exampleObj.RDS", package = "DGEobj", mustWork = TRUE))
#'    # Get Contrast Data to plot
#'    contrast <- names(DGEobj::getType(t_obj1, "topTable"))[1]
#'
#'    # Some options with a custom datafile
#'    myPlot <- profilePlot(t_obj1,
#'                          contrast,
#'                          pthreshold = 0.1,
#'                          title = "BDL_vs_Sham",
#'                          referenceLine = "blue")
#'
#'    myPlot <- profilePlot(t_obj1,
#'                          contrast,
#'                          pthreshold = 0.1,
#'                          title = "BDL_vs_Sham",
#'                          referenceLine = "blue",
#'                          plotType = "ggplot")
#' }
#'
#' @import ggplot2 magrittr
#' @importFrom dplyr filter arrange mutate case_when
#' @importFram rlang sym
#' @importFrom assertthat assert_that
#' @importFrom ggrepel geom_text_repel
#' @importFrom canvasXpress canvasXpress
#' @importFrom htmlwidgets JS
#'
#' @export
profilePlot <- function(dgeObj,
                        contrast,
                        plotType = "canvasXpress",
                        logRatioCol = "logFC",
                        logIntCol = "AveExpr",
                        pvalCol = "P.Value",
                        pthreshold = 0.01,
                        geneNameCol,
                        xlab = NULL,
                        ylab = NULL,
                        title = NULL,
                        sizeBySignificance = FALSE,
                        referenceLine = "darkgoldenrod1",
                        foldChangeThreshold = 1.5,
                        lineFitType = "loess") {
    ##### Asserts
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

    contrastDF <- DGEobj::getItem(dgeObj, contrast)

    assertthat::assert_that(nrow(contrastDF) > 0,
                            "data.frame" %in% class(contrastDF),
                            msg = "The specified contrast does not have a valid topTable associated with it. Re-run the function with a valid contrast.")

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

        assertthat::assert_that(length(names(getType(t_obj1, "geneData"))) == 1,
                                msg = "dgeObj must have exactly one gene object.")

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
        warning("foldChangeThreshold must be a singular numeric value. Assigning default value log2(1.5)")
        foldChangeThreshold <- 1.5
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

    if (!is.null(referenceLine) &&
        !all(is.character(referenceLine), length(referenceLine) == 1)) {
        warning("referenceLine must be a singular value of class character or 'NULL' to disable. Assigning default value 'darkgoldenrod1'.")
        referenceLine <- "darkgoldenrod1"
    } else if (.rgbaConversion(referenceLine) == "invalid value") {
        warning("Color specified is not valid. Assigning default value 'darkgoldenrod1'.")
        referenceLine <- "darkgoldenrod1"
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

        symbolColor <- sapply(c("deepskyblue4", "red3", "grey25"), .rgbaConversion, alpha = 0.5, USE.NAMES = FALSE)

        cx.data <- contrastDF %>%
            dplyr::select(c(xlabel, ylabel))

        group <- c("Decreased", "Increased", "No Change")

        ssc <- data.frame(group, symbolColor, row.names = NULL) %>%
            dplyr::filter(group %in% unique(contrastDF$Group))

        decorations <- list()

        if (!is.null(referenceLine)) {
            referenceLine <- .rgbaConversion(referenceLine, alpha = 0.5)
            decorations   <- .getCxPlotDecorations(decorations = decorations,
                                                   color = referenceLine,
                                                   width = 1,
                                                   y     = 0)
        }

        if (!is.null(foldChangeThreshold)) {
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                 color       = symbolColor[2],
                                                 width       = 1,
                                                 y           = log2(foldChangeThreshold))
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                 color       = symbolColor[1],
                                                 width       = 1,
                                                 y           = -log2(foldChangeThreshold))
        }

        events <- htmlwidgets::JS("{ 'mousemove' : function(o, e, t) {
                                                if (o != null && o != false) {
                                                  if (o.y != null &&
                                                      o.y.data != null &&
                                                      o.y.smps != null) {
                                                      info = '<b>' + o.y.vars[0]  + '</b>' + '<br/>' +
                                                             '<b>' + o.y.smps[0]  + '</b>' + ': ' + o.y.data[0][0] + '<br/>' +
                                                             '<b>' + o.y.smps[1]  + '</b>' + ': ' + o.y.data[0][1] ;
                                                      if (o.z != null && o.z['GeneName'] != null) {
                                                        info  = info + '<br/>' +
                                                              '<b> Symbol</b>' + ': ' + o.z['GeneName'] ;
                                                      }
                                                    t.showInfoSpan(e, info);

                                                  }
                                                }; }}")
        if (sizeBySignificance) {
            var.annot <- contrastDF %>%
                dplyr::select(Group, negLog10P)
            sizeBy <- "negLog10P"
            showSizeLegend <- TRUE
        } else {
            var.annot <- contrastDF %>%
                dplyr::select(Group)
            sizeBy  <- "Group"
            showSizeLegend <- FALSE
        }

        if (!missing(geneNameCol)) {
            gene_data <- DGEobj::getItem(dgeObj, "geneData") %>%
                dplyr::select(dplyr::all_of(geneNameCol))

            var.annot <- merge(var.annot, gene_data, by = 0, all = TRUE, sort = FALSE) %>%
                tibble::column_to_rownames(var = "Row.names") %>%
                dplyr::rename(GeneName = all_of(geneNameCol))
        }

        showLoessFit <- FALSE
        afterRender  <- NULL
        if (!is.null(lineFitType)) {
            lineFitType  <- .cxSupportedLineFit(lineFitType)
            lineFitColor <- .rgbaConversion("goldenrod1", alpha = 0.5)

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
                          colors           = ssc$symbolColor,
                          legendPosition   = "right",
                          showDecorations  = TRUE,
                          showLoessFit     = showLoessFit,
                          fitLineColor     = lineFitColor,
                          sizeByShowLegend = showSizeLegend,
                          title            = title,
                          xAxisTitle       = xlab,
                          yAxisTitle       = ylab,
                          sizeBy           = sizeBy,
                          events           = events,
                          afterRender      = afterRender)

        if (sizeBy == "Group") {
            cx_params <- c(cx_params, list(sizes = c(10, 10, 4)))
        }
        do.call(canvasXpress::canvasXpress, cx_params)
    } else {

        group <- c( "Increased", "No Change", "Decreased")
        symbolColor <- c( "red3", "grey25", "deepskyblue4")

        ssc <- data.frame(group, symbolColor, row.names = NULL) %>%
            dplyr::filter(group %in% unique(contrastDF$Group))

        profilePlot <- ggplot(contrastDF, aes_string(x = xlabel, y = ylabel)) +
            aes(shape = Group,
                color = Group,
                fill  = Group) +
            # Scale lines tell it to use the actual values, not treat them as factors
            scale_shape_manual(name = "Group", guide = "legend", labels = ssc$group,
                               values = rep("circle", 3)) +
            scale_color_manual(name = "Group", guide = "legend", labels = ssc$group,
                               values = ssc$symbolColor) +
            scale_fill_manual(name = "Group", guide = "legend", labels = ssc$group,
                              values = ssc$symbolColor) +
            geom_point(alpha = 0.5)
        # Optional Decorations
        if (sizeBySignificance) {
            profilePlot <- profilePlot + aes(size = negLog10P) +
                scale_size_continuous()
        } else {
            profilePlot <- profilePlot + aes(size = Group) +
                scale_size_manual(name = "Group", guide = "legend", labels = ssc$group,
                                  values = c(4, 4, 2))
        }

        if (!is.null(referenceLine)) {
            profilePlot <- profilePlot +
                geom_hline(yintercept = 0,
                           color = referenceLine,
                           size = 1,
                           alpha = 0.5)
        }

        if (!is.null(foldChangeThreshold)) {
            profilePlot <- profilePlot +
                geom_hline(yintercept = log2(foldChangeThreshold),
                           color = symbolColor[2],
                           size = 1,
                           alpha = 0.5) +
                geom_hline(yintercept = -log2(foldChangeThreshold),
                           color = symbolColor[1],
                           size = 1,
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
                            size = 1,
                            color = "goldenrod1",
                            alpha = 0.5,
                            se = FALSE,
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

        profilePlot + theme(legend.position = "right")
    }
}
