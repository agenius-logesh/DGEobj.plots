#' Create formatted scatterplot of first two cols of data.frame
#'
#' Creates a nicely formatted scatterplot of the
#' first two columns in a dataframe.  Several formatting options are
#' available to enhance the plots.  If p-values or FDR values and a threshold are supplied,
#' the plot is color coded to show X unique, Y unique, and
#' common differentially expressed (DE) genes in different colors.
#'
#' Other options add an identity line (slope = 1, intercept = 0) and/or a crosshair at (0, 0).
#'
#' \strong{Data Structure for the input dataframe:}
#'
#' The x and y values should be in the first two columns. By default, their
#' column names will be used for the axis labels. The x and y labels can be changed
#' using the xlab and ylab arguments.
#'
#' Optionally, significance measures in the form of p-values or FDR values can be supplied
#' for X and Y respectively. If provided, these columns \strong{must} be named "xp" and "yp" respectively.
#' Together with a threshold (which defaults to 0.01), these values will
#' be used to color code the plot to show X Unique, Y Unique, and Common DE
#' genes.  Use either p-values or FDR values for the significance columns. Use the
#' pThreshold argument to set a proper threshold for FDR values.
#'
#' Sensible defaults are chosen for symbols (Size, Shape, Color and Fill), but optional
#' arguments allow much about the symbols to be customized. A length of 4 is
#' required for these arguments which applies the attributes in this order:
#' Common, X Unique, Y Unique, and Not Significant.
#'
#' Note: if p-values or FDR values are not used to color the plot, the X Unique color
#' values are used.
#'
#' @param dgeObj DGEobj with a class of DGEobj.
#' @param contrasts A Two character vector Name of a topTable item in DGEobj and its a class of dataframe
#'        with logFC and P.Value. These two contrasts representing the x and y variables.
#'        Optionally add xp and yp columns to hold p-values or FDR values.
#' @param plotType Plot type must be canvasXpress or ggplot (default = canvasXpress).
#' @param xlab X-axis label (default = first column name)
#' @param ylab Y-axis label (default = second column name)
#' @param title Plot title (Optional)
#' @param pThreshold Used to color points (default = 0.01)
#' @param symbolSize Size of symbols (default = c(7, 7, 7, 3))
#' @param symbolShape Shape of the symbols (default = c("circle", "circle", "circle", "circle")).
#'        For ggplot reference see \url{http://www.cookbook-r.com/Graphs/Shapes_and_line_types}
#' @param symbolColor c(Common, xUnique, yUnique, NoChange) symbols colors (default = c("darkgoldenrod1", "deepskyblue4", "red3", "grey25"))
#' @param transparency Controls the transparency of the plotted points (0-1; default = 0.5)
#' @param crosshair Color for the crosshair (default = "grey50">, NULL disables)
#'        See \url{http://research.stowers-institute.org/efg/R/Color/Chart}
#' @param referenceLine Color for a slope=1, intercept=0 reference line
#'        (default = "darkgoldenrod1"; NULL disables)
#' @param refLineThickness Set thickness for crosshair and referenceLine (default = 1)
#' @param legendPosition One of "top", "bottom", "left", "right", "ne", "se", "nw", "sw", NULL.
#'        top/bottom/left/right place the legend outside the figure.  ne/se/nw/sw place the figure
#'        inside the figure. NULL disables the legend (default = "right")
#' @param footnote Optional string placed right justified at bottom of plot.
#' @param footnoteSize Applies to footnote (default = 3)
#' @param footnoteColor Applies to footnote (default = "black")
#'
#' @return canvasXpress or ggplot object based on plotType selection
#'
#' @examples
#' \dontrun{
#'   # Retrieve the first two contrasts from a DGEobj as a list of dataframes (length = 2; named items)
#'   contrasts <- names(DGEobj::getType(dgeObj, "topTable"))[1:2]
#'   contrastList <- lapply(contrasts, function(x){
#'      getItems(dgeObj, x)
#'    })
#'    names(contrastList) <- contrasts
#'
#'   # Capture the default logFC and P.Value
#'   compareDat <- comparePrep(contrastList)
#'
#'   # Switch to an FDR value for the significance measure
#'   compareDat <- comparePrep(contrastList, significanceCol = "adj.P.Val")
#'
#'   # Draw the plot
#'   cPlot <- comparePlot(dgeObj, contrasts, title = "Plot Title")
#'   print(cPlot)
#'
#'   # Deluxe Plot with bells and whistles.
#'   myPlot <- comparePlot(dgeObj,
#'                         contrasts,
#'                         pThreshold = 0.5,
#'                         xlab = "x Axis Label",
#'                         ylab = "y Axis Label",
#'                         title = "Plot Title",
#'                         crosshair = "red",
#'                         referenceLine = "blue",
#'                         legendPosition = "right")
#'
#'                         myPlot <- comparePlot(dgeObj,
#'                         contrasts,
#'                         pThreshold = 0.5,
#'                         xlab = "x Axis Label",
#'                         ylab = "y Axis Label",
#'                         title = "Plot Title",
#'                         crosshair = "red",
#'                         referenceLine = "blue",
#'                         legendPosition = "right",
#'                         plotType = "ggplot")
#' }
#'
#' @import ggplot2
#' @importFrom dplyr mutate arrange filter select rename_with summarise across everything
#' @importFrom assertthat assert_that
#' @importFrom canvasXpress canvasXpress
#' @importFrom magrittr set_rownames multiply_by
#'
#' @export
comparePlot <- function(dgeObj,
                        contrasts,
                        sigMeasurePlot = TRUE,
                        plotType = "canvasXpress",
                        pThreshold = 0.01,
                        xlab = NULL,
                        ylab = NULL,
                        title = NULL,
                        symbolSize = c(7, 7, 7, 3),
                        symbolShape = c("circle", "circle", "circle", "circle"),
                        symbolColor = c("darkgoldenrod1", "deepskyblue4", "red3", "grey25"),
                        transparency = 0.5,
                        crosshair = "grey50",
                        referenceLine = "darkgoldenrod1",
                        refLineThickness = 1,
                        legendPosition = "right",
                        footnote,
                        footnoteSize = 3,
                        footnoteColor = "black") {

    ##### Asserts
    assertthat::assert_that(!missing(dgeObj),
                            !is.null(dgeObj),
                            "DGEobj" %in% class(dgeObj),
                            msg = "dgeObj must be specified and must belong to DGEobj class.")

    assertthat::assert_that(!missing(contrasts),
                            !is.null(contrasts),
                            length(contrasts) == 2,
                            is.character(contrasts),
                            all(contrasts %in% names(DGEobj::getType(dgeObj, type = "topTable"))),
                            msg = "contrasts must be a class of character and must be two of the top tables in the dgeObj. with logFC and P.value columns.")

    contrastList <- lapply(contrasts, function(x){
        getItems(dgeObj, x)
    })
    names(contrastList) <- contrasts

    if (any(is.null(sigMeasurePlot),
            !is.logical(sigMeasurePlot),
            length(sigMeasurePlot) != 1)) {
        warning("sigMeasurePlot must be a singular logical value. Assigning default value TRUE")
        sigMeasurePlot <- TRUE
    }

    if (sigMeasurePlot) {
        compareDF <- comparePrep(contrastList)
    } else {
        compareDF <- comparePrep(contrastList)[,1:2]
    }

    plotType <- tolower(plotType)
    if (any(is.null(plotType),
            !is.character(plotType),
            length(plotType) != 1,
            !plotType %in% c("canvasxpress", "ggplot"))) {
        warning("plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'.")
        plotType <- "canvasxpress"
    }

    if (any(is.null(pThreshold),
            !is.numeric(pThreshold),
            length(pThreshold) != 1)) {
        warning("pThreshold must be a singular value of class numeric. Assigning default value '0.01'.")
        pThreshold <- 0.01
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

    if (any(is.null(symbolColor),
            !is.character(symbolColor),
            length(symbolColor)  != 4,
            length(.validate_colors(symbolColor)) != 4)) {
        warning("symbolColor must be a vector of 4 character values. Assigning default values 'darkgoldenrod1', 'deepskyblue4', 'red3', 'grey25'.")
        symbolColor <- c("darkgoldenrod1", "deepskyblue4", "red3", "grey25")
    }

    if (any(is.null(symbolSize),
            !is.numeric(symbolSize),
            length(symbolSize)  != 4)) {
        warning("symbolSize must be a vector of 4 integer values. Assigning default values 7, 7, 7, 3.")
        symbolSize  <-  c(7, 7, 7, 3)
    }

    if (any(is.null(symbolShape),
            !is.character(symbolShape),
            length(symbolShape)  != 4,
            plotType == "canvasxpress" && !is.null(symbolShape) && length(.validate_cx_shapes(symbolShape)) != 4,
            plotType == "ggplot" && !is.null(symbolShape) && length(.validate_gg_shapes(symbolShape)) != 4)) {
        warning("symbolShape must be a vector of 4 charcter values. Assigning default values 'circle', 'circle', 'circle', 'circle'.")
        symbolShape  <- c("circle", "circle", "circle", "circle")
    }

    if (any(is.null(transparency),
            !is.numeric(transparency),
            length(transparency) != 1,
            transparency <= 0,
            transparency > 1)) {
        warning("transparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.5'.")
        transparency <- 0.5
    }

    if (!is.null(crosshair) &&
        !all(is.character(crosshair), length(crosshair) == 1)) {
        warning("crosshair must be a singular value of class character or 'NULL' to disable. Assigning default value 'grey50'.")
        crosshair <- "grey50"
    } else if (.rgbaConversion(crosshair) == "invalid value") {
        warning("Color specified is not valid. Assigning default value 'grey50'.")
        crosshair <- "grey50"
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
            length(refLineThickness) != 1)) {
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

    if (!is.null(footnote) &&
        !is.null(footnoteSize) &&
        any(!is.numeric(footnoteSize),
            length(footnoteSize) != 1)) {
        warning("footnoteSize must be a singular value of class numeric. Assigning default value '3'.")
        footnoteSize <- 3
    }

    if (!is.null(footnote) &&
        !is.null(footnoteColor) &&
        any(!is.character(footnoteColor),
            length(footnoteColor) != 1)) {
        warning("footnoteColor must be a singular value of class character. Assigning default value 'black'.")
        footnoteColor <- "black"
    }

    levels         <- c("Common", "X Unique", "Y Unique", "Not Significant")
    xlabel         <- make.names(colnames(compareDF)[1])
    ylabel         <- make.names(colnames(compareDF)[2])
    compPlot       <- NULL
    colnames(compareDF)[1:2] <- c(xlabel, ylabel)

    if (is.null(xlab)) {
        xlab <- xlabel
    }

    if (is.null(ylab)) {
        ylab <- ylabel
    }

    if (is.null(title)) {
        title = ""
    }

    if (all(c("xp","yp") %in% colnames(compareDF))) {
        sigMeasurePlot <- TRUE
        compareDF <- compareDF %>%
            dplyr::mutate(group = ifelse(xp <= pThreshold,
                                         ifelse(yp <= pThreshold, "Common", "X Unique"),
                                         ifelse(yp <= pThreshold, "Y Unique", "Not Significant")),
                          group = factor(group,
                                         levels = c("Common", "X Unique", "Y Unique", "Not Significant")))
    }

    y_range <- compareDF %>%
        dplyr::select(ylabel) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(), list(min, max))) %>%
        dplyr::rename_with(~ c("min", "max"))

    if (plotType == "canvasxpress") {
        # adding transparency to colors
        symbolColor <- sapply(symbolColor, .rgbaConversion, alpha = transparency, USE.NAMES = FALSE)
        decorations <- list()
        if (!is.null(crosshair)) {
            decorations <- .getCxPlotDecorations(decorations,
                                                 color = .rgbaConversion(crosshair, alpha = transparency),
                                                 width = refLineThickness,
                                                 x     = 0)
            decorations <- .getCxPlotDecorations(decorations,
                                                 color = .rgbaConversion(crosshair, alpha = transparency),
                                                 width = refLineThickness,
                                                 y     = 0)
        }
        if (!is.null(referenceLine)) {
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                 color       = .rgbaConversion(referenceLine, alpha = transparency),
                                                 width       = refLineThickness,
                                                 x           = ceiling(y_range$max),
                                                 y           = floor(y_range$min))
        }
        cx.data <- compareDF %>%
            dplyr::select(c(xlabel, ylabel)) %>%
            dplyr::rename_with(~ c(xlab, ylab))
        if (sigMeasurePlot) {
            cx.data   <- round(cx.data, digits = 2)
            var.annot <- compareDF %>%
                dplyr::select(group) %>%
                dplyr::rename_with(~ c("Group"))
            colorBy <- "Group"
            sizeBy  <- "Group"
            colors  <- symbolColor[c(1,4,2,3)]
            sizes   <- symbolSize[c(1,4,2,3)]
            shapes  <- symbolShape[c(1,4,2,3)]
        } else {
            cx.data   <- round(compareDF %>% dplyr::select(c(xlabel, ylabel)), digits = 2)
            var.annot <- NULL
            colorBy   <- NULL
            sizeBy    <- NULL
            colors    <- symbolColor[2]
            sizes     <- symbolSize[2]
            shapes    <- symbolShape[2]
        }
        canvasXpress::canvasXpress(data             = cx.data,
                                   varAnnot         = var.annot,
                                   decorations      = decorations,
                                   graphType        = "Scatter2D",
                                   colorBy          = colorBy,
                                   colors           = colors,
                                   shapes           = shapes,
                                   legendPosition   = legendPosition,
                                   scatterAxesEqual = TRUE,
                                   showDecorations  = TRUE,
                                   sizeBy           = sizeBy,
                                   sizes            = sizes,
                                   sizeByShowLegend = FALSE,
                                   title            = title,
                                   xAxisTitle       = xlab,
                                   yAxisTitle       = ylab,
                                   citation         = footnote,
                                   citationFontSize = footnoteSize,
                                   citationColor    = footnoteColor
        )
    } else {
        ssc <- data.frame(group = factor(x = levels, levels = levels),
                          symbolShape = symbolShape,
                          symbolSize  = symbolSize,
                          symbolColor = symbolColor,
                          symbolFill  = symbolColor)
        # Used to set uniform square scale
        scalemax = compareDF[,1:2] %>% as.matrix %>% abs %>% max %>% magrittr::multiply_by(1.05)
        if (!sigMeasurePlot) {
            compPlot <- compareDF %>%
                ggplot(aes_string(x = xlabel, y = ylabel)) +
                geom_point(shape = 21,
                                    size  = ssc$symbolSize[ssc$group == "X Unique"],
                                    color = ssc$symbolFill[ssc$group == "X Unique"],
                                    fill  = ssc$symbolFill[ssc$group == "X Unique"],
                                    alpha = transparency) +
                coord_equal(xlim = c(-scalemax, scalemax), ylim = c(-scalemax, scalemax))
        } else {
            compPlot <- compareDF %>%
                ggplot(aes_string(x = xlabel, y = ylabel)) +
                aes(shape = group, size = group,
                             color = group, fill = group) +
                scale_shape_manual(name = "Group", guide = "legend", labels = ssc$group, values = ssc$symbolShape) +
                scale_size_manual(name = "Group", guide = "legend", labels = ssc$group, values = ssc$symbolSize) +
                scale_color_manual(name = "Group", guide = "legend", labels = ssc$group, values = ssc$symbolColor) +
                scale_fill_manual(name = "Group", guide = "legend", labels = ssc$group, values = ssc$symbolFill) +
                geom_point(alpha = transparency) +
                # Make it square with same axis scales
                coord_equal(xlim = c(-scalemax, scalemax), ylim = c(-scalemax, scalemax)) +
                # Box around the legend
                theme(legend.background = element_rect(fill = "gray95", size = .5, linetype = "dotted"))
        }
        if (!is.null(crosshair)) {
            compPlot <- compPlot +
                geom_hline(yintercept = 0,
                                    color = crosshair,
                                    size = refLineThickness,
                                    alpha = 0.5) +
                geom_vline(xintercept = 0,
                                    color = crosshair,
                                    size = refLineThickness,
                                    alpha = 0.5)
        }
        if (!is.null(referenceLine)) {
            compPlot <- compPlot +
                geom_abline(slope = 1,
                                     intercept = 0,
                                     color = referenceLine,
                                     size = refLineThickness,
                                     alpha = 0.5)
        }

        compPlot +
            theme(legend.position = "right") +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(title)
    }
}


#' Create a data.frame to use with comparePlot from topTable data.frames
#'
#' Takes two topTable dataframes and outputs a dataframe suitable for function
#' comparePlot() (2 columns of LogRatio data and 2 columns of significant
#' measures). Filter the two topTable to contain only the intersecting genes
#' (present in both datasets). The two dataframes must have the same type of
#' gene IDs as rownames.
#'
#' @param contrastList A named list of 2 topTable dataframes (Required). The
#'   names are used as column names for the value columns in the output.
#' @param valueCol Name of column containing values to plot (Default = "logFC")
#' @param significanceCol Name of column to use for significance (Default = "P.Value")
#'
#' @return A data frame with 2 LogRatio measurements and 2 significance columns.  Columns 1 and 3 map
#' to sample 1 and columns 2 and 4 map to sample 2.  The returned dataframe is formatted as expected
#' by the comparePlot function.
#'
#' @examples
#' \dontrun{
#'   # Retrieve the 1st two contrasts from a DGEobj
#'   contrastList <- getType(dgeObj, "topTable")[1:2]
#'
#'   # Capture the default logFC and P.Value
#'   compareDat <- comparePrep(contrastList)
#'
#'   # Switch to an FDR value for the significance measure
#'   compareDat <- comparePrep(contrastList, significanceCol="adj.P.Val")
#'
#'   # Draw the plot
#'   cPlot <- comparePlot(compareDat)
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter arrange bind_cols rename_with
#' @importFrom magrittr set_rownames
#'
#' @export
comparePrep <- function(contrastList,
                        valueCol = "logFC",
                        significanceCol = "P.Value"){
    assertthat::assert_that(length(contrastList) == 2,
                            !is.null(names(contrastList)),
                            "data.frame" %in% class(contrastList[[1]]),
                            "data.frame" %in% class(contrastList[[2]]),
                            msg = "contrastList must be a named list of length 2 where both items are of class 'data.frame'.")
    assertthat::assert_that(valueCol %in% colnames(contrastList[[1]]),
                            valueCol %in% colnames(contrastList[[2]]),
                            msg = "The valueCol must be included in the colnames of both items of contrastList.")
    assertthat::assert_that(significanceCol %in% colnames(contrastList[[1]]),
                            significanceCol %in% colnames(contrastList[[2]]),
                            msg = "The significanceCol must be included in the colnames of both items of contrastList.")

    commonIDs <- intersect(rownames(contrastList[[1]]), rownames(contrastList[[2]]))
    assertthat::assert_that(length(commonIDs) > 0,
                            msg = "No common gene IDs were found between the two dataframes in contrastList.")

    # Filter both tables to the same set of genes in the same order
    tt1 <- construct_comparator_data(contrastList[[1]], commonIDs)
    tt2 <- construct_comparator_data(contrastList[[2]], commonIDs)
    assertthat::assert_that(all(tt1$geneid == tt2$geneid),
                            msg = "Gene IDs in the two topTable files in contrastList are not identical.")

    ttNames <- names(contrastList)
    # construct the final comparePlot data
    dplyr::bind_cols(fc1 = tt1[[valueCol]],
                     fc2 = tt2[[valueCol]],
                     xp  = tt1[[significanceCol]],
                     yp  = tt2[[significanceCol]]) %>%
        dplyr::rename_with(~c(ttNames[1], ttNames[2], "xp", "yp")) %>%
        as.data.frame() %>%
        magrittr::set_rownames(tt1$geneid)
}

construct_comparator_data <- function(data, commonIDs) {
    data %>%
        dplyr::mutate(geneid = rownames(.)) %>%
        dplyr::filter(geneid %in% commonIDs) %>%
        dplyr::arrange(geneid)
}
