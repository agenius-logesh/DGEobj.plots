#' Create formatted scatterplot of a common column from 2 contrasts
#'
#' Creates a nicely formatted scatterplot of any common column from any 2 topTable dataframes(defaults to logFC).
#'  Additionally, if p-values or FDR values and a threshold are supplied,
#' the plot is color coded to show X unique, Y unique, and
#' common differentially expressed (DE) genes in different colors.
#'
#' Other options add an identity line (slope = 1, intercept = 0) and/or a crosshair at (0, 0).
#'
#' \strong{Data Structure for the input dataframe:}
#'
#' The x and y values should a common column from two chosen contrasts. By default, the
#' contrast names will be used for the axis labels. The x and y labels can be changed
#' using the xlab and ylab arguments.
#'
#' Optionally, significance measures in the form of p-values or FDR values can be supplied
#' for X and Y respectively. If provided, these columns \strong{must} be named "xp" and "yp" respectively.
#' Together with a threshold (which defaults to 0.01), these values will
#' be used to color code the plot to show X Unique, Y Unique, and Common DE
#' genes.  Use either p-values or FDR values for the significance columns. Use the
#' pThreshold argument to set a proper threshold for FDR values.
#'
#'
#' @param dgeObj A DGeobj with atleast 2 contrasts
#' @param contrasts The itemNames of a pair of two contrasts in DGEobj that has logFC column.
#'        Optionally add xp and yp columns to hold p-values or FDR values using colorBySigMeasure.
#' @param colorBySigMeasure Colors points by significance measures.  (default = TRUE)
#' @param pvalCol Name of the p-value column (default = "P.Value")
#' @param valueCol Column name of the data for plot (default = "logFC")
#' @param plotType Plot type must be canvasXpress or ggplot (default = canvasXpress).
#' @param xlab X-axis label (default = first column name)
#' @param ylab Y-axis label (default = second column name)
#' @param title Plot title (Optional)
#' @param pThreshold Significant value threshold (default = 0.01)
#' @param referenceLine Color for a slope=1, intercept=0 reference line
#'        (default = "darkgoldenrod1"; NULL disables)
#'
#' @return canvasXpress or ggplot object based on plotType selection
#'
#' @examples
#' \dontrun{
#'   # Retrieve the first two contrasts from a DGEobj as a list of dataframes (length = 2; named items)
#'   contrasts <- names(DGEobj::getType(dgeObj, "topTable"))[1:2]
#'   contrastList <- lapply(contrasts, function(x){
#'      DGEobj::getItems(dgeObj, x)
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
#'                         referenceLine = "blue")
#' }
#'
#' @import ggplot2
#' @importFrom dplyr mutate arrange filter select rename_with summarise across everything
#' @importFrom assertthat assert_that
#' @importFrom canvasXpress canvasXpress
#' @importFrom magrittr "%>%" set_rownames multiply_by
#' @importFrom DGEobj getItems
#'
#' @export
comparePlot <- function(dgeObj,
                        contrasts,
                        colorBySigMeasure = TRUE,
                        pvalCol = "P.Value",
                        valueCol = "logFC",
                        plotType = "canvasXpress",
                        pThreshold = 0.01,
                        xlab = NULL,
                        ylab = NULL,
                        title = NULL,
                        referenceLine = "darkgoldenrod1") {

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

    assertthat::assert_that(!is.null(valueCol),
                            length(valueCol) == 1,
                            (valueCol %in% colnames(contrastList[[1]])) && (valueCol %in% colnames(contrastList[[2]])),
                            msg = "valueCol to be a singular value of class character and must be present in both the contrasts.")

    assertthat::assert_that(!is.null(pvalCol),
                            length(pvalCol) == 1,
                            (pvalCol %in% colnames(contrastList[[1]])) && (pvalCol %in% colnames(contrastList[[2]])),
                            msg = "pvalCol to be a singular value of class character and must be present in both the contrasts.")

    if (any(is.null(colorBySigMeasure),
            !is.logical(colorBySigMeasure),
            length(colorBySigMeasure) != 1)) {
        warning("colorBySigMeasure must be a singular logical value. Assigning default value TRUE")
        colorBySigMeasure <- TRUE
    }

    if (colorBySigMeasure) {
        compareDF <- comparePrep(contrastList,
                                 valueCol = valueCol,
                                 significanceCol = pvalCol)
    } else {
        compareDF <- comparePrep(contrastList,
                                 valueCol = valueCol,
                                 significanceCol = pvalCol)[,1:2]
    }

    plotType <- tolower(plotType)
    if (any(is.null(plotType),
            !is.character(plotType),
            length(plotType) != 1,
            !tolower(plotType) %in% c("canvasxpress", "ggplot"))) {
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

    if (!is.null(referenceLine) &&
        !all(is.character(referenceLine), length(referenceLine) == 1)) {
        warning("referenceLine must be a singular value of class character or 'NULL' to disable. Assigning default value 'darkgoldenrod1'.")
        referenceLine <- "darkgoldenrod1"
    } else if (.rgbaConversion(referenceLine) == "invalid value") {
        warning("Color specified is not valid. Assigning default value 'darkgoldenrod1'.")
        referenceLine <- "darkgoldenrod1"
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
        colorBySigMeasure  <- TRUE
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
        # adding alpha to colors
        symbolColor <- c("darkgoldenrod1", "grey25", "deepskyblue4", "red3")
        symbolColor <- sapply(symbolColor, .rgbaConversion, alpha = 0.5, USE.NAMES = FALSE)
        decorations <- list()

        decorations <- .getCxPlotDecorations(decorations,
                                                 color = .rgbaConversion("grey50", alpha = 0.5),
                                                 width = 1,
                                                 x     = 0)
        decorations <- .getCxPlotDecorations(decorations,
                                             color = .rgbaConversion("grey50", alpha = 0.5),
                                             width = 1,
                                             y     = 0)

        if (!is.null(referenceLine)) {
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                 color       = .rgbaConversion(referenceLine, alpha = 0.5),
                                                 width       = 1,
                                                 x           = ceiling(y_range$max),
                                                 y           = floor(y_range$min))
        }
        cx.data <- compareDF %>%
            dplyr::select(c(xlabel, ylabel)) %>%
            dplyr::rename_with(~ c(xlab, ylab))
        if (colorBySigMeasure) {
            cx.data   <- round(cx.data, digits = 2)
            var.annot <- compareDF %>%
                dplyr::select(group) %>%
                dplyr::rename_with(~ c("Group"))
            colorBy <- "Group"
            sizeBy  <- "Group"
            colors  <- symbolColor
            sizes   <- c(7,4,7,7)
            shapes  <- rep("circle",4)
        } else {
            cx.data   <- round(compareDF %>% dplyr::select(c(xlabel, ylabel)), digits = 2)
            var.annot <- NULL
            colorBy   <- NULL
            sizeBy    <- NULL
            colors    <- "deepskyblue4"
            sizes     <- 7
            shapes    <- "circle"
        }
        canvasXpress::canvasXpress(data             = cx.data,
                                   varAnnot         = var.annot,
                                   decorations      = decorations,
                                   graphType        = "Scatter2D",
                                   colorBy          = colorBy,
                                   colors           = colors,
                                   shapes           = shapes,
                                   legendPosition   = "right",
                                   scatterAxesEqual = TRUE,
                                   showDecorations  = TRUE,
                                   sizeBy           = sizeBy,
                                   sizes            = sizes,
                                   sizeByShowLegend = FALSE,
                                   title            = title,
                                   xAxisTitle       = xlab,
                                   yAxisTitle       = ylab)
    } else {
        ssc <- data.frame(group = factor(x = levels, levels = levels),
                          symbolShape = rep("circle",4),
                          symbolSize  = c(7,7,7,4),
                          symbolColor = c("darkgoldenrod1", "deepskyblue4", "red3", "grey25"),
                          symbolFill  = c("darkgoldenrod1", "deepskyblue4", "red3", "grey25"))
        # Used to set uniform square scale
        scalemax = compareDF[,1:2] %>% as.matrix %>% abs %>% max %>% magrittr::multiply_by(1.05)
        if (!colorBySigMeasure) {
            compPlot <- compareDF %>%
                ggplot(aes_string(x = xlabel, y = ylabel)) +
                geom_point(shape = 21,
                           size  = ssc$symbolSize[ssc$group == "X Unique"],
                           color = ssc$symbolFill[ssc$group == "X Unique"],
                           fill  = ssc$symbolFill[ssc$group == "X Unique"],
                           alpha = 0.5) +
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
                geom_point(alpha = 0.5) +
                # Make it square with same axis scales
                coord_equal(xlim = c(-scalemax, scalemax), ylim = c(-scalemax, scalemax)) +
                # Box around the legend
                theme(legend.background = element_rect(fill = "gray95", size = .5, linetype = "dotted"))
        }

        compPlot <- compPlot +
            geom_hline(yintercept = 0,
                       color = "grey50",
                       size = 1,
                       alpha = 0.5) +
            geom_vline(xintercept = 0,
                       color = "grey50",
                       size = 1,
                       alpha = 0.5)

        if (!is.null(referenceLine)) {
            compPlot <- compPlot +
                geom_abline(slope = 1,
                            intercept = 0,
                            color = referenceLine,
                            size = 1,
                            alpha = 0.5)
        }

        compPlot + theme(legend.position = "right") +
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
#' @param contrastList A named list of 2 topTable dataframes (required). The
#'   names are used as column names for the value columns in the output.
#' @param valueCol Name of column containing values to plot (default = "logFC")
#' @param significanceCol Name of column to use for significance (default = "P.Value")
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
