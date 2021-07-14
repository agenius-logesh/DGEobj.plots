#' Create deluxe CDF Plots
#'
#' CDF plots are a good complement to p-value histograms as a way to evaluate
#' model performance and examine support for differential expression. Results
#' are ranked by p-value on the x-axis and the p-value plotted on the y-axis.
#' Since p-value distributions should be flat, this type of plot should produce a
#' straight line.  Any observations that fail to meet the null hypothesis will
#' appear as a break in the line at the low end of the curve.
#'
#' This function is designed to take topTable dataframes and display the
#' corresponding CDF plot. Data for the p-values below 0.1 (configurable via
#' pvalMax argument) are shown in a full size plot. An inset figure shows the
#' whole p-value scale. Points below 0.01 are a different color by default
#' (threshold set by pThreshold argument; shape/color attributes customizable
#' through other arguments).
#'
#' \strong{Data Structure for the input dataframe:}
#'
#' The defaults are set for dataframes produced by topTable.  The column
#' "P.Value" is used by default to accommodate the column names used in topTable
#' dataframes.  Any other dataframe can be used with by explicitly defining the
#' p-value column name with the appropriate argument.
#'
#' Sensible defaults are chosen for symbols (Size, Shape and Color).
#' There are optional arguments that allow these to be adjusted. A length of 2
#' is required for these arguments which applies the attributes in
#' this order: Significant, Not Significant.
#'
#' @param DGEdata Name of DGEobj with a class of DGEobj.
#' @param contrast A character vector of a topTable data in DGEobj and its a class of dataframe
#'        with LogRatio and LogIntensity columns and optionally a p-value or FDR column.
#' @param plotType Plot type must be canvasXpress or ggplot (default = canvasXpress).
#' @param pvalCol Name of the p-value or FDR column (default = "P.Value")
#' @param pvalMax Limit the range of the main plot (default = 0.10)
#' @param pThreshold Used to color points (default = 0.01)
#' @param xlab X axis label (default = "Rank")
#' @param ylab Y axis label (default = p-value column name)
#' @param title Plot title (Optional)
#' @param insetTitle Title for the inset plot (Optional)
#' @param referenceLine Color for an horizontal line drawn at the p-threshold
#'   (default = NULL; NULL disables, set to desired color to enable)
#' @param viewportX x-location for the inset plot(default = 0.15)
#' @param viewportY y-location for the inset plot(default = 0.85)
#' @param viewportWidth width of the inset plot (default = 0.35)
#'
#' @return A list containing main plot, inset plot for both plotType. For plotType ="ggplot" list contains a combined plot which
#' displays the inset plot in a viewport
#'
#' @examples
#' \dontrun{
#'    # Plot to console (DGEdata is a name of DGEonj and
#'    contrast is a dataframe from DGEobj)
#'    cdfPlot(DGEdata, contrast, title = "My CDF Plot")
#' }
#' @import ggplot2 magrittr
#' @importFrom dplyr arrange mutate case_when select filter
#' @importFrom assertthat assert_that
#' @importFrom canvasXpress canvasXpress
#'
#' @export
cdfPlot <- function(DGEdata,
                    contrast,
                    plotType       = "canvasXpress",
                    pvalCol        = "P.Value",
                    pThreshold     = 0.01,
                    xlab,
                    ylab,
                    title          = NULL,
                    insetTitle     = NULL,
                    referenceLine  = NULL,
                    viewportX      = 0.15,
                    viewportY      = 0.85,
                    viewportWidth  = 0.35,
                    pvalMax        = 0.1) {

    assertthat::assert_that(!missing(DGEdata),
                            !is.null(DGEdata),
                            "DGEobj" %in% class(DGEdata),
                            msg = "DGEdata must be specified as class of DGEobj.")

    assertthat::assert_that(!missing(contrast),
                            !is.null(contrast),
                            contrast %in% names(DGEobj::getType(DGEdata, type = "topTable")),
                            msg = "contrast to be a singular value of class character and must be one from DGEdata with LogIntensity and LogRatio columns and optionally a p-value.")

    contrastDF <- DGEobj::getItems(DGEdata, contrast)

    assertthat::assert_that(!is.null(pvalCol),
                            pvalCol %in% colnames(contrastDF),
                            msg = "pvalCol column not found in contrast data.")

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
        warning("pthreshold must be a singular numeric value. Assigning default value 0.01.")
        pThreshold <- 0.01
    }

    if (!is.null(title) &&
        !all(is.character(title),
             length(title) == 1)) {
        warning("title must be a singular value of class character. Assigning default value NULL.")
        title <- NULL
    }

    if (!is.null(insetTitle) &&
        !all(is.character(insetTitle),
             length(insetTitle) == 1)) {
        warning("insetTitle must be a singular value of class character. Assigning default value NULL.")
        insetTitle <- NULL
    }

    if (missing(xlab)) {
        xlab <- "Rank"
    } else {
        if (!is.null(xlab) &&
            !all(is.character(xlab),
            length(xlab) == 1)) {
            warning("xlab must be a singular value of class character. Assigning default value 'Rank' as the label.")
            xlab <- "Rank"
        }
    }

    if (missing(ylab)) {
        ylab <- pvalCol
    } else {
        if (!is.null(ylab) &&
            !all(is.character(ylab),
            length(ylab) == 1)) {
            warning("ylab must be a singular value of class character. Assigning default value 'pvalCol' as the label.")
            ylab <- pvalCol
        }
    }

    if (!is.null(referenceLine) &&
        !all(is.character(referenceLine),
             length(referenceLine) == 1)) {
        warning("referenceLine must be a singular value of class character or NULL to disable. Assigning default value NULL.")
        referenceLine <- NULL
    } else if (.rgbaConversion(referenceLine) == "invalid value") {
        warning("Color specified is not valid. Assigning default value NULL.")
        referenceLine <- NULL
    }

    if ((plotType == 'ggplot') &&
        (any(is.null(viewportX),
            !is.numeric(viewportX),
            length(viewportX) != 1))) {
        warning("viewportX must be a singular value of class numeric and must be greater than 0. Assigning default value 0.15.")
        viewportX <- 0.15
    }

    if ((plotType == 'ggplot') &&
        (any(is.null(viewportY),
            !is.numeric(viewportY),
            length(viewportY) != 1))) {
        warning("viewportY must be a singular value of class numeric and must be greater than 0. Assigning default value 0.85.")
        viewportY <- 0.85
    }

    if ((plotType == 'ggplot') &&
        (any(is.null(viewportWidth),
            !is.numeric(viewportWidth),
            length(viewportWidth) != 1,
            viewportWidth < 0))) {
        warning("viewportWidth must be a singular value of class numeric. Assigning default value 0.35.")
        viewportWidth <- 0.35
    }

    if (any(is.null(pvalMax),
            !is.numeric(pvalMax),
            length(pvalMax) != 1)) {
        warning("pvalMax must be a singular numeric value. Assigning default value 0.1.")
        pvalMax <- 0.1
    }

    # Storing column names in x and y variable
    x <- "Rank"
    y <- pvalCol

    if (is.null(title)) {
        title = ""
    }

    if (is.null(insetTitle)) {
        insetTitle = ""
    }

    # Combo PLOT: full data inset, most significant data in main plot
    # Rank by p-value
    contrastDF <- contrastDF %>%
        dplyr::arrange(!!sym(pvalCol))
    contrastDF$Rank <- c(1:nrow(contrastDF))

    # Let"s plot the p-value subsets
    contrastDF$group <- NA
    contrastDF$order <- NA
    contrastDF <- contrastDF %>%
        dplyr::mutate(group = dplyr::case_when(!!rlang::sym(pvalCol) <= pThreshold ~ "Significant",
                                               TRUE ~ "Not Significant"),
                      group = factor(group,
                                     levels = c("Significant", "Not Significant")))

    contrastDF_subset <- contrastDF %>%
        dplyr::filter(!!rlang::sym(pvalCol) <= pvalMax)
    cdfMain <- NULL
    cdfInset <- NULL

    if (plotType == "canvasxpress") {
        ## Create the canvasXpress cx.data and var.annot
        # Main plot
        cx.data <- contrastDF %>% dplyr::select(!!x,!!y)
        colnames(cx.data) <- c(x, y)
        var.annot <- contrastDF %>% dplyr::select(group)

        # Inset plot
        cx.data.subset <- contrastDF_subset %>% dplyr::select(!!x,!!y)
        colnames(cx.data.subset) <- c(x, y)
        var.annot.subset <- contrastDF_subset %>% dplyr::select(group)

        decorations <- list()
        if (!is.null(referenceLine)) {
            referenceLine <- .rgbaConversion(referenceLine, alpha = 0.7)
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                color = referenceLine,
                                                width = 3,
                                                y     = pThreshold)
        }

        max.value <- max(pThreshold, max(contrastDF_subset[[y]]))
        maxY <- max.value + max.value*0.1

        cdfMain <- canvasXpress::canvasXpress(data              = cx.data.subset,
                                              varAnnot          = var.annot.subset,
                                              decorations       = decorations,
                                              graphType         = "Scatter2D",
                                              colorBy           = "group",
                                              colors            = c("red3", "deepskyblue4"),
                                              shapeBy           = "group",
                                              shapes            = rep("circle",2),
                                              shapeByShowLegend = FALSE,
                                              sizeBy            = "group",
                                              sizes             = c(4,3),
                                              sizeByShowLegend  = FALSE,
                                              title             = title,
                                              xAxisTitle        = xlab,
                                              yAxisTitle        = ylab,
                                              setMaxY           = maxY)

        cdfInset <- canvasXpress::canvasXpress(data              = cx.data,
                                               varAnnot          = var.annot,
                                               decorations       = decorations,
                                               graphType         = "Scatter2D",
                                               colorBy           = "group",
                                               colors            = c("red3", "deepskyblue4"),
                                               shapeBy           = "group",
                                               shapes            = rep("circle",2),
                                               shapeByShowLegend = FALSE,
                                               sizeBy            = "group",
                                               sizes             = c(4,3),
                                               sizeByShowLegend  = FALSE,
                                               title             = insetTitle,
                                               xAxisTitle        = xlab,
                                               yAxisTitle        = ylab,
                                               setMaxY           = max(contrastDF[[y]]))
        cdfPlot <- list("main" = cdfMain, "inset" = cdfInset)
    } else {
        groupNames <- c("Not Significant", "Significant")

        symbolSize     = c(4, 3)
        symbolShape    = c("circle", "circle")
        symbolColor    = c("red3", "deepskyblue4")

        names(symbolShape) <- groupNames
        names(symbolSize)  <- groupNames
        names(symbolColor) <- groupNames

        # Plot subset percent of the data for the main plot
        cdfMain <- ggplot(contrastDF_subset, aes_string(x = x, y = y)) +
            aes(shape = group, size = group, color = group, fill = group) +
            # Scale lines tell it to use the actual values, not treat them as factors
            scale_shape_manual(values = symbolShape) +
            scale_size_manual( values =  symbolSize) +
            scale_color_manual(values = symbolColor,  aesthetics = c("colour", "fill")) +
            geom_point(alpha = 0.7)

        # Optional Decorations
        if (!is.null(referenceLine)) {
            cdfMain <- cdfMain +
                geom_hline(yintercept = pThreshold, color = referenceLine,
                           size = 3, alpha = 0.5)
        }

        # Add Labels
        cdfMain <- cdfMain +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(title)

        # Set up the inset plot with All Data
        cdfInset <- ggplot(contrastDF, aes_string(x = x, y = y)) +
            aes(shape = group, size = group, color = group, fill = group) +
            # Scale lines tell it to use the actual values, not treat them as factors
            scale_shape_manual(values = symbolShape) +
            scale_size_manual( values = symbolSize) +
            scale_color_manual(values = symbolColor, aesthetics = c("colour", "fill")) +
            geom_rect(xmin = 0, xmax = nrow(contrastDF),
                      ymin = 0, ymax = max(contrastDF[[y]]), color = "lightblue",
                      fill = "lightblue", alpha = 0.2) +
            geom_point(alpha = 0.7)

        #remove the legends for the inset plot
        cdfInset <- cdfInset + theme(legend.position = "none")

        # Add Labels and title
        cdfInset <- cdfInset +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(insetTitle)

        plot_limits <- get_plot_limits(cdfMain, viewportX, viewportY, viewportWidth)
        vp_plot <- cdfMain +
            annotation_custom(grob =  ggplotGrob(cdfInset),
                              ymin = plot_limits[["ymin"]],
                              ymax = plot_limits[["ymax"]],
                              xmin = plot_limits[["xmin"]],
                              xmax = plot_limits[["xmax"]])

        cdfPlot <- list(main = cdfMain, inset = cdfInset, combined = vp_plot)
    }

    cdfPlot
}

get_plot_limits <- function(main_plot, viewportX, viewportY, viewportWidth) {
    main_plot_build <- ggplot_build(main_plot)
    xrange <- main_plot_build$layout$panel_params[[1]]$x.range
    yrange <- main_plot_build$layout$panel_params[[1]]$y.range

    x_range_val <- xrange[[2]] - xrange[[1]]
    xmin <- xrange[[1]] + (0.02 * x_range_val)
    xmax <- xmin + (viewportWidth * x_range_val)

    y_range_val <- yrange[[2]] - yrange[[1]]
    ymin <- yrange[[2]] - (0.02 * y_range_val)
    ymax <- ymin - (viewportWidth * y_range_val)
    list("xmin" = xmin,
         "xmax" = xmax,
         "ymin" = ymin,
         "ymax" = ymax)
}
