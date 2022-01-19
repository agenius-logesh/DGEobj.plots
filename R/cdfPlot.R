#' Create deluxe CDF Plots
#'
#' CDF plots are a good complement to p-value histograms as a way to evaluate
#' model performance and examine support for differential expression. On the x-axis,
#' is the ranked by p-value and on y-axis is the p-value.
#' Since p-value distributions should be flat, this type of plot should produce a
#' straight line. Any observations that fail to meet the null hypothesis will
#' have a break in the line at the end of the curve.
#'
#' This function is designed to take a topTable dataframe and display the
#' corresponding CDF plots. The first plot (below_pvalMax) are p-values below the pvalMax and the
#' the second plot (all_pval) shows all the p-values. For plotType ="ggplot", output also
#' contains an additional plot that displays the all_pval plot as an inset in below_pvalMax.
#'
#' @param dgeObj A DGEobj with one or more topTables (required)
#' @param contrast Name of a topTable dataframe with p-value or an FDR column (required)
#' @param plotType Plot type must be canvasXpress or ggplot (default = canvasXpress).
#' @param pvalCol Name of the p-value or FDR column (default = "P.Value")
#' @param pvalMax Limit the range of the main plot (default = 0.10)
#' @param pThreshold Significant value threshold (default = 0.01)
#' @param xlab X axis label (default = "Rank")
#' @param ylab Y axis label (default = p-value column name)
#' @param title Plot title (Optional)
#' @param insetTitle Title for the inset plot (Optional)
#' @param referenceLine Color for a horizontal line drawn at the p-threshold
#'   (default = NULL; NULL disables, set to desired color to enable)
#' @param insetX x-location for the inset plot (default = 0.15)
#' @param insetY y-location for the inset plot (default = 0.85)
#' @param insetWidth width of the inset plot (default = 0.35)
#'
#' @return A list of plots.
#'
#' @examples
#' \dontrun{
#'    dgeObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj", mustWork = TRUE))
#'    # Plot to console (dgeObj is a DGEobj and contrast is a name of toptable dataframe from DGEobj)
#'    contrast <- names(DGEobj::getType(dgeObj, type = "topTable"))
#'
#'    cdfPlot(dgeObj, contrast = contrast[1], title = "My CDF Plot")
#'
#'    cdfPlot(dgeObj, contrast = contrast[1], title = "My CDF Plot", plotType = "ggplot")
#' }
#' @import ggplot2 magrittr
#' @importFrom dplyr arrange mutate case_when select filter
#' @importFrom assertthat assert_that
#' @importFrom canvasXpress canvasXpress
#'
#' @export
cdfPlot <- function(dgeObj,
                    contrast,
                    plotType       = "canvasXpress",
                    pvalCol        = "P.Value",
                    pThreshold     = 0.01,
                    xlab,
                    ylab,
                    title          = NULL,
                    insetTitle     = NULL,
                    referenceLine  = NULL,
                    insetX         = 0.15,
                    insetY         = 0.85,
                    insetWidth     = 0.35,
                    pvalMax        = 0.10) {

    assertthat::assert_that(!missing(dgeObj),
                            !is.null(dgeObj),
                            "DGEobj" %in% class(dgeObj),
                            msg = "dgeObj must be specified and must belong to DGEobj class.")

    assertthat::assert_that(!missing(contrast),
                            !is.null(contrast),
                            length(contrast) == 1,
                            contrast %in% names(DGEobj::getType(dgeObj, type = "topTable")),
                            msg = "contrast must be a singular value of class character and must be one of the top tables in the dgeObj.")

    contrastDF <- DGEobj::getItems(dgeObj, contrast)

    assertthat::assert_that(nrow(contrastDF) > 0,
                            "data.frame" %in% class(contrastDF),
                            msg = "The specified contrast does not have a valid topTable associated with it. Re-run the function with a valid contrast.")

    if (any(is.null(pvalCol),
            !is.character(pvalCol),
            length(pvalCol) != 1,
            !pvalCol %in% colnames(contrastDF))) {
        warning("pvalCol must to be a singular value of class character and must be in contrast data. Assigning default value 'P.Value'.")
        pvalCol <- "P.Value"
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
        (any(is.null(insetX),
            !is.numeric(insetX),
            length(insetX) != 1))) {
        warning("insetX must be a singular value of class numeric and must be greater than 0. Assigning default value 0.15.")
        insetX <- 0.15
    }

    if ((plotType == 'ggplot') &&
        (any(is.null(insetY),
            !is.numeric(insetY),
            length(insetY) != 1))) {
        warning("insetY must be a singular value of class numeric and must be greater than 0. Assigning default value 0.85.")
        insetY <- 0.85
    }

    if ((plotType == 'ggplot') &&
        (any(is.null(insetWidth),
            !is.numeric(insetWidth),
            length(insetWidth) != 1,
            insetWidth < 0))) {
        warning("insetWidth must be a singular value of class numeric. Assigning default value 0.35.")
        insetWidth <- 0.35
    }

    if (any(is.null(pvalMax),
            !is.numeric(pvalMax),
            length(pvalMax) != 1)) {
        warning("pvalMax must be a singular numeric value. Assigning default value 0.1.")
        pvalMax <- 0.1
    }

    groupNames <- c("Not Significant", "Significant")
    symbolShape    = c("circle", "circle")
    symbolColor    = c("red3", "deepskyblue4")
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
        dplyr::arrange(!!rlang::sym(pvalCol))
    contrastDF$Rank <- c(1:nrow(contrastDF))

    # Let's plot the p-value subsets
    contrastDF$group <- NA
    contrastDF$order <- NA
    contrastDF <- contrastDF %>%
        dplyr::mutate(group = dplyr::case_when(!!rlang::sym(pvalCol) <= pThreshold ~ "Significant",
                                               TRUE ~ "Not Significant"),
                      group = factor(group,
                                     levels = c("Significant", "Not Significant")))

    contrastDF_subset <- contrastDF %>%
        dplyr::filter(!!rlang::sym(pvalCol) <= pvalMax)
    cdfpvalMax <- NULL
    cdfAll <- NULL

    if (plotType == "canvasxpress") {
        symbolSize     = c(20, 18)
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
            referenceLine <- .rgbaConversion(referenceLine)
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                color = referenceLine,
                                                width = 1,
                                                y     = pThreshold)
        }

        # Footnote
        max.value <- max(pThreshold, max(contrastDF_subset[[y]]))
        maxY <- max.value + max.value*0.1

        cdfpvalMax <- canvasXpress::canvasXpress(data              = cx.data.subset,
                                                 varAnnot          = var.annot.subset,
                                                 decorations       = decorations,
                                                 graphType         = "Scatter2D",
                                                 colorBy           = "group",
                                                 colorScheme       = "Dark2",
                                                 shapeBy           = "group",
                                                 shapes            = symbolShape,
                                                 shapeByShowLegend = FALSE,
                                                 sizeBy            = "group",
                                                 sizes             = symbolSize,
                                                 sizeByShowLegend  = FALSE,
                                                 title             = title,
                                                 xAxisTitle        = xlab,
                                                 yAxisTitle        = ylab,
                                                 setMaxY           = maxY)

        cdfAll <- canvasXpress::canvasXpress(data              = cx.data,
                                             varAnnot          = var.annot,
                                             graphType         = "Scatter2D",
                                             colorBy           = "group",
                                             colorScheme       = "Dark2",
                                             shapeBy           = "group",
                                             shapes            = symbolShape,
                                             shapeByShowLegend = FALSE,
                                             sizeBy            = "group",
                                             sizes             = symbolSize,
                                             sizeByShowLegend  = FALSE,
                                             title             = insetTitle,
                                             xAxisTitle        = xlab,
                                             yAxisTitle        = ylab,
                                             setMaxY           = max(contrastDF[[y]]))
        cdfPlot <- list("below_pvalMax" = cdfpvalMax, "all_pval" = cdfAll)
    } else {
        symbolSize     = c(4, 3)
        names(symbolShape) <- groupNames
        names(symbolSize)  <- groupNames
        names(symbolColor) <- groupNames

        # Plot subset percent of the data for the main plot
        cdfpvalMax <- ggplot(contrastDF_subset, aes_string(x = x, y = y)) +
            aes(shape = group, size = group, color = group, fill = group) +
            # Scale lines tell it to use the actual values, not treat them as factors
            scale_shape_manual(values = symbolShape) +
            scale_size_manual( values =  symbolSize) +
            scale_color_manual(values = symbolColor,  aesthetics = c("colour", "fill")) +
            geom_point()
        #alpha = transparency
        # Optional Decorations
        if (!is.null(referenceLine)) {
            cdfpvalMax <- cdfpvalMax +
                geom_hline(yintercept = pThreshold, color = referenceLine,alpha = 0.5)
         #   size = refLineThickness,
        }

        # Add Labels
        cdfpvalMax <- cdfpvalMax +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(title)



        # Set up the inset plot with All Data
        cdfAll <- ggplot(contrastDF, aes_string(x = x, y = y)) +
            aes(shape = group, size = group, color = group, fill = group) +
            # Scale lines tell it to use the actual values, not treat them as factors
            scale_shape_manual(values = symbolShape) +
            scale_size_manual( values = symbolSize) +
            scale_color_manual(values = symbolColor, aesthetics = c("colour", "fill")) +
            geom_rect(xmin = 0, xmax = nrow(contrastDF),
                               ymin = 0, ymax = max(contrastDF[[y]]), color = "lightblue",
                               fill = "lightblue", alpha = 0.2) +
            geom_point()
        #alpha = transparency

        #remove the legends for the inset plot
        cdfAll <- cdfAll + theme(legend.position = "none")

        # Add Labels and title
        cdfAll <- cdfAll +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(insetTitle)

        #Plot all_pval as an inset of below_pvalMax
        plot_limits <- get_plot_limits(cdfpvalMax, insetX, insetY, insetWidth)
        inset_plot <- cdfpvalMax +
            annotation_custom(grob =  ggplotGrob(cdfAll),
                                       ymin = plot_limits[["ymin"]],
                                       ymax = plot_limits[["ymax"]],
                                       xmin = plot_limits[["xmin"]],
                                       xmax = plot_limits[["xmax"]])

        cdfPlot <- list(below_pvalMax = cdfpvalMax, all_pval = cdfAll, inset = inset_plot)
    }

    cdfPlot
}

get_plot_limits <- function(main_plot, insetX, insetY, insetWidth) {
    main_plot_build <- ggplot_build(main_plot)
    xrange <- main_plot_build$layout$panel_params[[1]]$x.range
    yrange <- main_plot_build$layout$panel_params[[1]]$y.range

    x_range_val <- xrange[[2]] - xrange[[1]]
    xmin <- xrange[[1]] + (0.02 * x_range_val)
    xmax <- xmin + (insetWidth * x_range_val)

    y_range_val <- yrange[[2]] - yrange[[1]]
    ymin <- yrange[[2]] - (0.02 * y_range_val)
    ymax <- ymin - (insetWidth * y_range_val)
    list("xmin" = xmin,
         "xmax" = xmax,
         "ymin" = ymin,
         "ymax" = ymax)
}
