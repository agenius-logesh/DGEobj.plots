#' Create limma MDS plot
#'
#' This is a wrapper around the plotMDS function that generates the plot with
#' canvasXpress or ggplot2 instead of base graphics.
#'
#' colorBy, shapeBy, and sizeBy are grouping variables that encode group info by
#' color, shape, or size.  These are input as column names in the designTable.
#'
#' The underlying limma::plotMDS() function uses a default of top = 500 to use the top 500
#' highest fold change genes for the analysis. Based on observed speed tests,
#' top = Inf has been utilized as the default for this function, as it was shown to quickly produce
#' a more stable result. However, this is configurable using the top argument, which
#' allows for selection of a number close the number of differential genes in the
#' input data.
#'
#' @param dgeObj DGEobj.
#' @param plotType Plot type must be canvasXpress or ggplot (Default to canvasXpress).
#' @param designTable Name of the design table object
#' @param colorBy A column name in the design table.Points are colored by the values in that column (Required)
#' @param shapeBy A column name in the design table.Points are shaped by the values in that column (Optional)
#' @param sizeBy A column name in the design table.Points are sized by the values in that column (Optional)
#' @param top Number of most variant genes to include (Default = Inf)
#' @param labels A column name in the design table. Text labels for the samples. These should be short
#'   abbreviations of the sample identifiers.
#'   Default = ReplicateGroup or rownames of dgeObj. Set to NULL to disable
#'   text labels.
#' @param title A title for the plot. (Optional)
#' @param vlineIntercept X intercept of vertical line (Optional)
#' @param hlineIntercept Y intercept of horizontal line (Optional)
#' @param dim.plot Define which dimension to plot. dim.plot should a numeric vector of
#' length 2 and should be lesser than the number of columns in DGEobj. (default = c(1,2))
#'
#' @return A list with two elements, the ggplot object and the MDS object returned
#'    by the plotMDS() function.
#'
#' @examples
#' \dontrun{
#'      # Plot the first two dimensions using all genes
#'      myMDS_cxplot <- ggplotMDS(MyDGEobj)
#'
#'      # Plot the 2nd and 3rd dimensions using the top 1000 genes
#'      myMDS_cxplot <- ggplotMDS(MyDGEobj, dim.plot = c(2, 3))
#'      myMDS_cxplot[[1]]
#'
#'      # MDSplot - ggplot
#'      myMDS_ggplot <- ggplotMDS(MyDGEobj, plotType = "ggplot")
#'      myMDS_ggplot <- ggplotMDS(MyDGEobj, plotType = "ggplot", dim.plot = c(2, 3))
#'      myMDS_ggplot[[1]]
#' }
#'
#' @import ggplot2 magrittr ggrepel
#' @importFrom canvasXpress canvasXpress
#' @importFrom htmlwidgets JS
#' @importFrom assertthat assert_that
#' @importFrom limma plotMDS
#' @importFrom stats as.dist
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr rename left_join select mutate
#' @importFrom DGEobj getItem
#'
#' @export
ggplotMDS <- function(dgeObj,
                      plotType     = "canvasXpress",
                      designTable  = "design",
                      colorBy      = "ReplicateGroup",
                      shapeBy      = NULL,
                      sizeBy       = NULL,
                      top          = Inf,
                      labels       = "ReplicateGroup",
                      title,
                      xlab,
                      ylab,
                      hlineIntercept,
                      vlineIntercept) {

    assertthat::assert_that(!missing(dgeObj),
                            !is.null(dgeObj),
                            "DGEobj" %in% class(dgeObj),
                            msg = "dgeObj must be specified and must belong to DGEobj class.")

    plotType <- tolower(plotType)
    if (any(is.null(plotType),
            !is.character(plotType),
            length(plotType) != 1,
            !plotType %in% c("canvasxpress", "ggplot"))) {
        warning("plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'.")
        plotType <- "canvasxpress"
    }

    design <- NULL
    design_default <- TRUE
    if (!is.null(designTable) &&
        length(designTable) == 1 &&
        is.character(designTable) &&
        designTable %in% names(dgeObj)) {
        design_default    <- FALSE
        design            <- DGEobj::getItem(dgeObj, designTable)
        colnames(design)  <- tolower(colnames(design))
    } else {
        warning("designTable is either missing or invalid. Assigning default object of type 'design'.")
    }

    if (design_default) {
        design_names <- DGEobj::getType(dgeObj, "design")
        if (length(design_names) == 1) {
            design           <- DGEobj::getItem(dgeObj, "design")
            colnames(design) <- tolower(colnames(design))
        }
    }

    if (is.null(design)) {
        warning("designTable is either missing or invalid and the default value 'design' is not present in the dgeObj. Unable to color,size or shape points on the plot.")
    }

    colorby_default <- TRUE
    if (!is.null(design)) {
        if (any(is.null(colorBy), !is.null(colorBy) && tolower(colorBy) %in% colnames(design))) {
            colorby_default <- FALSE
        }
    } else {
        colorBy <- NULL
    }

    if (colorby_default)  {
        colorBy <- NULL
        msg     <- "colorBy value specified is invalid or missing. Assigning default value"
        if (!is.null(design) && ("replicategroup" %in% colnames(design))) {
            colorBy <- "replicategroup"
            msg     <- paste(msg,"'ReplicateGroup'.")
        } else {
            colorBy <- NULL
            msg     <- paste(msg, "'NULL'.")
        }
        warning(msg)
    }

    if (!is.null(design)) {
        if (!is.null(shapeBy) &&
        !(tolower(shapeBy) %in% colnames(design))) {
        warning("shapeBy should be a column in the design attribute of dgeObj. Assigning NULL as default value.")
        shapeBy <- NULL
        }
    } else {
        shapeBy <- NULL
    }

    if (!is.null(design)) {
        if (!is.null(sizeBy) &&
        !(tolower(sizeBy) %in% colnames(design))) {
        warning("sizeBy should be a column in the design attribute of dgeObj. Assigning NULL as default value.")
        sizeBy <- NULL
        }
    } else {
        sizeBy <- NULL
    }

    if (any(is.null(top), length(top) != 1, !is.numeric(top))) {
        warning("top should be a numeric value or Inf. Assigning default value 'Inf'.")
        top <- Inf
    }

    # Validate labels
    addLabels       <- TRUE
    addDefaultLabel <- TRUE

    if (is.null(labels)) {
        addLabels <- FALSE
    } else if (!is.null(design) && (tolower(labels) %in% colnames(design))) {
        addDefaultLabel <- FALSE
    } else if (!is.null(design) && (!(tolower(labels) %in% colnames(design)))) {
        warning("label specifed is either missing or invalid. Assigning default values.")
    }

    if (addDefaultLabel) {
        labels <- NULL
        # Get labels from ReplicateGroup if present
        if ("DGEobj" %in% class(dgeObj)) {
            if (exists("design") && (with(design, exists("replicategroup")))) {
                    labels <- "replicategroup"
            }
        }
    }

    if (!missing(hlineIntercept) && !is.null(hlineIntercept)) {
        if (!is.numeric(hlineIntercept)) {
            warning("hlineIntercept must be numeric. Ignoring hlineIntercept.")
            hlineIntercept <- NULL
        }
    }

    if (!missing(vlineIntercept) && !is.null(vlineIntercept)) {
        if (!is.numeric(vlineIntercept)) {
            warning("vlineIntercept must be numeric. Ignoring vlineIntercept.")
            vlineIntercept <- NULL
        }
    }

    if ((!missing(xlab)) &&
        (!is.null(xlab)) &&
        ((!is.character(xlab)) || length(xlab) != 1)) {
        warning("xlab value specified is not valid. Ignoring xlab.")
        xlab <- NULL
    }

    if (!missing(ylab) &&
        !is.null(ylab) &&
        (!is.character(ylab) || length(ylab) != 1)) {
        warning("ylab value specified is not valid. Ignoring ylab.")
        ylab <- NULL
    }

    # ColorBlind palette:
    # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
    cbbPalette <- c("#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#E69F00",  "#F0E442", "#000000")
    colors <- cbbPalette

    if (missing(title)) {
        title <- "MDS Plot"
    } else {
        if (!is.character(title) || length(title) != 1) {
            warning("Invalid value specified for Title. Assigning default values 'MDS plot'.")
            title <- "MDS Plot"
        }
    }

    if ("DGEobj" %in% class(dgeObj)) {
        dgeObj <- DGEobj::getItem(dgeObj, "DGEList")
    }

    mds.data <- limma::plotMDS(dgeObj,
                          top = top,
                          plot = FALSE)

    # Pull the plotting data together
    plot_data <- merge(mds.data$x, mds.data$y, by = "row.names")
    plot_data <- dplyr::rename(plot_data,sampleID = Row.names)

    byColor <- FALSE
    if (!is.null(colorBy)) {
        colorBy <- tolower(colorBy)
        colorby_data <- design %>%
            dplyr::select(!!colorBy) %>%
            dplyr::rename(ColorCode = !!colorBy) %>%
            tibble::rownames_to_column("sampleID")
        plot_data <- plot_data %>%
            dplyr::left_join(colorby_data, by = "sampleID")
        byColor <- TRUE
    }

    if (addLabels) {
        if (!is.null(labels)) {
            labels <- tolower(labels)
            labels_data <- design %>%
                dplyr::select(!!labels) %>%
                dplyr::rename(Labels = !!labels) %>%
                tibble::rownames_to_column("sampleID")
            plot_data <- plot_data %>%
                dplyr::left_join(labels_data, by = "sampleID")
        } else {
            plot_data <- plot_data %>%
                dplyr::mutate(Labels = sampleID)
        }
    }

    byShape <- FALSE
    bySize  <- FALSE
    if (!missing(shapeBy) && !is.null(shapeBy)) {
        shapeBy <- tolower(shapeBy)
        shapeby_data <- design %>%
            dplyr::select(!!shapeBy) %>%
            dplyr::rename(Shape = !!shapeBy) %>%
            tibble::rownames_to_column("sampleID")
        plot_data <- plot_data %>%
            dplyr::left_join(shapeby_data, by = "sampleID")
        byShape <- TRUE
    }

    if (!missing(sizeBy) && !is.null(sizeBy)) {
        sizeBy <- tolower(sizeBy)
        sizeby_data <- design %>%
            dplyr::select(!!sizeBy) %>%
            dplyr::rename(Size = !!sizeBy) %>%
            tibble::rownames_to_column("sampleID")
        plot_data <- plot_data %>%
            dplyr::left_join(sizeby_data, by = "sampleID")
        bySize <- TRUE
    }

    rownames(plot_data) <- plot_data$sampleID;plot_data$sampleID <- NULL
    xylab               <- list(paste(mds.data$axislabel, mds.data$dim.plot[[1]], sep = " "),
                                paste(mds.data$axislabel, mds.data$dim.plot[[2]], sep = " "))
    citation            <- paste("top ", mds.data$top, " genes : gene.selection = ",
                                 mds.data$gene.selection, sep = "")

    colorCol <- NULL
    shapeCol <- NULL
    sizeCol  <- NULL

    if (missing(xlab) || is.null(xlab)) {
        xlab <- xylab[[1]]
    }

    if (missing(ylab) || is.null(ylab)) {
        ylab <- xylab[[2]]
    }

    if (byColor) {
        colorCol <- "ColorCode"
    }

    if (byShape) {
        shapeCol <- "Shape"
    }

    if (bySize) {
        sizeCol <- "Size"
    }

    # PlotType
    if (plotType == "canvasxpress") {
        colors   <- lapply(colors, .rgbaConversion)
        reflineColor <- "red"
        decorations  <- list()
        hlineIntercept_list <- list()
        if (!missing(hlineIntercept) && !is.null(hlineIntercept)) {
            hlineIntercept_list <- lapply(seq_along(hlineIntercept),function(i) {
                list(color = reflineColor,
                     width = 0.5,
                     y     = hlineIntercept[[i]])
            })
        }

        vlineIntercept_list <- list()
        if (!missing(vlineIntercept) && !is.null(vlineIntercept)) {
            vlineIntercept_list <- lapply(seq_along(vlineIntercept), function(i) {
                list(color = reflineColor,
                     width = 0.5,
                     x     = vlineIntercept[[i]])
            })
        }

        intercept_list <- c(hlineIntercept_list, vlineIntercept_list)
        decorations    <- list(line = intercept_list)
        cx.data  <- plot_data %>%  dplyr::select(c(x, y))
        var.data <- plot_data %>% dplyr::select(-c(x, y))

        events <- htmlwidgets::JS("{ 'mousemove' : function(o, e, t) {
                                                if (o != null && o != false) {
                                                    if (o.objectType == null && o.z.Labels != null) {
                                                        t.showInfoSpan(e, '<b>' +  o.z.Labels[0] + '</b> <br/>' +
                                                        '<b>' + 'SampleID' + ': ' + o.y.vars+ '</b> <br/>' +
                                                        '<b>' + o.y.smps[0]  + '</b>' + ': ' + o.y.data[0][0] + '<br/>' +
                                                        '<b>' + o.y.smps[1]  + '</b>' + ': ' + o.y.data[0][1]);
                                                    } else {
                                                        t.showInfoSpan(e, o.display);
                                                    };
                                                }; }}")
        mdsplot <- canvasXpress::canvasXpress(data                    = cx.data,
                                              varAnnot                = var.data,
                                              decorations             = decorations,
                                              graphType               = "Scatter2D",
                                              colorBy                 = colorCol,
                                              shapeBy                 = shapeCol,
                                              sizeBy                  = sizeCol,
                                              dataPointSize           = 10,
                                              showDecorations         = TRUE,
                                              shapes                  = "circle",
                                              colors                  = colors,
                                              title                   = title,
                                              xAxisTitle              = xlab,
                                              yAxisTitle              = ylab,
                                              citation                = citation,
                                              citationScaleFontFactor = 0.8,
                                              events                  = events)
    } else {
        shapes <- .get_valid_symbolShapes_ggplot()[1:8]
        sizes <- c(1:8)
        symColor <- "blue"
        mdsplot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y))
        geom_point_params <- list()

        if (byColor) {
            mdsplot <- mdsplot + ggplot2::aes(color = ColorCode)
        } else {
            geom_point_params[["color"]] = symColor
        }

        if (byShape) {
            mdsplot <- mdsplot + ggplot2::aes(shape = Shape) +  ggplot2::scale_shape_manual(values = shapes)
        } else {
            geom_point_params[["shape"]] = "circle"
        }

        if (bySize) {
            mdsplot <- mdsplot + ggplot2::aes(size = Size) + ggplot2::scale_size_manual(values = sizes)
        } else {
            geom_point_params[["size"]] = 10
        }

        mdsplot <- mdsplot + ggplot2::layer(geom = "point",
                                            stat = "identity",
                                            position = "identity",
                                            params = geom_point_params)

        if (addLabels) {
                mdsplot <- mdsplot +
                    ggrepel::geom_text_repel(ggplot2::aes(label = Labels), size = 3, max.overlaps = Inf)
        }

        #For discrete color values
        if (length(unique(colorBy)) <= length(colors)) {
            mdsplot <- mdsplot +
                ggplot2::scale_fill_manual(values = colors) +
                ggplot2::scale_colour_manual(values = colors)
        }

        # Add some other common elements
        mdsplot <- mdsplot +
            ggplot2::coord_fixed() +
            ggplot2::xlab(xlab) +
            ggplot2::ylab(ylab) +
            ggplot2::ggtitle(title)

        # Place an annotation on the bottom left of the plot
        xrange <- xrange(mdsplot)
        yrange <- yrange(mdsplot)
        # Put the annotation 10% from xmin
        xpos <- xrange[1] + ((xrange[2] - xrange[1]) * 0.1 )
        mdsplot <- mdsplot + ggplot2::annotate("text",
                                               x = xpos,
                                               y = yrange[1],
                                               label = citation,
                                               hjust = 0,
                                               size = ggplot2::rel(2.5),
                                               color = "grey30")

        if (!missing(hlineIntercept)) {
            mdsplot <- mdsplot + ggplot2::geom_hline(yintercept = hlineIntercept,
                                                     color = "red",
                                                     size = 0.5)
        }
        if (!missing(vlineIntercept)) {
            mdsplot <- mdsplot + ggplot2::geom_vline(xintercept = vlineIntercept,
                                                     color = "red",
                                                     size = 0.5)
        }
    }
    list(plot = mdsplot, mdsobj = mds.data)
}


#' Explain variance of MDS object or log2 matrix
#'
#' Takes a class MDS object from limma::plotMDS() and generates two plots: 1)
#' fraction of variance for each dimension, 2) cumulative variance. By default,
#' it plots the first 10 dimensions or the first N dimensions totaling 90%.
#'
#' @param mds A class MDS object from limma::plotMDS() or a data matrix to analyze
#'   (typically log2) (required)
#' @param plotType Plot type must be canvasXpress or ggplot (default = canvasXpress).
#' @param topN The number of dimensions to plot (default = 10)
#' @param cumVarLimit The maximum cumulative variance to plot. Range 0-1. (default = 0.9)
#' @param barColor Fill and outline color default = "dodgerblue4"
#' @param barWidth. Range 0-1. Applicable only for ggplot. (default = 0.65)
#'
#' @return A list with two plots(ggplot or canvasXpress plots) and the variance explained data.frame.
#'
#' @examples
#' \dontrun{
#'      # Plot the first two dimensions
#'      MyMDS <- ggplotMDS(MyDGEobj)
#'      MyMDS[[1]]  #the MDS plot
#'
#'      # Then apply MDS_var_explained (the MDS object is MyMDS[[2]])
#'      varResults <- MDS_var_explained(MyMDS[[2]])
#'      varResults[[1]] # The Variance per dimension plot
#'      varResults[[2]] # The cumulative variance plot
#'      var_explained <- varResults[[3]]  # Data used for plotting (unfiltered)
#'
#'      # MDS_var_explained - ggplot
#'      varResults_ggplot <- MDS_var_explained(MyMDS[[2]], plotType = "ggplot")
#'      varResults_ggplot[[1]] # The Variance per dimension plot
#'      varResults_ggplot[[2]] # The cumulative variance plot
#'      var_explained <- varResults_ggplot[[3]]  # Data used for plotting (unfiltered)
#' }
#'
#' @import ggplot2 magrittr
#' @importFrom assertthat assert_that
#' @importFrom limma plotMDS
#' @importFrom stats cmdscale var
#' @importFrom canvasXpress canvasXpress
#' @importFrom stringr str_c
#'
#' @export
MDS_var_explained <- function(mds,
                              plotType = "canvasXpress",
                              topN = 10,
                              cumVarLimit = 0.9,
                              barColor="dodgerblue4",
                              barWidth = 0.65) {
    plotType <- tolower(plotType)

    assertthat::assert_that(!missing(mds),
                            msg = "mds is required and must be specified.")
    assertthat::assert_that(!is.null(plotType),
                            plotType %in% c("canvasxpress", "ggplot"),
                            msg = "Plot type must be either canvasXpress or ggplot.")

    #input parameter validation
    if (any(is.null(topN), length(topN) != 1, !is.numeric(topN), topN < 1)) {
        warning("topN should be a numeric value. Assigning default value 10.")
        topN <- 10
    }

    if (any(is.null(cumVarLimit), length(cumVarLimit) != 1, !is.numeric(cumVarLimit), cumVarLimit < 0, cumVarLimit > 1)) {
        warning("cumVarLimit should be a single numeric value between 0 and 1. Assigning default value 0.9.")
        cumVarLimit <- 0.9
    }

    if (any(is.null(barColor), !is.character(barColor), length(barColor) != 1, .rgbaConversion(barColor) == "invalid value")) {
        warning("barColor specified is not valid. Assigning default value 'dodgerblue4'.")
        barColor <- "dodgerblue4"
    }

    if (any(is.null(barWidth), !is.numeric(barWidth), length(barWidth) != 1, barWidth < 0, barWidth > 1)) {
        warning("barWidth should be a single numeric value between 0 and 1. Assigning default value 0.65.")
        barWidth <- 0.65
    }

    if (!("MDS" %in% class(mds))) {
        mds <- limma::plotMDS(mds, plot = FALSE)
    }

    mds.distances <- mds %$% distance.matrix %>% as.dist
    mdsvals <- mds.distances %>%
        {suppressWarnings(cmdscale(., k = ncol(mds$distance.matrix) - 1))} %>%
        magrittr::set_colnames(stringr::str_c("Dim", seq_len(ncol(.)))) %>%
        as.data.frame
    var_vec <- unname(apply((mdsvals %>% as.matrix), 2, stats::var)) %>%
        magrittr::divide_by(sum(.))
    var_explained <- data.frame(var    = var_vec,
                                cumvar = cumsum(var_vec),
                                dim    = seq_along(var_vec))

    idx <- var_explained$cumvar < cumVarLimit
    if (sum(idx) < topN) {
        topN <- sum(idx)
    }
    plotdat <- var_explained[idx,][1:topN,]

    resultList   <- list()
    varexp_title <- "Variance Explained by MDS Dimensions"
    cumvar_title <- "Cumulative Variance Explained by MDS Dimensions"

    xlab    <- "MDS dimension"
    ylab_ve <- "Variance Explained"
    ylab_cv <- "Cumulative Variance Explained"

    if (plotType == "canvasxpress") {
        cx.data <- as.data.frame(t(plotdat))
        resultList$varexp <- canvasXpress::canvasXpress(data             = cx.data["var", ],
                                                        graphOrientation = "vertical",
                                                        graphType        = "Bar",
                                                        showLegend       = FALSE,
                                                        colors           = barColor,
                                                        title            = varexp_title,
                                                        xAxisTitle       = ylab_ve,
                                                        smpTitle         = xlab,
                                                        smpLabelRotate   = 90)
        resultList$cumvar <- canvasXpress::canvasXpress(data             = cx.data["cumvar", ],
                                                        graphOrientation = "vertical",
                                                        graphType        = "Bar",
                                                        showLegend       = FALSE,
                                                        colors           = barColor,
                                                        title            = cumvar_title,
                                                        xAxisTitle       = ylab_cv,
                                                        smpTitle         = xlab,
                                                        smpLabelRotate   = 90)
    } else {
        # Fraction variance for each dimension
        resultList$varexp <- ggplot2::ggplot(plotdat) +
            ggplot2::aes(x = dim, y = var) +
            ggplot2::geom_col(color = barColor,
                              fill = barColor,
                              width = barWidth) +
            ggplot2::labs(title = varexp_title,
                          x = xlab,
                          y = ylab_ve) +
            ggplot2::scale_x_continuous(breaks = setBreaks)

        # Cumulative variance plot (change the y dimension and relabel)
        resultList$cumvar <- resultList$varexp + ggplot2::aes(y = cumvar) +
            ggplot2::labs(title = cumvar_title,
                          y = ylab_cv) +
            ggplot2::ylim(0,1)
    }
    # Return the full data table too
    resultList$var_explained <- var_explained

    resultList
}

setBreaks <- function(limits){
    # Return integer breaks
    low <- floor(limits[1])
    high <- ceiling(limits[2])
    seq(from = low, to = high, by = 1)
}
