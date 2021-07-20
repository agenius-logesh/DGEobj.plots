#' Plot edgeR dispersion from a DGEobj
#'
#' Creates an edgeR dispersion plot for RNA-Seq QC purposes.  Takes a counts matrix
#' or DGEList for input.  Dispersion is plotted against AveLogCPM.  Optionally,
#' the plot can instead be Biological Coefficient of Variation (BCV is the square root of
#' dispersion) against AveLogCPM.
#'
#' @param DGEdata DGEobj.
#' @param matrixType design or counts. If matrixType is design it will take the DGEList data in DGEobj and
#'        if matrixType is counts it will take the counts data in DGEobj. (default = design)
#' @param plotType Plot type must be canvasXpress or ggplot (default = canvasXpress).
#' @param plotCategory One of "dispersion" or "BCV" (default = "dispersion")
#' @param symbolSize (default = 6)
#' @param symbolShape Any supported shape for the points (default = "circle")
#' \url{http://www.cookbook-r.com/Graphs/Shapes_and_line_types/}
#' @param symbolColor Fill color (default = "darkblue")
#' @param symbolTransparency Transparency for the points. Value ranges from 0 to 1. Smaller
#'   indicate more transparency (default = 0.5)
#' @param linefitColor (default = "red")
#' @param lineFit (default = NULL) Any type supported by geom_smooth(if plotType is ggplot) or
#' one of glm, lm, loess, gam if plotType is canvasXpress. Loess is recommended.
#'   recommended.
#' @param lineType Any supported style for the fitLine  (default = solid).
#'  \url{http://www.cookbook-r.com/Graphs/Shapes_and_line_types/}
#' @param ... Extra parameters to pass to edgeR::estimateDisp
#'
#' @return canvasxpress or ggplot object based on plotType selection
#'
#' @examples
#' \dontrun{
#'    # canvasxpress plot
#'    myCxplot <- plotDispersion(myDGEobj, matrixType = "counts")
#'
#'    # ggplot
#'    myGgplot <- plotDispersion(myDGEobj, matrixType = "counts", plotType = "ggplot")
#' }
#'
#' @importFrom assertthat assert_that see_if
#' @importFrom edgeR calcNormFactors estimateDisp DGEList
#' @importFrom canvasXpress canvasXpress
#'
#'
#' @export
plotDispersion <- function(DGEdata,
                           matrixType   = "design",
                           plotType     = "canvasXpress",
                           plotCategory = "dispersion",
                           symbolSize   = 6,
                           symbolShape  = "circle",
                           symbolColor  = "darkblue",
                           symbolTransparency  = 0.5,
                           linefitColor = "red",
                           lineFit      = NULL,
                           lineType     = "solid",
                           ...) {

    assertthat::assert_that(!missing(DGEdata),
                            !is.null(DGEdata),
                            "DGEobj" %in% class(DGEdata),
                            msg = "DGEdata must be specified and must belong to DGEobj class.")

    designMatrix <- stats::model.matrix(~ 0 + ReplicateGroup, getItem(DGEdata, "design"))

    matrixType <- tolower(matrixType)
    if (any(is.null(matrixType),
            !is.character(matrixType),
            length(matrixType) != 1,
            !matrixType %in% c("design", "counts"))) {
        warning("matrixType must be either design or counts. Assigning default value 'design'.")
        matrixType <- "design"
    }

    plotType <- tolower(plotType)
    if (any(is.null(plotType),
            !is.character(plotType),
            length(plotType) != 1,
            !plotType %in% c("canvasxpress", "ggplot"))) {
        warning("plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'.")
        plotType <- "canvasxpress"
    }

    plotCategory <- tolower(plotCategory)
    if (any(is.null(plotCategory),
            !is.character(plotCategory),
            length(plotCategory) != 1,
            !plotCategory %in% c("dispersion", "bcv"))) {
        warning("plotCategory must be either dispersion or bcv. Assigning default value 'dispersion'.")
        plotCategory <- "dispersion"
    }

    if (!is.null(lineFit) &&
        !all(is.character(lineFit),
             length(lineFit) == 1,
             tolower(lineFit) %in% c('glm', 'lm', 'loess', 'gam')))  {
        warning("lineFit must be one value from 'glm', 'lm', 'loess', 'gam' or 'NULL' to disable. Assigning default value 'NULL'.")
        lineFit <- NULL
    }

    if (!is.null(lineFit)) {
        if (any(is.null(linefitColor),
                !is.character(linefitColor),
                length(linefitColor) != 1,
                length(.validate_colors(linefitColor)) != 1)) {
            warning("linefitColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'red'.")
            linefitColor <- "red"
        }

        if (any(is.null(lineType),
                !is.character(lineType),
                length(lineType) != 1)) {
            warning("lineType must be a singular value of class character. Refer help section for the list of line types supported. Assigning default value 'solid'.")
            lineType <- "solid"
        }
    }

    if (any(is.null(symbolSize),
            !is.numeric(symbolSize),
            length(symbolSize)  != 1,
            !symbolSize >= 0)) {
        warning("symbolSize must be a singular numeric value. Assigning a default value of 6.")
        symbolSize  <-  6

    }

    if (any(is.null(symbolShape),
            !is.character(symbolShape),
            length(symbolShape)  != 1,
            plotType == "canvasxpress" && !is.null(symbolShape) && length(.validate_cx_shapes(symbolShape)) != 1,
            plotType == "ggplot" && !is.null(symbolShape) && length(.validate_gg_shapes(symbolShape)) != 1)) {
        warning("symbolShape must be a singular value of class 'character'. Assigning default value = 'circle'.")
        symbolShape  <- "circle"

    }

    if (any(is.null(symbolColor),
            !is.character(symbolColor),
            length(symbolColor)  != 1,
            length(.validate_colors(symbolColor)) != 1)) {
        warning("symbolColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'darkblue'.")
        symbolColor <- "darkblue"
    }

    if (any(is.null(symbolTransparency),
            !is.numeric(symbolTransparency),
            length(symbolTransparency) != 1,
            symbolTransparency <= 0,
            symbolTransparency > 1)) {
        warning("symbolTransparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.5'.")
        symbolTransparency <- 0.5
    }

    if (matrixType == "design") {
        dgelist <- DGEdata$DGEList %>%
            edgeR::calcNormFactors() %>%
            edgeR::estimateDisp(design = designMatrix, robust = TRUE, ...)
    } else {
        dgelist <- DGEdata$counts %>%  # Process a counts matrix
            as.matrix %>%
            edgeR::DGEList() %>%
            edgeR::calcNormFactors() %>%
            edgeR::estimateDisp(design = designMatrix, robust = TRUE, ...)
    }

    if (plotCategory == "dispersion") {
        plotdata <- data.frame(AveLogCPM = dgelist$AveLogCPM, Dispersion = dgelist$tagwise.dispersion)
        title <- "EdgeR Dispersion Plot"
        ylab  <- "Dispersion"
    } else {
        plotdata <- data.frame(AveLogCPM = dgelist$AveLogCPM, Dispersion = sqrt(dgelist$tagwise.dispersion))
        title <- "EdgeR BCV Plot"
        ylab  <- "BCV"
    }
    rownames(plotdata) <- rownames(dgelist$counts)
    MyDispPlot <- NULL

    if (plotType == "canvasxpress") {
        showLoessFit <- FALSE
        afterRender <- NULL
        if (!is.null(lineFit)) {
            if (lineFit %in% c("lm","glm")) {
                afterRender <- list(list("addRegressionLine"))
            } else if (lineFit %in% c("loess","gam")) {
                showLoessFit <- TRUE
            }
        }
        MyDispPlot <- canvasXpress::canvasXpress(data                    = plotdata,
                                                 graphType               = "Scatter2D",
                                                 colors                  = symbolColor,
                                                 dataPointSize           = symbolSize,
                                                 shapes                  = symbolShape,
                                                 transparency            = symbolTransparency,
                                                 scatterOutlineThreshold = 0,
                                                 title                   = title,
                                                 yAxisTitle              = ylab,
                                                 showLoessFit            = showLoessFit,
                                                 fitLineColor            = linefitColor,
                                                 fitLineStyle            = lineType,
                                                 afterRender             = afterRender)


    } else {
        MyDispPlot <- ggplot(plotdata, aes(x = AveLogCPM, y = Dispersion)) +
            geom_point(size  = symbolSize,
                       shape = symbolShape,
                       fill  = symbolColor,
                       color = symbolColor,
                       alpha = symbolTransparency)

        if (!is.null(lineFit)) {
            MyDispPlot <- MyDispPlot +
                geom_smooth(formula  = y ~ x,
                            method   = lineFit,
                            color    = linefitColor,
                            linetype = lineType)
        }

        MyDispPlot <- MyDispPlot +
            ylab(ylab) +
            ggtitle(title) +
            expand_limits(x = 0, y = 0)
    }

    MyDispPlot
}
