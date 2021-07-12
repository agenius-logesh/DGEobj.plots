#' Plot edgeR dispersion from a DGEobj
#'
#' Creates an edgeR dispersion plot for RNA-Seq QC purposes.  Takes a counts matrix
#' or DGEList for input.  Dispersion is plotted against AveLogCPM.  Optionally,
#' the plot can instead be Biological Coefficient of Variation (BCV is the square root of
#' dispersion) against AveLogCPM.
#'
#' @param DGEdata Counts matrix or DGEList (required)
#' @param designMatrix A design matrix created by stats::model.matrix (required)
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
#'    myCxplot <- plotDispersion(myDGElist, designMatrix )
#'    myCxplot <- plotDispersion(myDGEobj, designMatrix)
#'
#'    # ggplot
#'    myGgplot <- plotDispersion(myDGElist, designMatrix, plotType = "ggplot")
#'    myGgplot <- plotDispersion(myDGEobj, designMatrix, plotType = "ggplot")
#' }
#'
#' @importFrom assertthat assert_that see_if
#' @importFrom edgeR calcNormFactors estimateDisp DGEList
#' @importFrom canvasXpress canvasXpress
#'
#'
#' @export
plotDispersion <- function(DGEdata,
                           designMatrix,
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

    plotType = tolower(plotType)
    plotCategory = tolower(plotCategory)
    if (!is.null(lineFit)) {
        lineFit <- tolower(lineFit)
    }

    assertthat::assert_that(!missing(DGEdata),
                            !missing(designMatrix),
                            msg = "Both DGEdata and designMatrix must be specified.")

    assertthat::assert_that(class(DGEdata)[[1]] %in% c("matrix","DGEList"),
                            "matrix" %in% class(designMatrix),
                            msg = "DGEdata must be of class 'matrix' or 'DGEList' and designMatrix must be of class 'matrix'.")

    assertthat::assert_that(plotType %in% c("canvasxpress", "ggplot"),
                            msg = "Plot type must either be canvasXpress or ggplot.")

    assertthat::assert_that(plotCategory %in% c("dispersion", "bcv"),
                            msg = "Plot Category must  either be dispersion or BCV.")

    if (!is.null(lineFit)) {
        if (!assertthat::see_if(is.character(linefitColor),
                                length(linefitColor) == 1)) {
            warning("linefitColor must be a singular value of class 'character' and must specify the name of the color or the rgb value. Assigning default value 'red'.")
            linefitColor <- "red"
        }
    }

    if (plotType == "canvasxpress") {
        if (!assertthat::see_if(is.numeric(symbolSize),
                                length(symbolSize) == 1,
                                symbolSize > 0)) {
            warning("symbolSize must be a singular numeric value. Assigning a default value of 6.")
            symbolSize <- 6
        }

        if (!assertthat::see_if(is.character(symbolColor),
                                length(symbolColor) == 1)) {
            warning("symbolColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'darkblue'.")
            symbolColor <- "darkblue"
        }

        if (!assertthat::see_if(is.character(symbolShape),
                                length(symbolShape) == 1)) {
            warning("symbolShape must be a singular value of class 'character'. Assigning default value = 'circle'.")
            symbolShape <- "circle"
        }

        if (!assertthat::see_if(is.numeric(symbolTransparency),
                                length(symbolTransparency) == 1,
                                all(symbolTransparency > 0 & symbolTransparency <= 1))) {
            warning("symbolTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.5.")
            symbolTransparency <- 0.5
        }

        if (!is.null(lineFit)) {
            if (!assertthat::see_if(lineFit %in% c('glm', 'lm', 'loess', 'gam'),
                                    length(lineFit) == 1)) {
                warning("lineFit must be one of glm, lm, loess or gam. Assigning default value NULL")
                lineFit <- NULL
            }

            if (!assertthat::see_if(is.character(lineType),
                                    length(lineType) == 1)) {
                warning("lineType must be a must be a singular value of class 'character'. Refer help section for the list of line types supported. Assigning default value 'solid'.")
                lineType = "solid"
            }
        }
    }

    if (plotType == "ggplot") {
        if (!assertthat::see_if(is.numeric(symbolSize))) {
            warning("symbolSize must be of class numeric. Assigning a default value of 6.")
            symbolSize <- 6
        }

        if (!assertthat::see_if(is.character(symbolColor))) {
            warning("symbolColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'darkblue'.")
            symbolColor <- "darkblue"
        }

        if (!assertthat::see_if(is.numeric(symbolTransparency),
                                symbolTransparency >= 0 & symbolTransparency <= 1)) {
            warning("symbolTransparency must be a numeric value and must be between 0 and 1. Assigning default value 0.5.")
            symbolTransparency <- 0.5
        }

        if (!assertthat::see_if(class(symbolShape) %in% c("character", "numeric"),
                                length(symbolShape) == 1)) {
            warning("symbolShape must be a numeric value between 0 and 25 or must be of class 'character'. Refer help section for the list of shapes supported. Assigning default value 'circle'.")
            symbolShape <- "circle"
        }

        if ((!is.null(lineFit)) & (!assertthat::see_if(class(lineType) %in% c("character", "numeric"), length(lineType) == 1))) {
            warning("lineType must be a singular numeric value between 0 and 6 or must be of class 'character'. Refer help section for the list of line types supported. Assigning default value 'solid'.")
            lineType <- "solid"
        }
    }

    if (class(DGEdata)[[1]] == "DGEList") {
        dgelist <- DGEdata %>%
            edgeR::calcNormFactors() %>%
            edgeR::estimateDisp(design = designMatrix, robust = TRUE, ...)
    } else {
        dgelist <- DGEdata %>%  # Process a counts matrix
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
