#' Plot edgeR dispersion from a DGEobj
#'
#' Creates an edgeR dispersion plot for RNA-Seq QC purposes.  Takes a counts matrix
#' or DGEList for input.  Dispersion is plotted against AveLogCPM.  Optionally,
#' the plot can instead be Biological Coefficient of Variation (BCV is the square root of
#' dispersion) against AveLogCPM.
#'
#' @param dgeObj DGEobj.
#' @param replicateGroupCol A singular value of class character and must be in design data. (default = "ReplicateGroup")
#' @param countsMatrix If TRUE, uses the countsMatrix in DGEobj to construct the plot else DGElist will be used. (default = FALSE)
#' @param plotType Plot type must be canvasXpress or ggplot (default = canvasXpress).
#' @param plotCategory One of "dispersion" or "BCV" (default = "dispersion")
#' @param lineFit (default = NULL) Any type supported by geom_smooth(if plotType is ggplot) or
#' one of glm, lm, loess, gam if plotType is canvasXpress. Loess is recommended.
#'   recommended.
#' @param ... Extra parameters to pass to edgeR::estimateDisp
#'
#' @return canvasxpress or ggplot object based on plotType selection
#'
#' @examples
#' \dontrun{
#'    # canvasxpress plot
#'    myCxplot <- plotDispersion(myDGEobj)
#'
#'    # ggplot
#'    myGgplot <- plotDispersion(myDGEobj, designMatrix, plotType = "ggplot")
#' }
#'
#' @importFrom assertthat assert_that see_if
#' @importFrom edgeR calcNormFactors estimateDisp DGEList
#' @importFrom canvasXpress canvasXpress
#'
#'
#' @export
plotDispersion <- function(dgeObj,
                           countsMatrix = FALSE,
                           replicateGroupCol = "ReplicateGroup",
                           plotType     = "canvasXpress",
                           plotCategory = "dispersion",
                           lineFit      = NULL,
                           ...) {

    assertthat::assert_that(!missing(dgeObj),
                            !is.null(dgeObj),
                            "DGEobj" %in% class(dgeObj),
                            msg = "dgeObj must be specified and must belong to DGEobj class.")

    design_names <- names(DGEobj::getType(dgeObj, type = "design"))
    assertthat::assert_that(length(design_names) == 1,
                            msg = "DGEobj must have exactly one design object.")
    design <- DGEobj::getType(dgeObj, type = "design")[[1]]

    if (any(is.null(replicateGroupCol),
            !is.character(replicateGroupCol),
            length(replicateGroupCol) != 1,
            !replicateGroupCol %in% names(design))) {
        warning("replicateGroupCol must be a singular value of class character and must be a column name in design data. Assigning default value 'ReplicateGroup'.")
        replicateGroupCol <- "ReplicateGroup"
    }

    designMatrix <- stats::model.matrix(~ 0 + ReplicateGroup, design)

    if (any(is.null(countsMatrix),
            !is.logical(countsMatrix),
            length(countsMatrix) != 1)) {
        warning("countsMatrix must be a singular logical value. Assigning default value FALSE.")
        countsMatrix <- FALSE
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

    if (countsMatrix) {
        dgelist <- dgeObj$counts %>%  # Process a counts matrix
            as.matrix %>%
            edgeR::DGEList() %>%
            edgeR::calcNormFactors() %>%
            edgeR::estimateDisp(design = designMatrix, robust = TRUE, ...)
    } else {
        dgelist <- dgeObj$DGEList %>%
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
                                                 colors                  = "darkblue",
                                                 scatterOutlineThreshold = 0,
                                                 title                   = title,
                                                 yAxisTitle              = ylab,
                                                 showLoessFit            = showLoessFit,
                                                 fitLineColor            = "red",
                                                 fitLineStyle            = "solid",
                                                 afterRender             = afterRender)


    } else {
        MyDispPlot <- ggplot(plotdata, aes(x = AveLogCPM, y = Dispersion)) +
            geom_point(shape = "circle",
                       fill  = "darkblue",
                       color = "darkblue")

        if (!is.null(lineFit)) {
            MyDispPlot <- MyDispPlot +
                geom_smooth(formula  = y ~ x,
                            method   = lineFit,
                            color    = "red",
                            linetype = "solid")
        }

        MyDispPlot <- MyDispPlot +
            ylab(ylab) +
            ggtitle(title) +
            expand_limits(x = 0, y = 0)
    }

    MyDispPlot
}
