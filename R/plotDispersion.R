#' Plot edgeR dispersion from a DGEobj
#'
#' Creates an edgeR dispersion plot for RNA-Seq QC purposes.  Takes a DGEobj which has a counts matrix
#' or DGEList as input.  Dispersion is plotted against AveLogCPM.  Optionally,
#' the plot can instead be Biological Coefficient of Variation (BCV is the square root of
#' dispersion) against AveLogCPM.
#'
#' @param dgeObj DGEobj which has a counts matrix or a DGElist
#' @param replicateGroupCol A singular value of class character and must be a column name in design object (default = "ReplicateGroup")
#' @param countsMatrix If TRUE, uses the countsMatrix in DGEobj to construct the plot else DGElist will be used. (default = TRUE)
#' @param plotType Plot type must be canvasXpress or ggplot (default = canvasXpress).
#' @param plotCategory One of "dispersion" or "BCV" (default = "dispersion")
#' @param lineFit If the plotType is ggplot, any method supported by `geom_smooth()`. If the plotType is canvasXpress, one of glm, lm, loess, gam is accepted .
#' Loess is recommended. (default = NULL)
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
#'    myGgplot <- plotDispersion(myDGEobj, plotType = "ggplot")
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom edgeR calcNormFactors estimateDisp DGEList
#' @importFrom canvasXpress canvasXpress
#'
#'
#' @export
plotDispersion <- function(dgeObj,
                           countsMatrix = TRUE,
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
                            msg = "dgeObj must have exactly one design object.")
    design <- DGEobj::getType(dgeObj, type = "design")[[1]]

    if (any(is.null(replicateGroupCol),
            !is.character(replicateGroupCol),
            length(replicateGroupCol) != 1,
            !replicateGroupCol %in% names(design))) {
        warning("replicateGroupCol must be a singular value of class character and must be a column name in design data. Assigning default value 'ReplicateGroup'.")
        replicateGroupCol <- "ReplicateGroup"
    }

    designMatrix <- stats::model.matrix(as.formula(paste("~ 0 +", replicateGroupCol)), design)

    if (any(is.null(countsMatrix),
            !is.logical(countsMatrix),
            length(countsMatrix) != 1)) {
        warning("countsMatrix must be a singular logical value. Assigning default value TRUE.")
        countsMatrix <- TRUE
    }

    if (countsMatrix) {
        counts_names <- suppressWarnings(names(DGEobj::getType(dgeObj,"counts")))
        assertthat::assert_that(length(counts_names) == 1,
                                msg = "dgeObj needs to have exactly one counts matrix." )
    } else {
        DGEList_names <- suppressWarnings(names(DGEobj::getType(dgeObj,"DGEList")))
        assertthat::assert_that(length(DGEList_names) == 1,
                                msg = "dgeObj needs to have exactly one DGEList." )
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
        dgelist <- DGEobj::getType(dgeObj, "counts")[[1]] %>%  # Process a counts matrix
            as.matrix %>%
            edgeR::DGEList() %>%
            edgeR::calcNormFactors() %>%
            edgeR::estimateDisp(design = designMatrix, robust = TRUE, ...)
    } else {
        dgelist <- DGEobj::getType(dgeObj, "DGEList")[[1]] %>%
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
