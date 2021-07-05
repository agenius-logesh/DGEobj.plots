#' Plot logRatio contrasts
#'
#' Intended to plot a set of contrast results, one plot for each gene of
#' interest.
#'
#' Input is a tidy dataframe constructed from topTable output and
#' requires \strong{logFC}, \strong{CI.L}, and \strong{CI.R} columns as well as a gene identifier of choice.
#'
#' Outputs a canvasXpress \emph{(the default)} or ggplot2 object faceted by the facetColname when
#'  \code{facet} parameter is set to \strong{TRUE} \emph{(the default)}.
#'
#' If \code{facet} parameter is set to \strong{FALSE} the output will be a list of individual canvasXpress or
#' ggplots, one for each facetColname value \emph{(typically gene)}.
#'
#' @param contrastsDF A tidy dataframe of data to plot (Required) (see ?tidyContrasts).
#' @param plotType Plot type must be canvasXpress or ggplot (Default to canvasXpress).
#' @param facetColname Define the column name to separate plots (Required) (e.g. GeneID).
#' @param xColname Define the column name to group boxplots by (Required) (e.g. Contrast).
#' @param yColname Define the column name for the output of the boxplots (default = "logFC")
#' @param CI.R_colname Define name of the CI high value (default = "CI.R")
#' @param CI.L_colname Define name of the CI low value (default =  "CI.L")
#' @param plotCategory One of "bar" or "point" (default = "bar")
#' @param refLine Adds a horizontal line at y = 0 (default = TRUE)
#' @param refLineColor Color for the reference line (default = "red")
#' @param xlab X axis label (defaults to xColname)
#' @param ylab Y axis label (defaults to yColname)
#' @param title Plot title, set to NULL to disable (Optional, default is NULL)
#' @param barColor Color for the bar outline (default = "dodgerblue4")
#' @param barTransparency Transparency for the bar layer (default = 1)
#' @param pointColor Color for the point layer (default = "grey30")
#' @param pointShape Shape for the point layer (default = "circle")
#' @param barTransparency Transparency for the box layer (default = 1)
#' @param pointSize Size of the points (default = 4)
#' @param lineLayer Add a fitted line layer (default = FALSE)
#' @param lineColor Color of the line fit (default = "dodgerblue4")
#' @param lineSize Size of the line fit (default = 1)
#' @param lineFit Type of fit to use.  One of c("auto", "lm", "glm", "gam",
#'   "loess"). (default = "loess")
#' @param facet Specifies whether to facet (TRUE) or print individual plots
#'   (FALSE)  (default = TRUE)
#' @param facetCol Explicitly set the number of rows for the facet plot. default
#'   behavior will automatically set the columns. (default = ceiling(sqrt(length(unique(contrastsDF[facetCol])))))
#' @param labelAngle Angle to set the sample labels on the X axis (Default =  45; Range = 0-90)
#' @param axisFree Specify same scale or independent scales for each subplot (Default = TRUE;
#'   Allowed values: TRUE and FALSE)
#'
#' @return canvasXpress (the default) or a ggplot object:
#' #' \itemize{
#'   \item If \code{facet = TRUE} \emph{(default)}, returns a faceted canvasXpress or ggplot object.
#'   \item If \code{facet = FALSE}, returns a list of canvasXpress or ggplot objects indexed by observation (gene) names.
#' }
#'
#' @examples
#' \dontrun{
#'   # DGEobj example
#'   # Put contrasts in tidy format keeping logFC, and confidence limits contrastsDF
#'   tidyDat <- tidyContrasts(DGEobj,
#'                            rownameColumn = "EnsgID",
#'                            includeColumns = c("logFC", "CI.R", "CI.L"))
#'
#'   # Add gene symbols from geneData
#'   ens2genesym <- DGEobj$geneData %>%
#'                  rownames_to_column(var = "EnsgID") %>%
#'                  select(EnsgID, GeneSymbol = GeneName)
#'   tidyDat <- left_join(tidyDat, ens2genesym)
#'
#'   # Filter for a small set of genes of interest
#'   idx <- stringr::str_detect(tidyDat$GeneSymbol, "^PPAR")
#'   tidyDat <- tidyDat[idx,]
#'
#'   # Simple barplot
#'   logRatioPlot(tidyDat,
#'                facetColname = "GeneSymbol",
#'                xColname = "Contrast",
#'                facetCol = 2)
#'
#'   # Lineplot with some options
#'   logRatioPlot(tidyDat,
#'                plotCategory = "point",
#'                facetColname = "GeneSymbol",
#'                xColname = "Contrast",
#'                facetCol = 4,
#'                axisFree = FALSE,
#'                facet = TRUE,
#'                title = "Test",
#'                pointSize = 4,
#'                lineLayer = TRUE,
#'                lineSize = 0.1,
#'                labelAngle = 60)
#' }
#'
#' @import ggplot2 magrittr
#' @importFrom dplyr left_join filter arrange mutate case_when
#' @importFrom assertthat assert_that
#' @importFrom stringr str_c
#' @importFrom canvasXpress canvasXpress
#' @importFrom htmlwidgets JS
#'
#' @export
logRatioPlot <- function(contrastsDF,
                         plotType = "canvasXpress",
                         facetColname,
                         xColname,
                         yColname = "logFC",
                         CI.R_colname = "CI.R",
                         CI.L_colname = "CI.L",
                         plotCategory = "bar",
                         refLine = TRUE,
                         refLineColor = "red",
                         xlab = xColname,
                         ylab = yColname,
                         title = NULL,
                         barColor = "dodgerblue4",
                         barTransparency = 1,
                         pointColor = "dodgerblue4",
                         pointShape = "circle",
                         pointTransparency = 1,
                         pointSize = 2,
                         lineLayer = FALSE,
                         lineColor = "dodgerblue4",
                         lineSize = 1,
                         lineFit = "loess",
                         facet = TRUE,
                         facetCol,
                         labelAngle = 45,
                         axisFree = TRUE) {
    assertthat::assert_that(!missing(contrastsDF),
                            !is.null(contrastsDF),
                            nrow(contrastsDF) > 0,
                            "data.frame" %in% class(contrastsDF),
                            msg = "contrastsDF must be specified and should be of class 'data.frame'.")
    plotType <- tolower(plotType)
    if (any(length(plotType) != 1,
            !plotType %in% c("canvasxpress", "ggplot"))) {
        warning("plotType must be either canvasXpress or ggplot. Setting default value 'canvasXpress'")
        plotType <- "canvasxpress"
    }
    assertthat::assert_that(!missing(facetColname),
                            !is.null(facetColname),
                            length(facetColname) == 1,
                            facetColname %in% colnames(contrastsDF),
                            msg = "facetColname must be one of contrastsDF columns.")
    assertthat::assert_that(!missing(xColname),
                            !is.null(xColname),
                            length(xColname) == 1,
                            xColname %in% colnames(contrastsDF),
                            msg = "xColname must be one of contrastsDF columns.")
    if (any(is.null(yColname),
            length(yColname) != 1,
            !yColname %in% colnames(contrastsDF))) {
        if ("logFC" %in% colnames(contrastsDF)) {
            warning("yColname must be one of contrastsDF columns. Setting default value 'logFC'")
            yColname <- "logFC"
        } else{
            assertthat::assert_that(FALSE, msg = "yColname must be one of contrastsDF columns.")
        }
    }

    if (any(is.null(plotCategory),
            !is.character(plotCategory),
            length(plotCategory) != 1,
            !(tolower(plotCategory) %in% c("bar", "point")))) {
        warning("plotCategory must be either 'bar' or 'point'. Setting default value 'bar'")
        plotCategory  <-  "bar"
    } else {
        plotCategory <- tolower(plotCategory)
    }

    is_confidence_used <- TRUE
    if (any(is.null(CI.R_colname),
            length(CI.R_colname) != 1,
            !CI.R_colname %in% colnames(contrastsDF))) {
        if ("CI.R" %in% colnames(contrastsDF)) {
            warning("CI.R_colname must be one of contrastsDF columns. Setting default value 'CI.R'")
            CI.R_colname <- "CI.R"
        } else{
            warning("CI.R_colname must be one of contrastsDF columns. Disabling confidenece limits.")
            is_confidence_used <- FALSE
        }
    }

    if (is_confidence_used &&
        any(is.null(CI.L_colname),
            length(CI.L_colname) != 1,
            !CI.L_colname %in% colnames(contrastsDF))) {
        if ("CI.L" %in% colnames(contrastsDF)) {
            warning("CI.L_colname must be one of contrastsDF columns. Setting default value 'CI.L'")
            CI.L_colname <- "CI.L"
        } else{
            warning("CI.L_colname must be one of contrastsDF columns. Disabling confidenece limits.")
            is_confidence_used <- FALSE
        }
    }

    if (any(is.null(refLine),
           !is.logical(refLine),
           length(refLine) != 1)) {
        warning("refLine must be singular logical value. Setting default value 'TRUE'.")
        refLine <- TRUE
    }

    if (refLine) {
        if (any(is.null(refLineColor),
                !is.character(refLineColor),
                length(refLineColor) != 1)) {
            warning("refLineColor must be a singular value of class character. Assigning default value 'red'.")
            refLineColor <- "red"
        } else if (.rgbaConversion(refLineColor) == "invalid value") {
            warning("Color specified is not valid. Assigning default value 'red'.")
            refLineColor <- "red"
        }
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

    if (any(is.null(barColor),
            !is.character(barColor),
            length(barColor) != 1)) {
        warning("barColor must be a singular value of class character. Assigning default value 'dodgerblue4'.")
        barColor <- "dodgerblue4"
    } else if (.rgbaConversion(barColor) == "invalid value") {
        warning("Color specified is not valid. Assigning default value 'dodgerblue4'.")
        barColor <- "dodgerblue4"
    }

    if (any(is.null(barTransparency),
            !is.numeric(barTransparency),
            length(barTransparency) != 1,
            barTransparency <= 0,
            barTransparency > 1)) {
        warning("barTransparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '1'.")
        barTransparency <- 1
    }

    if (any(is.null(pointColor),
            !is.character(pointColor),
            length(pointColor) != 1)) {
        warning("pointColor must be a singular value of class character. Assigning default value 'dodgerblue4'.")
        pointColor <- "dodgerblue4"
    } else if (.rgbaConversion(pointColor) == "invalid value") {
        warning("Color specified is not valid. Assigning default value 'dodgerblue4'.")
        pointColor <- "dodgerblue4"
    }

    if (any(is.null(pointShape),
            !is.character(pointShape),
            length(pointShape)  != 1,
            !is.null(pointShape) && !.is_valid_symbolShapes_ggplot(pointShape))) {
        warning("pointShape must be a singular charcter values. Assigning default values 'circle'.")
        pointShape  <- "circle"
    }

    if (any(is.null(pointTransparency),
            !is.numeric(pointTransparency),
            length(pointTransparency) != 1,
            pointTransparency <= 0,
            pointTransparency > 1)) {
        warning("pointTransparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '1'.")
        pointTransparency <- 1
    }

    if (any(is.null(pointSize),
            !is.numeric(pointSize),
            length(pointSize) != 1,
            pointSize < 0)) {
        warning("pointSize must be a singular value of class numeric. Assigning default value '2'.")
        pointSize <- 2
    }

    if (any(is.null(lineLayer),
            !is.logical(lineLayer),
            length(lineLayer) != 1)) {
        warning("lineLayer must be a singular value of class logical. Assigning default value 'FALSE'.")
        lineLayer <- FALSE
    }

    if (!is.null(lineFit)) {
        if (any(is.null(lineColor),
                !is.character(lineColor),
                length(lineColor) != 1)) {
            warning("lineColor must be a singular value of class character. Assigning default value 'dodgerblue4'.")
            lineColor <- "dodgerblue4"
        } else if (.rgbaConversion(lineColor) == "invalid value") {
            warning("Color specified is not valid. Assigning default value 'dodgerblue4'.")
            lineColor <- "dodgerblue4"
        }
    }

    if (!is.null(lineFit) &&
        any(is.null(lineSize),
            !is.numeric(lineSize),
            length(lineSize) != 1,
            lineSize < 0)) {
        warning("lineSize must be a singular value of class numeric. Assigning default value '1'.")
        lineSize <- 1
    }

    if (!is.null(lineFit) &&
        any(length(lineFit) != 1,
            !tolower(lineFit) %in% c('glm', 'lm', 'loess', 'gam'))) {
        warning("lineFit must be one of 'glm', 'lm', 'loess', 'gam' or NULL to disable. Assigning default value 'loess'.")
        lineFit <- "loess"
    }

    if (any(is.null(labelAngle),
            !is.numeric(labelAngle),
            length(labelAngle) != 1,
            labelAngle < 0,
            labelAngle > 90)) {
        warning("labelAngle must be a singular value of class numeric between 0 and 90. Assigning default value '45'.")
        labelAngle <- 45
    }

    #axisFree
    if (any(is.null(axisFree),
            length(axisFree) != 1,
            !is.logical(axisFree))) {
        warning("axisFree must be a singular logical value. Assigning default value TRUE.")
        axisFree <- TRUE
    }

    if (any(is.null(facet),
            !is.logical(facet),
            length(facet) != 1)) {
        warning("facet must be singular logical value. Setting default value 'TRUE'.")
        facet <- TRUE
    }

    if (facet && !missing(facetColname)) {
        if (missing(facetCol)) {
            facetCol <- contrastsDF[[facetColname]] %>% unique %>% length %>% sqrt %>% ceiling
        } else if (any(is.null(facetCol),
                       !is.numeric(facetCol),
                       length(facetCol) != 1,
                       facetCol < 0)) {
            warning("facetCol must be a singular value of class numeric. Assigning default value.")
            facetCol <- contrastsDF[[facetColname]] %>% unique %>% length %>% sqrt %>% ceiling
        }
    }


    if (plotType == "canvasxpress") {
        if (plotCategory == "bar") {
            graphType <- "Bar"
        } else {
            graphType <- "Boxplot"
        }

        minX <- NULL
        maxX <- NULL
        if (is_confidence_used) {
            tidy_data <- contrastsDF %>%
                dplyr::mutate(min = dplyr::case_when(!!rlang::sym(CI.L_colname) < 0 ~ !!rlang::sym(CI.L_colname),
                                                     TRUE ~ 0) - 1,
                              max = dplyr::case_when(!!rlang::sym(CI.R_colname) < 0 ~ 0,
                                                     TRUE ~ !!rlang::sym(CI.R_colname)) + 1) %>%
                tidyr::gather(key = "logType",
                              value = !!rlang::sym(yColname),
                              !!rlang::sym(yColname),
                              !!rlang::sym(CI.L_colname),
                              !!rlang::sym(CI.R_colname))
            minX <- floor(min(tidy_data$min)) - 0.1
            maxX <- ceiling(max(tidy_data$max)) + 0.1

        } else {
            tidy_data <- contrastsDF
        }

        events <- htmlwidgets::JS(
        "{ 'mousemove' : function(o, e, t) {
               if (o != null && o != false &&
                   o.x != null && o.w != null) {
                   var num = o.w.mean;
                   mean = parseFloat(num).toFixed(4);
                   info = '<b>Gene Symbol</b>: ' + o.x.GeneSymbol[0] + '<br/>' +
                          '<b>Contrast:</b>: ' + o.x.Contrast[0] + '<br/>' +
                          '<b>Log FC Mean:</b>: ' + mean + '<br/>';
                          t.showInfoSpan(e, info);
                };
        }}")

        cx_params <- list(groupingFactors         = xColname,
                          graphOrientation        = "vertical",
                          colors                  = barColor,
                          graphType               = graphType,
                          smpTitle                = xlab,
                          smpLableFontStyle       = "bold",
                          smpTitleScaleFontFactor = 1,
                          xAxisTitle              = ylab,
                          showLegend              = FALSE,
                          xAxis2Show              = FALSE,
                          transparency            = barTransparency,
                          events                  = events)

        if (!axisFree && !is.null(minX) && !is.null(maxX)) {
            cx_params <- c(cx_params, list(setMinX = minX, setMaxX = maxX))
        }

        if (graphType == "Boxplot") {
            cx_params <- c(cx_params, list(boxplotType = "range"))
        }

        if (refLine) {
            decorations <- list()
            referenceLine <- .rgbaConversion(refLineColor)
            decorations   <- .getCxPlotDecorations(decorations = decorations,
                                                   color       = referenceLine,
                                                   width       = 1,
                                                   y           = 0)
            cx_params <- c(cx_params, list(decorations = decorations))
        }

        if (facet) {
            numrow   <- (tidy_data[[facetColname]] %>% unique %>% length / facetCol) %>% ceiling
            tidy_data <- tidy_data %>%
                dplyr::arrange(!!rlang::sym(facetColname))
            cx.data <- tidy_data %>%
                dplyr::select(!!rlang::sym(yColname)) %>%
                t() %>%
                as.data.frame()
            smp.data <- tidy_data %>%
                dplyr::select(!!rlang::sym(facetColname),
                              !!rlang::sym(xColname))
            rownames(smp.data) <- colnames(cx.data)
            cx_params <- c(list(data = cx.data,
                                smpAnnot = smp.data,
                                segregateSamplesBy = facetColname,
                                layoutTopology = paste0(numrow, 'X', facetCol),
                                layoutAdjust = axisFree,
                                title = title,
                                smpLabelRotate = labelAngle),
                           cx_params)
            do.call(canvasXpress::canvasXpress, cx_params)
        } else {
            plotlist <- list()
            plotby_vec <- unique(contrastsDF[[facetColname]])
            plotlist <- lapply(plotby_vec, function(x) {
                tidy_data <- tidy_data %>%
                    dplyr::filter(!!rlang::sym(facetColname) == x)
                cx.data <- tidy_data %>%
                    dplyr::select(!!rlang::sym(yColname)) %>%
                    t() %>%
                    as.data.frame()
                smp.data <- tidy_data %>%
                    dplyr::select(!!rlang::sym(facetColname),
                                  !!rlang::sym(xColname))
                rownames(smp.data) <- colnames(cx.data)
                cx_params <- c(list(data = cx.data,
                                    smpAnnot = smp.data,
                                    title = x,
                                    setMinX = floor(min(tidy_data$min)) - 0.1,
                                    setMaxX = ceiling(max(tidy_data$max)) + 0.1,
                                    smpLabelRotate = 90),
                               cx_params)
                do.call(canvasXpress::canvasXpress, cx_params)
            })
            names(plotlist) <- plotby_vec
            plotlist
        }
    } else {
        .addGeoms <- function(myPlot){
            if (plotCategory == "bar") {
                myPlot <- myPlot + geom_bar(stat = "identity",
                                            alpha = barTransparency,
                                            color = barColor,
                                            fill = barColor)
            } else if (plotCategory == "point") {
                myPlot <- myPlot + geom_point(alpha = pointTransparency,
                                              color = pointColor,
                                              fill = pointColor,
                                              size = pointSize,
                                              shape = pointShape)
            }

            # Add error bars if columns present
            if (is_confidence_used) {
                myPlot <- myPlot + geom_errorbar(aes_string(ymin = CI.L_colname, ymax = CI.R_colname), width = .2)
            }

            if (lineLayer) {
                myPlot <- myPlot + geom_smooth(aes_string(group = facetColname),
                                               method = lineFit,
                                               formula = y ~ x,
                                               color = lineColor,
                                               size = lineSize,
                                               se = FALSE)
            }
            myPlot
        }

        if (facet) {
            if (axisFree) {
                axisFree <- "free"
            } else {
                axisFree <- "fixed"
            }
            myPlot <- ggplot2::ggplot(contrastsDF, aes_string(x = xColname, y = yColname))
            myPlot <- .addGeoms(myPlot)
            facetFormula <- stringr::str_c("~", facetColname, sep = " ")
            myPlot <- myPlot + ggplot2::facet_wrap(facetFormula, ncol = facetCol, scales = axisFree)

            myPlot <- myPlot + ggplot2::xlab(xlab)
            myPlot <- myPlot + ggplot2::ylab(ylab)
            if (!is.null(title)) {
                myPlot <- myPlot + ggplot2::ggtitle(title)
            }

            if (labelAngle > 0) {
                myPlot <- myPlot + theme(axis.text.x = element_text(angle = labelAngle, hjust = 1))
            }

            #Add refLine at 0
            if (refLine) {
                myPlot <- myPlot + geom_hline(yintercept = 0, color = refLineColor, size = 0.1)
            }

        } else {# Individual plots for each Gene returned in a list
            plotlist <- list()
            for (obs in unique(contrastsDF[[facetColname]])) { # For each gene
                dat <- contrastsDF[contrastsDF[[facetColname]] == obs, ] # Pull data for one gene
                aplot <- ggplot(dat, aes_string(x = xColname, y = yColname)) + # Samples vs Log2CPM
                    xlab(xlab) +
                    ylab(ylab) +
                    ggtitle(obs) +
                    theme_grey()
                aplot <- .addGeoms(aplot)

                if (!is.null(title)) {
                    aplot <- aplot + ggplot2::ggtitle(stringr::str_c(title, ": ", obs))
                }

                # if (labelAngle > 0) {
                #     aplot <- aplot + theme(axis.text.x = element_text(angle = labelAngle, hjust = 1))
                # }

                if (refLine) {
                    aplot <- aplot + geom_hline(yintercept = 0, color = refLineColor, size = 0.1)
                }
                plotlist[[obs]] <- aplot
            }
            myPlot = plotlist
        }
        myPlot
    }
}
