#' Plot logRatio contrasts
#'
#' Intended to plot a set of contrast results, one plot for each gene of
#' interest.
#'
#' Input is a DGEobj object that must contain  topTable object with at least one contrast dataframe.
#' Also, it requires \strong{logFC}, \strong{CI.L},
#' and \strong{CI.R} columns as well as a gene identifier of choice.
#'
#' Outputs a canvasXpress \emph{(the default)} or ggplot2 object faceted by the facetColname when
#'  \code{facet} parameter is set to \strong{TRUE} \emph{(the default)}.
#'
#' If \code{facet} parameter is set to \strong{FALSE} the output will be a list of individual canvasXpress or
#' ggplots, one for each facetColname value \emph{(typically gene)}.
#'
#' @param dgeObj A DGEobj that contains a contrast or more dataframe.
#' @param plotType Plot type must be canvasXpress or ggplot (Default to canvasXpress).
#' @param facetColname Define the column name to separate plots from geneData (default = "rgd_symbol").
#' @param xColname Define name for the computed column to group boxplots by (Required) (default = "Contrast").
#' @param yColname Define the column name for the output of the boxplots (default = "logFC")
#' @param CI.R_colname Define name of the CI high value (default = "CI.R")
#' @param CI.L_colname Define name of the CI low value (default =  "CI.L")
#' @param plotCategory One of "bar" or "point" (default = "bar")
#' @param refLine Adds a horizontal line at y = 0 (default = TRUE)
#' @param xlab X axis label (defaults to xColname)
#' @param ylab Y axis label (defaults to yColname)
#' @param title Plot title, set to NULL to disable (Optional, default is NULL)
#' @param facet Specifies whether to facet (TRUE) or print individual plots
#'   (FALSE)  (default = TRUE)
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
#'   # DGEobj example, subset DGEobj
#'   t_obj1_subset <- subset(t_obj1, row = c(1:6))
#'
#'
#'   # Simple barplot
#'  logRatioPlot(t_obj1_subset,
#'               facetColname = "rgd_symbol",
#'               xColname = "Contrast")
#'
#'   # Lineplot with some options
#'  logRatioPlot(t_obj1_subset,
#'               plotCategory = "point",
#'               facetColname = "rgd_symbol",
#'               xColname = "Contrast",
#'               axisFree = FALSE,
#'               facet = TRUE,
#'               title = "Test"
#'               labelAngle = 60)
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
logRatioPlot <- function(dgeObj,
                         plotType = "canvasXpress",
                         facetColname = "rgd_symbol",
                         xColname = "Contrast",
                         yColname = "logFC",
                         CI.R_colname = "CI.R",
                         CI.L_colname = "CI.L",
                         plotCategory = "bar",
                         refLine = TRUE,
                         xlab = xColname,
                         ylab = yColname,
                         title = NULL,
                         facet = TRUE,
                         labelAngle = 45,
                         axisFree = TRUE) {
    assertthat::assert_that(!missing(dgeObj),
                            !is.null(dgeObj),
                            "DGEobj" %in% class(dgeObj),
                            msg = "dgeObj must be specified and should be of class DGEobj")

    topTables <- DGEobj::getType(dgeObj, "topTable")
    assertthat::assert_that(length(topTables) > 0,
                            msg = "No topTable dataframes found in dgeObj. Please specify a dgeObj that contains topTable dataframes.")

    assertthat::assert_that(all(sapply(topTables, class) == "data.frame"),
                            msg = "topTables list must only contain dataframes.")

    minNameLen <- min(sapply(names(topTables), nchar))
    assertthat::assert_that(minNameLen > 0,
                            msg = "All dataframes in topTable list must be named (it must be a named list.)")

    geneData <- DGEobj::getType(dgeObj, "geneData")
    assertthat::assert_that(!is.null(geneData),
                            length(geneData) == 1,
                            msg = "There must be exactly one geneData object in dgeObj. Please specify a dgeObj that contains geneData.")
    geneData <- geneData[[1]]
    assertthat::assert_that(!is.null(geneData),
                            nrow(geneData) > 0,
                            msg = "No geneData is found in dgeObj. Please specify a dgeObj that contains geneData.")

    plotType <- tolower(plotType)
    if (any(length(plotType) != 1,
            !plotType %in% c("canvasxpress", "ggplot"))) {
        warning("plotType must be either canvasXpress or ggplot. Setting default value 'canvasXpress'")
        plotType <- "canvasxpress"
    }

    topTables <- lapply(topTables, function(x) {
        x$EnsgID <- rownames(x)
        rownames(x) <- NULL
        x})

    if (any(is.null(xColname),
            length(xColname) != 1)) {
        warning("xColname must be single string value. Setting default value 'Contrast'.")
        xColname <- "Contrast"
    }

    for (name in names(topTables)) {
        topTables[[name]][xColname] <- name
    }

    contrastsDF <- do.call(rbind, topTables)

    if (any(is.null(facetColname),
            length(facetColname) != 1,
            !facetColname %in% colnames(geneData))) {
        if ("rgd_symbol" %in% colnames(geneData)) {
            warning("facetColname must be one of geneData data columns. Setting default value 'rgd_symbol'")
            facetColname <- "rgd_symbol"
        } else{
            assertthat::assert_that(FALSE, msg = "facetColname must be one of geneData data columns.")
        }
    }
    # Add gene symbols from geneData
    genesSymbols <- data.frame("EnsgID" = row.names(geneData), GeneSymbol = geneData[[facetColname]])
    contrastsDF <-  dplyr::left_join(contrastsDF, genesSymbols, by = "EnsgID")
    symCol <- "GeneSymbol"

    if (any(is.null(yColname),
            length(yColname) != 1,
            !yColname %in% colnames(contrastsDF))) {
        if ("logFC" %in% colnames(contrastsDF)) {
            warning("yColname must be one of toptables data columns. Setting default value 'logFC'")
            yColname <- "logFC"
        } else{
            assertthat::assert_that(FALSE, msg = "yColname must be one of toptables data columns.")
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
            warning("CI.R_colname must be one of toptables data columns. Setting default value 'CI.R'")
            CI.R_colname <- "CI.R"
        } else{
            warning("CI.R_colname must be one of toptables data columns. Disabling confidenece limits.")
            is_confidence_used <- FALSE
        }
    }

    if (is_confidence_used &&
        any(is.null(CI.L_colname),
            length(CI.L_colname) != 1,
            !CI.L_colname %in% colnames(contrastsDF))) {
        if ("CI.L" %in% colnames(contrastsDF)) {
            warning("CI.L_colname must be one of toptables data columns. Setting default value 'CI.L'")
            CI.L_colname <- "CI.L"
        } else{
            warning("CI.L_colname must be one of toptables data columns. Disabling confidenece limits.")
            is_confidence_used <- FALSE
        }
    }

    if (any(is.null(refLine),
           !is.logical(refLine),
           length(refLine) != 1)) {
        warning("refLine must be singular logical value. Setting default value 'TRUE'.")
        refLine <- TRUE
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

    if (facet) {
        facetCol <- contrastsDF[[symCol]] %>% unique %>% length %>% sqrt %>% ceiling
    }

    #input validations
    facet_chart_limit <- 40

    if (plotType == "canvasxpress") {
        if (plotCategory == "bar") {
            graphType <- "Bar"
            transparency <- 1
        } else {
            graphType <- "Boxplot"
            transparency <- 1
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
            if (plotCategory == "point") {
                graphType <- "Dotplot"
            }
            minX <- floor(min(tidy_data[[yColname]])) - 0.1
            maxX <- ceiling(max(tidy_data[[yColname]])) + 0.1
            if (minX > 0) {
                minX <-  0
            }

            if (maxX < 0) {
                maxX <- 0
            }
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
                          colors                  = "dodgerblue4",
                          graphType               = graphType,
                          smpTitle                = xlab,
                          smpLableFontStyle       = "bold",
                          smpTitleScaleFontFactor = 1,
                          xAxisTitle              = ylab,
                          showLegend              = FALSE,
                          xAxis2Show              = FALSE,
                          transparency            = transparency,
                          events                  = events)

        if (!axisFree && !is.null(minX) && !is.null(maxX)) {
            cx_params <- c(cx_params, list(setMinX = minX, setMaxX = maxX))
        }

        if (graphType == "Boxplot") {
            cx_params <- c(cx_params, list(boxplotType = "range"))
        }

        if (refLine) {
            decorations <- list()
            referenceLine <- .rgbaConversion("red")
            decorations   <- .getCxPlotDecorations(decorations = decorations,
                                                   color       = referenceLine,
                                                   width       = 1,
                                                   y           = 0)
            cx_params <- c(cx_params, list(decorations = decorations))
        }

        if (facet) {
            plots_num <- tidy_data[[symCol]] %>% unique %>% length
            numrow   <- (plots_num / facetCol) %>% ceiling
            if (plots_num > facet_chart_limit) {
                warning(paste("A large number of charts/facets has/have been requested",
                              "and may take significant time to generate. It is suggested that less than",
                              facet_chart_limit,
                              "charts/facets are requested at a time."))
            }
            tidy_data <- tidy_data %>%
                dplyr::arrange(!!rlang::sym(symCol))
            cx.data <- tidy_data %>%
                dplyr::select(!!rlang::sym(yColname)) %>%
                t() %>%
                as.data.frame()
            smp.data <- tidy_data %>%
                dplyr::select(!!rlang::sym(symCol),
                              !!rlang::sym(xColname))
            rownames(smp.data) <- colnames(cx.data)
            cx_params <- c(list(data = cx.data,
                                smpAnnot = smp.data,
                                segregateSamplesBy = symCol,
                                layoutTopology = paste0(numrow, 'X', facetCol),
                                layoutAdjust = axisFree,
                                title = title,
                                smpLabelRotate = labelAngle),
                           cx_params)
            do.call(canvasXpress::canvasXpress, cx_params)
        } else {
            plotlist <- list()
            plotby_vec <- unique(contrastsDF[[symCol]])
            plotlist <- lapply(plotby_vec, function(x) {
                tidy_data <- tidy_data %>%
                    dplyr::filter(!!rlang::sym(symCol) == x)
                cx.data <- tidy_data %>%
                    dplyr::select(!!rlang::sym(yColname)) %>%
                    t() %>%
                    as.data.frame()
                smp.data <- tidy_data %>%
                    dplyr::select(!!rlang::sym(symCol),
                                  !!rlang::sym(xColname))
                rownames(smp.data) <- colnames(cx.data)
                if (is_confidence_used) {
                    minX <- floor(min(tidy_data$min)) - 0.1
                    maxX <- ceiling(max(tidy_data$max)) + 0.1
                } else {
                    minX <- floor(min(tidy_data[[yColname]])) - 0.1
                    maxX <- ceiling(max(tidy_data[[yColname]])) + 0.1
                    if (minX > 0) {
                        minX <-  0
                    }

                    if (maxX < 0) {
                        maxX <- 0
                    }
                }
                cx_params <- c(list(data = cx.data,
                                    smpAnnot = smp.data,
                                    title = x,
                                    setMinX = minX,
                                    setMaxX = maxX,
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
                                            alpha = 1,
                                            color = "dodgerblue4",
                                            fill = "dodgerblue4")
            } else if (plotCategory == "point") {
                myPlot <- myPlot + geom_point(alpha = 1,
                                              color = "dodgerblue4",
                                              fill = "dodgerblue4",
                                              size = 2,
                                              shape = "circle")
            }

            # Add error bars if columns present
            if (is_confidence_used) {
                myPlot <- myPlot + geom_errorbar(aes_string(ymin = CI.L_colname, ymax = CI.R_colname), width = .2)
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
            facetFormula <- stringr::str_c("~", symCol, sep = " ")
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
                myPlot <- myPlot + geom_hline(yintercept = 0, color = "red", size = 0.1)
            }

        } else {# Individual plots for each Gene returned in a list
            plotlist <- list()
            for (obs in unique(contrastsDF[[symCol]])) { # For each gene
                dat <- contrastsDF[contrastsDF[[symCol]] == obs, ] # Pull data for one gene
                aplot <- ggplot(dat, aes_string(x = xColname, y = yColname)) + # Samples vs Log2CPM
                    xlab(xlab) +
                    ylab(ylab) +
                    ggtitle(obs) +
                    theme_grey()
                aplot <- .addGeoms(aplot)

                if (!is.null(title)) {
                    aplot <- aplot + ggplot2::ggtitle(stringr::str_c(title, ": ", obs))
                }

                if (refLine) {
                    aplot <- aplot + geom_hline(yintercept = 0, color = "red", size = 0.1)
                }
                plotlist[[obs]] <- aplot
            }
            myPlot = plotlist
        }

        myPlot
    }
}
