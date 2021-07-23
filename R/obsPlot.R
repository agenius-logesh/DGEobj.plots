#' obsPlot: Plots a summary (box/violin) plot the for given gene
#'
#' Provides a summary plot (box/violin) for each observation (gene), showing data for each
#' experiment group.
#'
#' The plot can optionally include boxplot, violinplot or both and can additionally choose to display points on the plot.
#'
#' By default, the violinLayer is not displayed and only the boxplot, points and the mean on the plots can be seen.
#' Also, by default, the plots are faceted.
#'
#' Faceting the plot can be turned off to return a list of individual plots for each gene. Input is a DGEobj with a
#' Counts Matrix. User can also provide input parameters to convert the counts matrix to other desired units.
#'
#' @param data A  DGEObject. The countsMatrix in the DGEObject is extracted to plot the data. (required)
#' @param plotType Can be canvasXpress or ggplot (default = canvasXpress)
#' @param designTable Name of the design table in the DGEObj from which the grouping column will be extracted. (default = design)
#' @param countsMatrix Name of the counts matrix in the DGEObj which will be used to render the plot.(default = counts)
#' @param convertCounts A flag to indicate if counts matrix need to be converted or taken as is. Default value is NULL. This indicates
#'     countsMatrix need to be taken as is. To convert the counts matrix, specify the desired unit. Supported units include CPM,FPKM, FPK and TPM.
#'     This parameter is passed to DGEobj.utils::convertCounts
#' @param convert_geneLength Parameter to pass to DGEobj.utils::convertCounts.
#' @param convert_log Parameter to pass to DGEobj.utils::convertCounts. (default = FALSE)
#' @param convert_normalize Parameter to pass to DGEobj.utils::convertCounts. (default = none)
#' @param convert_prior.count Parameter to pass to DGEobj.utils::convertCounts. (default = NULL)
#' @seealso \link[DGEobj.utils]{convertCounts}
#' @param group Define the column name to group boxplots by (typically a replicate group column) (required)
#' @param violinLayer Adds a violin layer (default = FALSE)
#' @param showPoints Shows the datapoints on the plot (default = TRUE)
#' @param xlab X axis label (defaults to group column name if not specified)
#' @param ylab Y axis label (defaults to value column name if not specified)
#' @param title Plot title (optional)
#' @param facet Specifies whether to facet (TRUE) or print individual plots
#'   (FALSE)  (default = TRUE)
#' @param axisFree Specify same scale or independent scales for each subplot (default = TRUE;
#'   Allowed values: TRUE or FALSE)
#'
#' @return Plot of type canvasXpress or ggplot. If Facet = TRUE (default) returns a faceted object. If
#'   facet = FALSE, returns a list of objects indexed
#'   by observation (gene) names.
#'
#' @examples
#' \dontrun{
#'
#'   # Faceted boxplot
#'   obsPlot(DGEobj,
#'           designTable = "design",
#'           group = "replicategroup",
#'           countsMatrix = "counts")
#'
#'   # Faceted violin plot
#'   obsPlot(DGEobj,
#'            violinLayer = TRUE,
#'            designTable = "design",
#'            group = "replicategroup",
#'            countsMatrix = "counts")
#'
#'   # Return a list of plot for each individual gene
#'   myplots <- obsPlot(DGEobj,
#'                      designTable = "design",
#'                      group = "replicategroup",
#'                      countsMatrix = "counts",
#'                      facet = FALSE)
#'   # Plot one from the list
#'   myplots[[2]]
#'
#'   #ggplot
#'   obsPlot(DGEobj,
#'           designTable = "design",
#'           group = "replicategroup",
#'           countsMatrix = "counts",
#'           plotType = "ggplot")
#' }
#'
#' @import ggplot2 magrittr
#' @importFrom tidyr gather
#' @importFrom dplyr left_join filter select count
#' @importFrom assertthat assert_that
#' @importFrom canvasXpress canvasXpress
#' @importFrom stringr str_c
#' @importFrom rlang sym
#' @importFrom DGEobj getItem getType
#' @importFrom DGEobj.utils convertCounts
#'
#' @export
obsPlot <- function(dgeObj,
                    plotType            = "canvasXpress",
                    countsMatrix        = "counts",
                    convertCounts       = NULL,
                    convert_geneLength,
                    convert_log         = FALSE,
                    convert_normalize   = "none",
                    convert_prior.count = NULL,
                    designTable         = "design",
                    group               = "ReplicateGroup",
                    violinLayer         = FALSE,
                    showPoints          = TRUE,
                    xlab,
                    ylab,
                    title,
                    facet               = TRUE,
                    axisFree            = TRUE) {

    assertthat::assert_that(!missing(dgeObj),
                            !is.null(dgeObj),
                            "DGEobj" %in% class(dgeObj),
                            msg = "dgeObj must be specified and must belong to DGEobj class.")

    assertthat::assert_that(!is.null(DGEobj::getType(dgeObj,"counts")),
                            msg = "counts matrix must be available in dgeObj to plot the data.")

    assertthat::assert_that(!is.null(DGEobj::getType(dgeObj,"design")),
                            msg = "design table must be available in dgeObj to plot the data.")

    #plotType
    plotType <- tolower(plotType)
    if (any(is.null(plotType),
            !is.character(plotType),
            length(plotType) != 1,
            !plotType %in% c("canvasxpress", "ggplot"))) {
        warning("plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'.")
        plotType <- "canvasxpress"
    }

    #countsMatrix
    if (any(is.null(countsMatrix),
            !is.character(countsMatrix),
            !(countsMatrix %in% names(dgeObj)))) {
        if ("counts" %in% names(dgeObj)) {
            countsMatrix <- "counts"
        } else if (!is.null(DGEobj::getType(dgeObj,"counts"))) {
            countsMatrix <- names(DGEobj::getType(dgeObj,"counts"))
        }
        warning(paste0("countsMatrix specified is not present in DGEobj. Assigning default value '", countsMatrix,"'."))
    }

    #convertCounts
    if (!is.null(convertCounts) &&
        any(!is.character(convertCounts),
            length(convertCounts) != 1,
            !(toupper(convertCounts) %in% c("CPM", "FDPKM", "FPK", "TPM")))) {
        warning("Invalid value specificed for convertCounts. It must be null if counts matrix need not be converted or must be one of CPM, FPKM, FPK, and TPM. Assigning default value 'NULL'.")
        convertCounts <- NULL
    }

    if (!is.null(convertCounts)) {
        if (any(length(convert_log) != 1,
                is.null(convert_log),
                !is.logical(convert_log))) {
            warning("Invalid value specified for convert_log. Assigning default value FALSE.")
            convert_log <- FALSE
        }

        if (any(length(convert_normalize) != 1,
                is.null(convert_normalize),
                !is.character(convert_normalize),
                !(toupper(convert_normalize) %in% c("TMM", "RLE", "UPPERQUARTILE", "TMMWZP", "NONE")))) {
            warning("Invalid value specified for convert_normalize. Must be one of 'TMM', 'RLE', 'upperquartile', 'TMMwzp' or 'none'. Assigning default value 'none'.")
            convert_normalize <- "none"
        }

        if (!missing(convert_geneLength) && convertCounts != "CPM") {
            assertthat::assert_that(length(convert_geneLength) == nrow(DGEobj::getItem(dgeObj, countsMatrix)),
                                    msg = "geneLength must be the same length of the number of rows in countsMatrix.")
        }

        if (!is.null(convert_prior.count) &&
            any(length(convert_prior.count) != 1,
                !is.numeric(convert_prior.count))) {
            warning("Invalid value specified for convert_prior.count Assigning default value NULL.")
            convert_prior.count <- NULL
        }

        data <- DGEobj.utils::convertCounts(DGEobj::getItem(dgeObj, countsMatrix),
                                      unit        = convertCounts,
                                      geneLength  = convert_geneLength,
                                      log         = convert_log,
                                      normalize   = convert_normalize,
                                      prior.count = convert_prior.count) %>%
            as.data.frame()
    } else {
        data <- DGEobj::getItem(dgeObj, countsMatrix) %>%
            as.data.frame()
    }

        if (any(is.null(designTable),
                !is.character(designTable),
                length(designTable) != 1,
                !(designTable %in% names(dgeObj)))) {
            if ("design" %in% names(dgeObj)) {
                designTable <- "design"
            } else if (!is.null(DGEobj::getType(dgeObj,"design"))) {
                designTable <- names(DGEobj::getType(dgeObj,"design"))[[1]]
            }
            warning(paste0("designTable specified is not present in DGEobj. Assigning default value '", designTable,"'."))
        }

    design            <- DGEobj::getItem(dgeObj, designTable)
    colnames(design)  <- tolower(colnames(design))
    group_default     <- NULL
    if ("replicategroup" %in% colnames(design)) {
        group_default <- "replicategroup"
    }

    if (any(is.null(group),
            !is.character(group),
            length(group) != 1,
            !(tolower(group) %in% colnames(design)))) {
        if (!is.null(group_default)) {
            warning("group must be specified and should be one of the columns in the design object in dgeObj. Assigning ",
                    group_default,
                    " as the default value.")
            group <- group_default
        } else {
            stop("group must be specified and should be one of the columns in the designTable in dgeObj.")
        }
    }

    # Create a rownames column
    data$GeneID    <- rownames(data)
    rownames(data) <- NULL
    data           <- tidyr::gather(data, key = "sample", value = "value",-GeneID)
    group          <- tolower(group)
    group.data     <- design %>% dplyr::select(!!group)
    colnames(group.data) <- "group"
    group.data$sample    <- rownames(group.data);rownames(group.data) <- NULL
    data <- dplyr::left_join(data, group.data, by = "sample" )

    plotByCol <- "GeneID"
    valueCol  <- "value"
    groupCol  <- "group"

    #input validations
    facet_chart_limit <- 40

    if (any(is.null(facet),
            !is.logical(facet),
            length(facet) != 1)) {
        warning("facet must be a singular logical value. Assigning default value TRUE.")
        facet <- TRUE
    }

    if (facet) {
        #Number of charts to plot
        if (nrow(unique(data[plotByCol])) > facet_chart_limit) {
            warning(paste("A large number of charts/facets has/have been requested and may take significant time to generate.  It is suggested that less than",
                          facet_chart_limit,
                          "charts/facets are requested at a time."))
        }

        #facetRow
        numrow <- data[plotByCol] %>%
            unique %>%
            dplyr::count() %>%
            sqrt %>%
            ceiling %>%
            as.integer
    }

    if (missing(title)) {
        title <- NULL
    } else if ((!is.null(title)) &&
               (!is.character(title) || length(title) != 1)) {
        warning("Invalid title specificed. Title must be singular value of class character.")
        title <- NULL
    }

    if ((!missing(xlab)) &&
        (!is.null(xlab)) &&
        ((!is.character(xlab)) || length(xlab) != 1)) {
        warning("xlab value specified is not valid. Assigning groupCol name as the default value.")
        xlab <- groupCol
    } else if (missing(xlab)) {
        xlab <- groupCol
    }

    if (!missing(ylab) &&
        !is.null(ylab) &&
        (!is.character(ylab) || length(ylab) != 1)) {
        warning("ylab value specified is not valid. Assigning valueCol name as the default value.")
        ylab <- valueCol
    } else if (missing(ylab)) {
        ylab <- valueCol
    }

    if (any(is.null(violinLayer),
            length(violinLayer) != 1,
            !is.logical(violinLayer))) {
        warning("violinLayer must be a singular logical value. Assigning default value FALSE.")
        violinLayer <- FALSE
    }

    if (violinLayer) {
        violinColor <- "deepskyblue3"
        boxColor <- NA
    } else {
        violinColor <- NA
        boxColor <- "deepskyblue3"
    }

    #point validations
    if (any(is.null(showPoints),
            length(showPoints) != 1,
            !is.logical(showPoints))) {
        warning("showPoints must be a singular logical value. Assigning default value TRUE.")
        showPoints <- TRUE
    }

    #axisFree
    if (any(is.null(axisFree),
            length(axisFree) != 1,
            !is.logical(axisFree))) {
        warning("axisFree must be a singular logical value. Assigning default value TRUE.")
        axisFree <- TRUE
    }

    obsPlot <- NULL
    if (plotType == "canvasxpress") {
        if (facet) {
            numcol   <- ((data[[plotByCol]] %>% unique %>% length) / numrow) %>% ceiling
            cx_data  <- data %>% dplyr::select(!!rlang::sym(valueCol)) %>% t() %>% as.data.frame()
            smp_data <- data %>% dplyr::select(-!!rlang::sym(valueCol))
            rownames(smp_data) <- colnames(cx_data)
            obsPlot  <- canvasXpress::canvasXpress(data               = cx_data,
                                                   smpAnnot            = smp_data,
                                                   graphOrientation    = "vertical",
                                                   graphType           = "Boxplot",
                                                   groupingFactors     = groupCol,
                                                   boxplotColor        = boxColor,
                                                   boxplotMean         = TRUE,
                                                   boxplotWhiskersType = "single",
                                                   showViolinBoxplot   = violinLayer,
                                                   showBoxplotIfViolin = TRUE,
                                                   violinColor         = violinColor,
                                                   smpLabelRotate      = 30,
                                                   smpLabelScaleFontFactor = 0.5,
                                                   smpTitle            = xlab,
                                                   layoutAdjust        = axisFree,
                                                   showBoxplotOriginalData = showPoints,
                                                   theme               = "CanvasXpress",
                                                   xAxisTitle          = ylab,
                                                   xAxis2Show          = FALSE,
                                                   title               = title,
                                                   showLegend          = FALSE,
                                                   layoutTopology      = paste0(numrow, "X", numcol),
                                                   segregateSamplesBy  = plotByCol,
                                                   afterRender         = list(list('sortSamplesByCategory', list("group"))))
        } else {
            plotlist   <- list()
            plotby_vec <- unique(data[[plotByCol]])
            obsPlot    <- lapply(plotby_vec, function(x) {
                data_subset <- data %>%
                    dplyr::filter(!!rlang::sym(plotByCol) == x)
                cx_data <- data_subset %>%
                    dplyr::select(!!rlang::sym(valueCol)) %>%
                    t() %>%
                    as.data.frame()
                smp_data <- data_subset %>%
                    dplyr::select(-!!rlang::sym(valueCol))
                rownames(smp_data) <- colnames(cx_data)
                title <- x

                canvasXpress::canvasXpress(data                = cx_data,
                                           smpAnnot            = smp_data,
                                           graphOrientation    = "vertical",
                                           graphType           = "Boxplot",
                                           groupingFactors     = groupCol,
                                           boxplotColor        = boxColor,
                                           boxplotMean         = TRUE,
                                           boxplotWhiskersType = "single",
                                           showViolinBoxplot   = violinLayer,
                                           showBoxplotIfViolin = TRUE,
                                           violinColor         = boxColor,
                                           smpLabelRotate      = 30,
                                           smpLabelScaleFontFactor = 0.5,
                                           smpTitle            = xlab,
                                           layoutAdjust        = axisFree,
                                           showBoxplotOriginalData = showPoints,
                                           theme               = "CanvasXpress",
                                           xAxisTitle          = ylab,
                                           title               = title,
                                           xAxis2Show          = FALSE,
                                           showLegend          = FALSE,
                                           afterRender         = list(list('sortSamplesByCategory', list("group"))))
            })
        }
    } else {
        .addGeoms <- function(obsPlot) {
            obsPlot <- obsPlot + geom_boxplot(
                alpha = 0.5,
                color = "black",
                fill  = boxColor,
                outlier.shape = outlier.shape,
                outlier.size  = outlier.size
            ) + stat_summary(fun   = mean,
                             geom  = "point",
                             shape = "square",
                             size  = 3,
                             color = "goldenrod1",
                             fill  = "red2",
                             alpha = 0.7)

            if (violinLayer) {
                obsPlot <- obsPlot + geom_violin(alpha = 0.5,
                                                 color = "black",
                                                 fill  = violinColor)
            }

            if (showPoints) {
                    obsPlot <-
                        obsPlot +  geom_point(position = position_jitter(width = 0.1),
                                                       alpha    = 0.5,
                                                       color    = "grey30",
                                                       fill     = "dodgerblue4",
                                                       size     = 2,
                                                       shape    = "circle")
            }

            obsPlot
        }

        # Reduce box outliers to a dot if geom_points turned on
        outlier.size  <- 1.5
        outlier.shape <- 19
        if (showPoints) {
            outlier.size  <- 1
            outlier.shape <- "."
        }

        if (axisFree) {
            axisFree <- "free"
        } else {
            axisFree <- "fixed"
        }

        # Plot code here
        if (facet) {
            obsPlot <- ggplot(data,  aes_string(x = groupCol, y = valueCol))
            obsPlot <- .addGeoms(obsPlot) +  theme(axis.text.x =  element_text(angle = 30, hjust = 1))
            facetFormula <- stringr::str_c("~", plotByCol, sep = " ")
            obsPlot <- obsPlot + facet_wrap(facetFormula, nrow = numrow, scales = axisFree)
            obsPlot <- obsPlot + xlab(xlab)
            obsPlot <- obsPlot + ylab(ylab)
            if (!missing(title)) {
                obsPlot <- obsPlot + ggtitle(title)
            }
        } else {
            plotlist <- list()
            for (obs in unique(data[[plotByCol]])) {
                dat   <- data[data[[plotByCol]] == obs, ]
                aplot <-  ggplot(dat,  aes_string(x = groupCol, y = valueCol)) +
                    xlab(xlab) +
                    ylab(ylab) +
                    ggtitle(obs)
                aplot <- .addGeoms(aplot) +  theme(axis.text.x =  element_text(angle = 30, hjust = 1))
                plotlist[[obs]] <- aplot
            }
            obsPlot <- plotlist
        }
    }
    obsPlot
}
