context("DGEobj.plots - tests for ggplotMDS.R functions")


test_that("obsPlot.R: obsPlot()", {
    skip_if(!all(c("design","counts") %in% names(t_obj1)))
    skip_if(is.null(t_obj1$design$ReplicateGroup))

    #subset t_obj1
    t_obj1_subset <- subset(t_obj1, row = c(1:6))

    obs_plot <- obsPlot(t_obj1_subset)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #facet = FALSE
    obs_plot <- obsPlot(t_obj1_subset,
                        facet = FALSE)
    expect_length(obs_plot, length(unique(rownames(t_obj1_subset))))
    expect_s3_class(obs_plot[[1]], c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1_subset,
                        plotType = "ggplot")
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    obs_plot <- obsPlot(t_obj1_subset,
                        facet = FALSE,
                        plotType = "ggplot")
    expect_length(obs_plot, length(unique(rownames(t_obj1_subset))))
    expect_s3_class(obs_plot[[1]], c("gg", "ggplot"))

    genelength <- getItem(t_obj1_subset, "geneData")$ExonLength
    obs_plot <- obsPlot(t_obj1_subset,
                        designTable   = "design",
                        group         = "replicategroup",
                        countsMatrix  = "counts",
                        convertCounts = "TPM",
                        convert_geneLength = genelength,
                        convert_log   = FALSE,
                        convert_normalize   = "none",
                        convert_prior.count = NULL,
                        violinLayer = TRUE,
                        showPoints  = TRUE,
                        xlab        = "value",
                        ylab        = "replicate_group",
                        title       = "obsPlot",
                        facet       = TRUE,
                        axisFree    = TRUE)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1_subset,
                        designTable   = "design",
                        group         = "replicategroup",
                        countsMatrix  = "counts",
                        convertCounts = "CPM",
                        convert_log   = TRUE,
                        convert_normalize = "none",
                        convert_prior.count = 0.5,
                        violinLayer   = TRUE,
                        showPoints    = FALSE)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1_subset,
                        designTable = "design",
                        group       = "replicategroup",
                        violinLayer = TRUE)

    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1_subset,
                        plotType    = "ggplot",
                        designTable = "design",
                        group       = "replicategroup",
                        violinLayer = TRUE,
                        showPoints  = FALSE)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    obs_plot <- obsPlot(t_obj1_subset,
                        plotType    = "ggplot",
                        group       = "replicategroup",
                        designTable = "design",
                        violinLayer = TRUE)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    obs_plot <- obsPlot(t_obj1_subset,
                        plotType    = "ggplot",
                        designTable = "design",
                        group       = "replicategroup",
                        violinLayer = TRUE,
                        showPoints  = FALSE)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    obs_plot <- obsPlot(t_obj1_subset,
                        designTable   = "design",
                        plotType      = "ggplot",
                        group         = "replicategroup",
                        countsMatrix  = "counts",
                        convertCounts = "TPM",
                        convert_geneLength = genelength,
                        convert_log   = FALSE,
                        convert_normalize = "none",
                        convert_prior.count = NULL,
                        violinLayer   = TRUE,
                        showPoints    = TRUE,
                        xlab          = "value",
                        ylab          = "replicate_group",
                        title         = "obsPlot",
                        facet         = TRUE,
                        axisFree      = FALSE)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    obs_plot <- obsPlot(t_obj1_subset,
                        plotType      = "ggplot",
                        designTable   = "design",
                        group         = "replicategroup",
                        countsMatrix  = "counts",
                        convertCounts = "CPM",
                        convert_log   = TRUE,
                        convert_normalize = "upperquartile",
                        convert_prior.count = 0.5,
                        violinLayer   = FALSE,
                        showPoints    = TRUE)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    # testing assert statements
    msg <- "dgeObj must be specified and must belong to DGEobj class."
    expect_error(obsPlot(dgeObj = 1:10),
                 regexp = msg)
    expect_error(obsPlot(),
                 regexp = msg)

    obj_no_counts <- rmItem(t_obj1_subset, "counts")
    expect_error(suppressWarnings(obs_plot <- obsPlot(obj_no_counts,
                                     designTable = "design",
                                     group       = "replicategroup")),
                 regexp = "counts matrix must be available in dgeObj to plot the data.")

    expect_warning(obs_plot <- obsPlot(t_obj1),
                   regexp = "A large number of charts/facets has/have been requested and may take significant time to generate.  It is suggested that less than 40 charts/facets are requested at a time.")
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    msg <- "group must be specified and should be one of the columns in the design object in dgeObj. Assigning replicategroup as the default value."
    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       group = "notavalidname"),
                 regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       group = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       group = c("notavalidname", "invalidname")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obj_no_rep_group <- t_obj1_subset
    obj_no_rep_group$design$ReplicateGroup <- NULL
    expect_error(obsPlot(obj_no_rep_group,
                         group = "notavalidgroup"),
                 regexp = "group must be specified and should be one of the columns in the designTable in dgeObj.")

    obj_no_design <- rmItem(t_obj1_subset, "design")
    expect_error(suppressWarnings(obsPlot(obj_no_design,
                         designTable = "design",
                         group       = "replicategroup")),
                 regexp = "design table must be available in dgeObj to plot the data.")

    msg <- "designTable specified is not present in DGEobj. Assigning default value 'design'."
    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       designTable = "notavalidname",
                                       group       = "replicategroup"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       designTable = 1,
                                       group       = "replicategroup"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       designTable = c("invalidname", "notavalidname"),
                                       group       = "replicategroup"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    design_data <- getItem(t_obj1_subset, "design")
    t_obj1_updated_design <- rmItem(t_obj1_subset,"design")
    t_obj1_updated_design <- addItem(t_obj1_updated_design, item = design_data, itemName = "design_new", itemType = "design")
    expect_warning(obs_plot <- obsPlot(t_obj1_updated_design,
                                       designTable = c("invalidname", "notavalidname"),
                                       group       = "replicategroup"),
                   regexp = "designTable specified is not present in DGEobj. Assigning default value 'design_new'.")
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obj_no_rep_group <- t_obj1_subset
    obj_no_rep_group$design$ReplicateGroup <- NULL
    expect_error(obsPlot(obj_no_rep_group),
                 regexp = "group must be specified and should be one of the columns in the designTable in dgeObj.")

    #Testing optional parameters
    #plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'."
    expect_warning(obs_plot <- obsPlot(dgeObj  = t_obj1_subset,
                                       plotType = "cx"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(dgeObj   = t_obj1_subset,
                                        plotType = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(dgeObj  = t_obj1_subset,
                                       plotType = c("canvasXpress", "ggplot")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))


    #countsMatrix
    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       countsMatrix = "notavalidname"),
                   regexp = "countsMatrix specified is not present in DGEobj. Assigning default value 'counts'.")

    #convertCounts
    msg <- "Invalid value specificed for convertCounts. It must be null if counts matrix need not be converted or must be one of CPM, FPKM, FPK, and TPM. Assigning default value 'NULL'."
    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       convertCounts = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       convertCounts = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       convertCounts = c("CPM", "TPM")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #convert_normalize
    msg <- "Invalid value specified for convert_normalize. Must be one of 'TMM', 'RLE', 'upperquartile', 'TMMwzp' or 'none'. Assigning default value 'none'."
    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       convertCounts     = "CPM",
                                       convert_normalize = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       convertCounts     = "CPM",
                                       convert_normalize = c('TMM', 'RLE')),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       convertCounts     = "CPM",
                                       convert_normalize = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #convert_log
    msg <- "Invalid value specified for convert_log. Assigning default value FALSE."
    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       convertCounts = "CPM",
                                       convert_log   = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       convertCounts = "CPM",
                                       convert_log   = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       convertCounts = "CPM",
                                       convert_log   = c(TRUE, FALSE)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       convertCounts = "CPM",
                                       convert_log   = c(TRUE, FALSE)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #convert_genelength
    expect_error(obsPlot(t_obj1_subset,
                         convertCounts = "TPM",
                         convert_geneLength = c(1,2)),
                 regexp = "geneLength must be the same length of the number of rows in countsMatrix.")
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #convert_prior.count
    msg <- "Invalid value specified for convert_prior.count Assigning default value NULL."
    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       convertCounts = "CPM",
                                       convert_prior.count = "a"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       convertCounts = "CPM",
                                      convert_prior.count = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #group


    #designTable

    #facet
    msg <- "facet must be a singular logical value. Assigning default value TRUE."
    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       facet = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       facet = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       facet = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #title
    msg <- "Invalid title specificed. Title must be singular value of class character."
    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       title = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       title = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1_subset,
                        title = NULL)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1_subset,
                        plotType = "ggplot",
                        title    = NULL)
    expect_s3_class(obs_plot, c("gg", "ggplot"))


    #xlab
    msg <- "xlab value specified is not valid. Assigning groupCol name as the default value."
    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       xlab  = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       xlab  = c("xlab1", "xlab2")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1_subset,
                        xlab  = NULL)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1_subset,
                        plotType = "ggplot",
                        xlab     = NULL)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    #ylab
    msg <- "ylab value specified is not valid. Assigning valueCol name as the default value."
    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       ylab  = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       ylab  = c("ylab1", "ylab2")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1_subset,
                        ylab  = NULL)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1_subset,
                        plotType = "ggplot",
                        ylab     = NULL)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    #violinLayer
    msg <- "violinLayer must be a singular logical value. Assigning default value FALSE."
    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       violinLayer = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       violinLayer = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       violinLayer = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #showPoints
    msg <- "showPoints must be a singular logical value. Assigning default value TRUE."
    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       showPoints = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       showPoints = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       showPoints = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #axisFree
    msg <- "axisFree must be a singular logical value. Assigning default value TRUE."
    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       axisFree   = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       axisFree   = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1_subset,
                                       axisFree   = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

})
