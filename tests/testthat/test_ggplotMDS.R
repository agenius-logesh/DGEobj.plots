context("DGEobj.plots - tests for ggplotMDS.R functions")


test_that("ggplotMDS.R: ggplotMDS()", {
    mds_plot <- ggplotMDS(dgeObj = t_obj1)
    expect_length(mds_plot, 2)
    expect_named(mds_plot, c("plot", "mdsobj"))
    expect_type(mds_plot, "list")
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    mds_plot <- ggplotMDS(dgeObj = t_obj1,
                          plotType = "ggplot")
    expect_length(mds_plot, 2)
    expect_named(mds_plot, c("plot", "mdsobj"))
    expect_type(mds_plot, "list")
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    #Testing optional parameters
    mds_plot <- ggplotMDS(dgeObj        = t_obj1,
                          designTable    = "design",
                          colorBy        = "ReplicateGroup",
                          shapeBy        = "ReplicateGroup",
                          sizeBy         = "ReplicateGroup",
                          labels         = "ReplicateGroup",
                          hlineIntercept = c(1,0.25),
                          vlineIntercept = c(1,0.25),
                          top            = 10,
                          title          = "MDS",
                          xlab           = "xaxis",
                          ylab           = "yaxis")
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    mds_plot <- ggplotMDS(dgeObj        = t_obj1,
                          shapeBy        = "ReplicateGroup",
                          plotType       = "ggplot",
                          sizeBy         = "ReplicateGroup",
                          hlineIntercept = 0.25,
                          vlineIntercept = 0.25,
                          top            = 2,
                          labels         = NULL,
                          title          = "MDS",
                          xlab           = "xaxis",
                          ylab           = "yaxis")
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    mds_plot <- ggplotMDS(dgeObj     = t_obj1,
                          designTable = "design",
                          colorBy     =  "ReplicateGroup",
                          plotType    = "ggplot")
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    mds_plot <- ggplotMDS(dgeObj     = t_obj1,
                          designTable = "design",
                          colorBy     =  "ReplicateGroup",
                          shapeBy     =  "ReplicateGroup",
                          plotType    = "ggplot")
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    mds_plot <- ggplotMDS(dgeObj     = t_obj1,
                          designTable = "design",
                          colorBy     =  "ReplicateGroup",
                          sizeBy      = "ReplicateGroup",
                          plotType    = "ggplot")
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    mds_plot <- ggplotMDS(dgeObj     = t_obj1,
                          designTable = "design",
                          colorBy     =  "ReplicateGroup",
                          plotType    = "ggplot")
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    # testing assert statements
    msg <- "dgeObj must be specified and must belong to DGEobj class."
    expect_error(ggplotMDS(dgeObj = 1:10),
                 regexp = msg)
    expect_error(ggplotMDS(dgeObj = NULL),
                 regexp = msg)
    expect_error(ggplotMDS(),
                 regexp = msg)

    #Testing optional parameters

    #plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'."
    expect_warning(ggplotMDS(dgeObj = t_obj1,
                           plotType  = "cx"),
                 regexp = msg)
    expect_warning(ggplotMDS(dgeObj = t_obj1,
                           plotType  = NULL),
                 regexp = msg)
    #designTable
    msg <- "designTable is either missing or invalid. Assigning default object of type 'design'."
    expect_warning(mds_plot <- ggplotMDS(dgeObj     = t_obj1,
                                         designTable = "not a valid value"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(dgeObj     = t_obj1,
                                         designTable = 1),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(dgeObj     = t_obj1,
                                         designTable = c("abc", "def")),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(dgeObj     = t_obj1,
                                         designTable = NULL,
                                         colorBy     = NULL),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    #colorBy
    msg <- "colorBy value specified is invalid or missing. Assigning default value 'ReplicateGroup'."
    expect_warning(mds_plot <- ggplotMDS(dgeObj = t_obj1,
                                         colorBy = c(1,2)),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    missing_repgroup <- t_obj1
    missing_repgroup[["design"]][["ReplicateGroup"]] <- NULL
    expect_warning(mds_plot <- ggplotMDS(dgeObj = missing_repgroup,
                                         colorBy = "ReplicateGroup"),
                   regexp = "colorBy value specified is invalid or missing. Assigning default value 'NULL'.")
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    #shapeBy
    msg <- "shapeBy should be a column in the design attribute of dgeObj. Assigning NULL as default value."
    expect_warning(mds_plot <- ggplotMDS(dgeObj = t_obj1,
                                         shapeBy = "Replicate"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(dgeObj = t_obj1,
                                         shapeBy = "1"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(dgeObj = t_obj1,
                                         shapeBy = c("value1", "value2")),
                   regexp = msg)

    #sizeBy
    msg <- "sizeBy should be a column in the design attribute of dgeObj. Assigning NULL as default value."
    expect_warning(mds_plot <- ggplotMDS(dgeObj = t_obj1,
                                         sizeBy  = "Replicate"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(dgeObj = t_obj1,
                                         sizeBy  = "1"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(dgeObj = t_obj1,
                                         sizeBy  = c("value1", "value2")),
                   regexp = msg)

    #labels
    msg <- "label specifed is either missing or invalid. Assigning default values."
    expect_warning(mds_plot <- ggplotMDS(dgeObj  = t_obj1,
                                         labels   = "Replicate",
                                         plotType = "ggplot"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(dgeObj  = t_obj1,
                                         labels   = c("va1", "val2"),
                                         plotType = "ggplot"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(dgeObj  = t_obj1,
                                         labels   = 1,
                                         plotType = "ggplot"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    missing_repgroup_obj <- t_obj1
    missing_repgroup_obj[["design"]][["ReplicateGroup"]] <- NULL
    expect_warning(mds_plot <- ggplotMDS(dgeObj  = missing_repgroup_obj,
                                         plotType = "ggplot"),
                   regexp = "label specifed is either missing or invalid. Assigning default values.")
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    #xlab
    msg <- "xlab value specified is not valid. Ignoring xlab."

    expect_warning(mds_plot <- ggplotMDS(dgeObj = t_obj1,
                                         xlab = 1),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(dgeObj = t_obj1,
                                         xlab = c("value1", "value2")),
                   regexp = msg)

    #ylab
    msg <- "ylab value specified is not valid. Ignoring ylab."
    expect_warning(mds_plot <- ggplotMDS(dgeObj = t_obj1,
                                         ylab = 1),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(dgeObj = t_obj1,
                                         ylab = c("value1", "value2")),
                   regexp = msg)

    #top
    msg <- "top should be a numeric value or Inf. Assigning default value 'Inf'."
    expect_warning(mds_plot <- ggplotMDS(dgeObj = t_obj1,
                                         top     = "abc"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(dgeObj = t_obj1,
                                         top     = c(1,2)),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    msg <- "hlineIntercept must be numeric. Ignoring hlineIntercept."
    expect_warning(mds_plot <- ggplotMDS(dgeObj        = t_obj1,
                                         plotType       = "ggplot",
                                         hlineIntercept = "abc"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(dgeObj        = t_obj1,
                                         plotType       = "ggplot",
                                         hlineIntercept = c("a","b")),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    msg <- "vlineIntercept must be numeric. Ignoring vlineIntercept."
    expect_warning(mds_plot <- ggplotMDS(dgeObj        = t_obj1,
                                         plotType       = "ggplot",
                                         vlineIntercept = "abc"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    msg <- "Invalid value specified for Title. Assigning default values 'MDS plot'."
    expect_warning(mds_plot <- ggplotMDS(dgeObj = t_obj1,
                                         title   = 32),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(dgeObj  = t_obj1,
                                         title    = c('title1', 'title2')),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))
})


test_that("ggplotMDS.R: MDS_var_explained()", {
    mds_plot <- ggplotMDS(dgeObj = t_obj1)
    var_result <- MDS_var_explained(mds_plot$mdsobj)
    expect_length(var_result, 3)
    expect_named(var_result, c("varexp", "cumvar", "var_explained"))
    expect_type(var_result, "list")
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))
    expect_s3_class(var_result$cumvar, c("canvasXpress", "htmlwidget"))

    var_result <- MDS_var_explained(mds_plot$mdsobj,
                                    plotType = "ggplot")
    expect_length(var_result, 3)
    expect_named(var_result, c("varexp", "cumvar", "var_explained"))
    expect_type(var_result, "list")
    expect_s3_class(var_result$varexp, c("gg", "ggplot"))
    expect_s3_class(var_result$cumvar, c("gg", "ggplot"))

    # testing mds matrix
    var_result <- MDS_var_explained(t_obj1$counts)
    expect_length(var_result, 3)
    expect_named(var_result, c("varexp", "cumvar", "var_explained"))
    expect_type(var_result, "list")
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))
    expect_s3_class(var_result$cumvar, c("canvasXpress", "htmlwidget"))

    var_result <- MDS_var_explained(t_obj1$counts,
                                    plotType = "ggplot")
    expect_length(var_result, 3)
    expect_named(var_result, c("varexp", "cumvar", "var_explained"))
    expect_type(var_result, "list")
    expect_s3_class(var_result$varexp, c("gg", "ggplot"))
    expect_s3_class(var_result$cumvar, c("gg", "ggplot"))

    # testing optional parameters
    var_result <- MDS_var_explained(mds_plot$mdsobj,
                                    barWidth = 0.8,
                                    cumVarLimit = 1.0,
                                    barColor = "red",
                                    topN = Inf)
    expect_length(var_result, 3)
    expect_named(var_result, c("varexp", "cumvar", "var_explained"))
    expect_type(var_result, "list")
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))
    expect_s3_class(var_result$cumvar, c("canvasXpress", "htmlwidget"))

    var_result <- MDS_var_explained(mds_plot$mdsobj,
                                    plotType = "ggplot",
                                    barWidth = 0.8,
                                    cumVarLimit = 1.0,
                                    barColor = "red")
    expect_length(var_result, 3)
    expect_named(var_result, c("varexp", "cumvar", "var_explained"))
    expect_type(var_result, "list")
    expect_s3_class(var_result$varexp, c("gg", "ggplot"))
    expect_s3_class(var_result$cumvar, c("gg", "ggplot"))

    msg <- "barWidth should be a single numeric value between 0 and 1. Assigning default value 0.65."
    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   barWidth = 1.5),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                    plotType = "ggplot",
                                    barWidth = 1.5),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("gg", "ggplot"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   plotType = "ggplot",
                                                   barWidth = c("a","b")),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("gg", "ggplot"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   plotType = "ggplot",
                                                   barWidth = -1),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("gg", "ggplot"))

    msg <- "cumVarLimit should be a single numeric value between 0 and 1. Assigning default value 0.9."
    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   cumVarLimit = 1.5),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   cumVarLimit = c("a","b")),
                   regexp = msg)
    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   cumVarLimit = -1),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    msg <- "barColor specified is not valid. Assigning default value 'dodgerblue4'."
    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   barColor = "abc"),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   barColor = 1),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   barColor = c("red", "blue")),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    msg <- "topN should be a numeric value. Assigning default value 10."
    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   topN = "abc"),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   topN = c(1,2)),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   topN = -1),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    # testing assert statements
    expect_error(MDS_var_explained(), regexp = "mds is required and must be specified.")
    expect_error(MDS_var_explained(t_obj1$counts, plotType = "cx"),
                 regexp = "Plot type must be either canvasXpress or ggplot.")

})
