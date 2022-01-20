context("DGEobj.plots - tests for mapDGEobj.R functions")

test_that('mapDGEobj.R: mapDGEobj()', {
    #using canvasXpress
    map_DGEobj <- mapDGEobj(t_obj1)
    expect_s3_class(map_DGEobj, c("canvasXpress", "htmlwidget"))

    msg <- "dgeObj must be specified and must be of class 'DGEobj'."
    expect_error(mapDGEobj(),
                 regexp = msg)
    expect_error(mapDGEobj(dgeObj = NULL),
                 regexp = msg)
    expect_error(mapDGEobj(iris),
                 regexp = msg)

    #Optional parameters
    #plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'canvasXpress'."
    expect_warning(map_DGEobj <- mapDGEobj(t_obj1,
                                           plotType = "cx"),
                   regexp = msg)

    expect_s3_class(map_DGEobj, c("canvasXpress", "htmlwidget"))

    expect_warning(map_DGEobj <- mapDGEobj(t_obj1,
                                           plotType = NULL),
                   regexp = msg)
    expect_s3_class(map_DGEobj, c("canvasXpress", "htmlwidget"))

    expect_warning(map_DGEobj <- mapDGEobj(t_obj1,
                                           plotType = 1),
                   regexp = msg)
    expect_s3_class(map_DGEobj, c("canvasXpress", "htmlwidget"))

    expect_warning(map_DGEobj <- mapDGEobj(t_obj1,
                                           plotType = c("canvasXpress", "igraph")),
                   regexp = msg)

    expect_s3_class(map_DGEobj, c("canvasXpress", "htmlwidget"))

    #using ggplot
    skip_if_not_installed("ggraph")
    skip_if_not_installed("tidygraph")

    map_DGEobj <- mapDGEobj(t_obj1, plotType = "ggplot")
    expect_s3_class(map_DGEobj, c("ggraph", "gg", "ggplot"))

    map_DGEobj <-
        mapDGEobj(t_obj1, plotType = "ggplot", directed = FALSE)
    expect_s3_class(map_DGEobj, c("ggraph", "gg", "ggplot"))

    #directed
    msg <-
        "directed must be a singular logical value. Assigning default value TRUE."
    expect_warning(map_DGEobj <- mapDGEobj(t_obj1,
                                           plotType = "ggplot",
                                           directed = NULL),
                   regexp = msg)
    expect_s3_class(map_DGEobj, c("ggraph", "gg", "ggplot"))

    expect_warning(
        map_DGEobj <- mapDGEobj(t_obj1,
                                plotType = "ggplot",
                                directed = "Invalidvalue"),
        regexp = msg
    )
    expect_s3_class(map_DGEobj, c("ggraph", "gg", "ggplot"))

    expect_warning(map_DGEobj <- mapDGEobj(
        t_obj1,
        plotType = "ggplot",
        directed = c(TRUE, FALSE)
    ),
    regexp = msg)
    expect_s3_class(map_DGEobj, c("ggraph", "gg", "ggplot"))

})
