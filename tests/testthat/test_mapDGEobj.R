context("DGEobj.plots - tests for mapDGEobj.R functions")


test_that('mapDGEobj.R: mapDGEobj()', {
    map_DGEobj <- mapDGEobj(t_obj1)
    expect_s3_class(map_DGEobj, c("canvasXpress", "htmlwidget"))

    map_DGEobj <- mapDGEobj(t_obj1, plotType = "ggplot")
    expect_s3_class(map_DGEobj, "igraph")

    map_DGEobj <- mapDGEobj(t_obj1, plotType = "ggplot", directed = FALSE)
    expect_s3_class(map_DGEobj, "igraph")

    msg <- "dgeObj must be specified and must be of class 'DGEobj'."
    expect_error(mapDGEobj(),
                 regexp = msg)
    expect_error(mapDGEobj(dgeObj = NULL),
                 regexp = msg)
    expect_error(mapDGEobj(iris),
                 regexp = msg)

    #Optional parameters
    #plotType
    msg <- "plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'."
    expect_warning( map_DGEobj <- mapDGEobj(t_obj1,
                                            plotType = "cx"),
                    regexp = msg)
    expect_s3_class(map_DGEobj, c("canvasXpress", "htmlwidget"))

    expect_warning( map_DGEobj <- mapDGEobj(t_obj1,
                                            plotType = NULL),
                    regexp = msg)
    expect_s3_class(map_DGEobj, c("canvasXpress", "htmlwidget"))

    expect_warning( map_DGEobj <- mapDGEobj(t_obj1,
                                            plotType = 1),
                    regexp = msg)
    expect_s3_class(map_DGEobj, c("canvasXpress", "htmlwidget"))

    expect_warning( map_DGEobj <- mapDGEobj(t_obj1,
                                            plotType = c("canvasXpress", "ggplot")),
                    regexp = msg)
    expect_s3_class(map_DGEobj, c("canvasXpress", "htmlwidget"))

    #directed
    msg <- "directed must be a singular logical value. Assigning default value TRUE."
    expect_warning(map_DGEobj <- mapDGEobj(t_obj1,
                                           plotType = "ggplot",
                                           directed = NULL),
                                regexp = msg)
    expect_s3_class(map_DGEobj, "igraph")

    expect_warning(map_DGEobj <- mapDGEobj(t_obj1,
                                           plotType = "ggplot",
                                           directed = "Invalidvalue"),
                   regexp = msg)
    expect_s3_class(map_DGEobj, "igraph")

    expect_warning(map_DGEobj <- mapDGEobj(t_obj1,
                                           plotType = "ggplot",
                                           directed = c(TRUE, FALSE)),
                   regexp = msg)
    expect_s3_class(map_DGEobj, "igraph")
})
