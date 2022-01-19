#' Construct DGEobj relationships
#'
#' Reads a DGEobj and visualizes parent/child relationships between
#' data items in a DGEobj.
#'
#' @param dgeObj DGEobj to find the parent/child relationships between data items.
#' @param plotType Must be canvasXpress or ggplot (default = canvasXpress).
#' @param directed Only applies to ggplot. Indicates if the graph should
#'     be directed or not. (default = TRUE)
#'
#' @return A class ggplot object or a canvasXpress network plot.
#'
#'
#' @examples
#' \dontrun{
#'   # canvasXpress network plot
#'   mapDGEobj(dgeObj)
#'
#'   # ggplot object
#'   mapDGEobj(dgeObj, plotType = "ggplot")
#'   }
#'
#' @import magrittr
#' @importFrom assertthat assert_that
#' @importFrom canvasXpress canvasXpress
#' @importFrom htmlwidgets JS
#' @importFrom dplyr filter rename left_join
#'
#' @export
mapDGEobj <- function(dgeObj,
                      plotType = "canvasXpress",
                      directed = TRUE) {

    assertthat::assert_that(!missing(dgeObj),
                            !is.null(dgeObj),
                            "DGEobj" %in% class(dgeObj),
                            msg = "dgeObj must be specified and must be of class 'DGEobj'.")

    plotType <- tolower(plotType)
    if (any(is.null(plotType),
            !is.character(plotType),
            length(plotType) != 1,
            !tolower(plotType) %in% c("canvasxpress", "ggplot"))) {
        warning("plotType must be either canvasXpress or ggplot. Assigning default value 'canvasXpress'.")
        plotType <- "canvasxpress"
    }

    if (plotType == "ggplot") {
        assertthat::assert_that(requireNamespace("ggraph", quietly = TRUE),
                                msg = "ggraph package is required for this plot type")

        assertthat::assert_that(requireNamespace("tidygraph", quietly = TRUE),
                                msg = "tidygraph package is required for this plot type")
    }

    if (any(is.null(directed),
            !is.logical(directed),
            length(directed) != 1)) {
        warning("directed must be a singular logical value. Assigning default value TRUE.")
        directed <- TRUE
    }

    child <- names(dgeObj) %>%
        as.data.frame()
    colnames(child) <- "child"

    parent_list <- attr(dgeObj, "parent")
    mul_parent <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("child", "parent"))
    for (i in names(parent_list)) {
        if (length(parent_list[[i]]) > 1) {
           for (j in unlist(parent_list[[i]])) {
               mul_parent <- rbind(mul_parent, data.frame(child = i, parent = j))
           }
           parent_list[[i]] <- NULL
        }
    }

    parent <- data.frame(parent_list) %>%
        t() %>%
        as.data.frame() %>%
        dplyr::rename(parent = V1) %>%
        dplyr::filter(nchar(parent) > 0)

    parent[["child"]] <- rownames(parent)

    parent <- rbind(parent, mul_parent)
    edges  <- parent[c("parent", "child")]

    assertthat::assert_that(all(parent$parent %in% child$child),
                            all(parent$child %in% child$child),
                            msg = "A valid DGEobj needs to be specified. Node can have a maximum of one parent only.")

    type <- attr(dgeObj, "type") %>%
        as.data.frame() %>%
        t() %>%
        as.data.frame() %>%
        dplyr::rename(Type = V1)

    type[["child"]] <- rownames(type)
    type <- select(type, child, Type)

    basetype <- attr(dgeObj, "basetype") %>%
        as.data.frame() %>%
        t() %>%
        as.data.frame() %>%
        dplyr::rename(BaseType = V1)

    basetype[["child"]] <- rownames(basetype)

    nodes <- dplyr::left_join(type, basetype, by = "child")

    if (plotType == "canvasxpress") {
        colnames(nodes) <- c("id", "Type", "BaseType")
        colnames(edges) <- c("id1", "id2")

        events <- htmlwidgets::JS("{ 'mousemove' : function(o, e, t) {
                                                if (o != null && o != false) {
                                                    if (o.objectType == null) {
                                                        if (o.nodes != null) {
                                                            t.showInfoSpan(e, '<b>' + 'Node' + ': ' + o.nodes[0].id + '</b> <br/>' +
                                                             '<b>' + 'Type'  + '</b>' + ': ' + o.nodes[0].Type + '<br/>' +
                                                             '<b>' + 'Base type'  + '</b>' + ': ' + o.nodes[0].BaseType + '<br/>');

                                                        } else if (o.edges != null) {
                                                            t.showInfoSpan(e, '<b>' + o.edges[0].id1 + '&#10230;' + o.edges[0].id2 + '</b>');
                                                        }
                                                    } else {
                                                        t.showInfoSpan(e, o.display);
                                                    };
                                                }; }}")

        canvasXpress::canvasXpress(data              = list(nodeData = nodes, edgeData = edges),
                                   colorNodeBy       = "Type",
                                   labelNodePosition = "left",
                                   edgeWidth         = 2,
                                   graphType         = "Network",
                                   nodeSize          = 20,
                                   nodeFontSize      = 10,
                                   networkLayoutType = "forceDirected",
                                   events            = events)

    } else {
        tidy_graph <- tidygraph::tbl_graph(nodes = nodes, edges = edges)

        if (directed) {
            plain_graph <- ggraph::ggraph(tidy_graph, layout = "sugiyama") +
                ggraph::geom_edge_link(arrow = arrow(length = unit(4, "mm")),
                                       end_cap = ggraph::circle(6, "mm"))

        } else {
            plain_graph <- ggraph::ggraph(tidy_graph, layout = "sugiyama") +
                ggraph::geom_edge_link()
        }

        plain_graph  +
            ggraph::geom_node_point(aes(color = Type), size = 12) +
            ggraph::geom_node_label(aes(label = child), size = 3)

    }
}
