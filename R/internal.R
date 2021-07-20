.tsmsg <- function(...) {
    # works like message() but prepends a timestamp
    message(date(), ": ", ...)
}

.rgbaConversion <- function(color, alpha = 0.5){
    rgbastr <- NULL
    if (!is.character(color)) {
        rgbastr <- "invalid value"
    }
    tryCatch({
        rgbaVal <- paste(c(col2rgb(color), alpha), collapse = ",")
        rgbastr <- paste0("rgba(", rgbaVal, ")")
    }, error = function(e) {
        warning(paste("Ïnvalid color specified",color))
    })
    if (is.null(rgbastr)) {
        rgbastr <- "invalid value"
    }
    rgbastr
}


.getCxPlotDecorations <- function(decorations, color, width, x, y) {
    line <- list(color = color,
                 width = width)
    if (!missing(x) && !missing(y)) {
        line <- append(line, list(x  = x,
                                  x2 = y,
                                  y  = x,
                                  y2 = y))
    } else if (!missing(x)) {
        line <- append(line, list(x = x))
    } else if (!missing(y)) {
        line <- append(line, list(y = y))
    }

    list(line = append(decorations$line, list(line)))
}

.is_valid_symbolShapes_cxplot <- function(shape) {
    valid_shapes <- .get_valid_symbolShapes_cxplot()
    is_valid_shape <- FALSE
    if (!is.null(shape)) {
        is_valid_shape <- ifelse(shape %in% valid_shapes, TRUE, FALSE)
    }
    is_valid_shape
}

.get_valid_symbolShapes_cxplot <- function() {
    valid_shapes <- c("circle","sphere", "square", "rhombus", "triangle", "plus", "star", "octagon", "oval",
                      "minus", "pacman", "pacman2", "mdavid", "rect2", "pentagon",
                      "rect3", "arc", "rectangle", "image")
}

.get_valid_symbolShapes_ggplot <- function() {
    shape_names <- c(
        "circle", "square", "diamond", "triangle", "plus", "asterisk", "cross", "bullet",
        paste("circle", c("open", "filled", "cross", "plus", "small")),
        paste("square", c("open", "filled", "cross", "plus", "triangle")),
        paste("diamond", c("open", "filled", "plus")),
        paste("triangle", c("open", "filled", "square")),
        paste("triangle down", c("open", "filled"))
    )
}

.is_valid_symbolShapes_ggplot <- function(shape) {
    valid_shapes <- .get_valid_symbolShapes_ggplot()
    is_valid_shape <- FALSE
    if (!is.null(shape)) {
        is_valid_shape <- ifelse(shape %in% valid_shapes, TRUE, FALSE)
        if (!is_valid_shape && (shape %in% c(1:25))) {
            is_valid_shape <- TRUE
        }
    }
    is_valid_shape
}

.validate_colors <- function(colors) {
    valid_colors <- list()
    valid_colors <- lapply(colors, function(color){
        ifelse(.rgbaConversion(color) != "invalid value", color, NA)
    })
    valid_colors[!is.na(valid_colors)]
}

.validate_cx_shapes <- function(shapes) {
    valid_shapes <- list()
    valid_shapes <- lapply(shapes, function(shape){
        ifelse(.is_valid_symbolShapes_cxplot(shape), shape, NA)
    })
    valid_shapes[!is.na(valid_shapes)]
}

.validate_gg_shapes <- function(shapes) {
    valid_shapes <- list()
    valid_shapes <- lapply(shapes, function(shape){
        ifelse(.is_valid_symbolShapes_ggplot(shape), shape, NA)
    })
    valid_shapes[!is.na(valid_shapes)]
}

.getCxPlotDecorations <- function(decorations, color, width, x, y) {
    line <- list(color = color,
                 width = width)
    if (!missing(x) && !missing(y)) {
        line <- append(line, list(x  = x,
                                  x2 = y,
                                  y  = x,
                                  y2 = y))
    } else if (!missing(x)) {
        line <- append(line, list(x = x))
    } else if (!missing(y)) {
        line <- append(line, list(y = y))
    }
    list(line = append(decorations$line, list(line)))
}

.setLegendPosition <- function(ggplot,
                               legendPosition = "right",
                               themeStyle = "grey") {

    if (is.null(legendPosition)) {
        ggplot <- ggplot +
            theme(legend.position = "none")
    } else if (legendPosition %in% c("top", "bottom", "left", "right")) {
        ggplot <- ggplot +
            theme(legend.position = legendPosition)
    } else { # Place legend inside figure; with or without a grey background
        #Four corners
        if (legendPosition == "topRight") {
            ggplot <- ggplot +
                theme(legend.justification = c(1,1), legend.position = c(1,1))
        } else if (legendPosition == "bottomRight") {
            ggplot <- ggplot +
                theme(legend.justification = c(1,0), legend.position = c(1,0))
        } else if (legendPosition == "bottomLeft") {
            ggplot <- ggplot +
                theme(legend.justification = c(0,0), legend.position = c(0,0))
        } else if (legendPosition == "topLeft") {
            ggplot <- ggplot +
                theme(legend.justification = c(0,1), legend.position = c(0,1))
        }
    }
    ggplot <- ggplot +
        theme(legend.background = element_rect(color = "grey30", fill = "grey90"))
    return(ggplot)

}

#' @importFrom utils packageVersion
.yrange <- function(my.ggp) {
    assertthat::assert_that(class(my.ggp)[[2]] == "ggplot",
                            msg = "my.ggp must be of class 'ggplot'.")
    # Method used is ggplot2 version-dependent
    ggplot_version <- stringr::str_sub(as.character(utils::packageVersion("ggplot2")),1,1)
    if (ggplot_version == 2) {
        # ggplot2 v2 solution:
        range <- ggplot2::ggplot_build(my.ggp)$layout$panel_ranges[[1]]$y.range
    } else {
        # ggplot2 v3 solution:
        range <- ggplot2::ggplot_build(my.ggp)$layout$panel_params[[1]]$y.range
    }
    return(range)
}

.xrange <- function(my.ggp) {
    assertthat::assert_that(class(my.ggp)[[2]] == "ggplot",
                            msg = "my.ggp must be of class 'ggplot'.")
    # Method used is ggplot2 version-dependent
    ggplot_version <- stringr::str_sub(as.character(utils::packageVersion("ggplot2")),1,1)
    if (ggplot_version == 2) {
        # ggplot2 v2:
        range <- ggplot2::ggplot_build(my.ggp)$layout$panel_ranges[[1]]$x.range
    } else {
        # ggplot2 v3 solution:
        range <-  ggplot2::ggplot_build(my.ggp)$layout$panel_params[[1]]$x.range
    }
    return(range)
}

.addFootnote <- function(my.ggp,
                         footnoteText,
                         footnoteSize,
                         footnoteColor,
                         footnoteJust = 1,
                         yoffset = 0) {

    yr <- .yrange(my.ggp)
    xr <- .xrange(my.ggp)
    yoffset <- yoffset * (yr[2] - yr[1])
    xcoord <- ifelse(footnoteJust < 0.50, xr[1], xr[2])
    if (footnoteJust == 0.5) {# Special case = center
        xcoord <- xr[1] + ((xr[2] - xr[1]) / 2)
    }
    my.ggp <- my.ggp +
        annotate("text", label = footnoteText, x = xcoord, y = yr[1] + yoffset,
                 size = footnoteSize,
                 colour = footnoteColor,
                 hjust = footnoteJust,
                 vjust = 1)
}


.cxSupportedLineFit <- function(linefit) {
    if (!(linefit %in% c("lm", "loess"))) {
        if (linefit == "glm") {
            linefit <- "lm"
        } else if (linefit == "gam") {
            linefit <- "loess"
        }
        warning("Model type is not supported for canvasXpress charts and ", linefit," is being used")
    }

    return(linefit)
}
