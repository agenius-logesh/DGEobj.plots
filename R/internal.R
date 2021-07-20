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
