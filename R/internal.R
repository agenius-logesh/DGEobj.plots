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

#' @importFrom utils packageVersion
yrange <- function(my.ggp) {
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

xrange <- function(my.ggp) {
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
