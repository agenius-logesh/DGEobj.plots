baseTheme <- function(base_size = 18,
                      base_family = "",
                      scale_legend = TRUE) {
    if (scale_legend == TRUE && base_size > 17) {
        Legend.ScaledSize <- 14/base_size
        Legend.ScaledFont <- 12/base_size
    } else {
        Legend.ScaledSize = 1.2 # theme_grey defaults
        Legend.ScaledFont = 0.8
    }

    baseTheme <- theme(  # Base size is the axis tick mark labels; other elements are scaled
        axis.text.x = element_text(size = rel(1.0)),
        axis.text.y = element_text(size = rel(1.0)),
        axis.title.x = element_text(size = rel(1.25), vjust = 0.5, hjust = 0.5, color = "black"),
        axis.title.y = element_text(size = rel(1.25), vjust = 0.5, hjust = 0.5, color = "black"),
        plot.title = element_text(face = "bold", size = rel(1.5)),
        legend.text = element_text(colour = "Black", size = rel(Legend.ScaledFont)),
        legend.title = element_text(colour = "Black", size = rel(1.2)),
        legend.title.align = 0.5,
        legend.key.size = unit(Legend.ScaledSize, "lines"),
        strip.text.x = element_text(size = rel(1.6)),
        strip.text.y = element_text(size = rel(1.6)),
        text = element_text(size = base_size, family = base_family)
    )
}


facetTheme <- function(base_size = 18,
                       base_family = "") {
    facetTheme = theme(
        axis.text.x = element_text(size = rel(1.0)),
        axis.text.y = element_text(size = rel(1.0)),
        axis.title.x = element_text(face = "bold", colour = "Black", size = rel(2.0)),
        axis.title.y = element_text(face = "bold", colour = "Black", size = rel(2.0)),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(lineheight = .8, face = "bold", size = rel(2.4)),
        legend.text = element_text(colour = "Black", size = rel(1.6)),
        legend.title = element_text(colour = "Black", size = rel(1.4)),
        legend.title.align = 0.5,
        strip.text.x = element_text(size = rel(1.6)),
        strip.text.y = element_text(size = rel(1.6)),
        text = element_text(size = base_size, family = base_family)
    )
}


baseFont <- function(base_size = 12,
                     base_family = ""){
    baseFont <- theme(text = element_text(size = base_size, family = base_family))
}
