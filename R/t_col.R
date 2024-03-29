t_col <- function(color, percent = 50, name = NULL) {
    #      color = color name
    #    percent = % transparency
    #       name = an optional name for the color

    ## Get RGB values for named color
    rgb.val <- col2rgb(color)

    ## Make new color using input color as base and alpha set by transparency
    t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                 maxColorValue = 255,
                 alpha = (100 - percent) * 255 / 100,
                 names = name)

    ## Save the color
    invisible(t.col)
}
# END
# Transparent colors
# Mark Gardener 2015
# www.dataanalytics.org.uk
