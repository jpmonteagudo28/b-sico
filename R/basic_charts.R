#' @title A lightweight extension of the base R plotting function
#'

chart = function(x,...){
  UseMethod("chart")
}

chart.default = function(
    x = NULL,
    y = NULL,
    by = NULL,
    facet = NULL,
    facet.args = NULL,
    data = NULL,
    type = NULL,
    xlim = NULL,
    ylim = NULL,
    log = "",
    main = NULL,
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    ann = par("ann"),
    axes = TRUE,
    frame.plot = NULL,
    asp = NA,
    grid = NULL,
    palette = NULL,
    legend = NULL,
    pch = NULL,
    lty = NULL,
    lwd = NULL,
    col = NULL,
    bg = NULL,
    fill = NULL,
    alpha = NULL,
    cex = 1,
    restore.par = FALSE,
    xmin = NULL,
    xmax = NULL,
    ymin = NULL,
    ymax = NULL,
    add = FALSE,
    file = NULL,
    width = NULL,
    height = NULL,
    empty = FALSE,
    xaxt = NULL,
    yaxt = NULL,
    flip = FALSE,
    xaxs = NULL,
    yaxs = NULL,
    ...
    )

dots <- list(...)
