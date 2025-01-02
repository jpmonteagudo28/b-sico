# https://stackoverflow.com/questions/29129252/facet-wrap-like-plot-using-r-base-graphics

facet_wrap <- function(x,y, by = NULL,data, horiz = TRUE, ...) {
  ## save current par settings and return after finished
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  zz <- unique(data[, z])

  ## sets up the layout to cascade horizontally or vertically
  ## and sets xlim and ylim appropriately
  if (horiz) {
    par(mfrow = c(1, length(zz)), ...)
    ylim <- range(data[, y])
    xlim <- NULL
  } else {
    par(mfrow = c(length(zz), 1), ...)
    xlim <- range(data[, x])
    ylim <- NULL
  }

  ## make a subset of data for each unique by variable
  ## and draw a basic plot for each one
  for (ii in zz) {
    tmp <- data[data[, z] %in% ii, ]
    plot(tmp[, x], tmp[, y], xlim = xlim, ylim = ylim)
  }
}
