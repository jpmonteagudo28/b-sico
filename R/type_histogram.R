#' Edward Tufte Style Histogram
#'
#' @description
#' Visualize the distribution of a single **continuous** variable using Edward Tufte's minimalist principles for data visualization.
#'
#' The function includes various methods for calculating histogram breaks, including:
#' - **Sturges**: Sturges' formula based on a normal distribution approximation.
#' - **Scott**: Scott's rule, which minimizes the integrated mean squared error for a normal density.
#' - **FD**: The Freedman-Diaconis rule, which uses the interquartile range for robust bin width calculation.
#' - **Doane**: A modified Sturges' formula to account for skewness in the data.
#' - **Terrell-Scott**: Terrell-Scott's non-normal reference rule for optimal binning, based on IQR.
#'
#' @param x A numeric vector representing the data to be visualized.
#' @param shape The shape of the histogram: "segment", "rect", or "lines".
#' @param show_border A character string specifying which borders to display for shape = "segment" ("left" or "right").
#' @param na_rm Logical, whether to remove `NA` values from the data. Default is `TRUE`.
#' @param breaks Method to compute the histogram bins. Options are:
#'   - `"sturges"`: Sturges' rule.
#'   - `"scott"`: Scott's normal reference rule.
#'   - `"fd"`: Freedman-Diaconis rule.
#'   - `"doane"`: Doane's formula.
#'   - `"ts"`: Terrell-Scott rule.
#'   - A numeric value for fixed bin width or a custom function.
#' @param freq Logical, whether to display frequency (`TRUE`) or density (`FALSE`) on the y-axis. Default is `TRUE`.
#' @param right Logical, whether bins should be right-closed. Default is `TRUE`.
#' @param fuzz Numeric, a small value added to bin boundaries to prevent rounding issues.
#' @param include_lowest Logical, whether the lowest bin should include the smallest value. Default is `TRUE`.
#' @param axes Logical, whether to display x and y axes. Default is `FALSE`.
#' @param labels Logical, whether to display axis labels. Default is `TRUE`.
#' @param label_names Optional, custom names for x and y axis labels. Default is `NULL`.
#' @param label_size Numeric, size of axis labels. Default is `NULL`.
#' @param main Optional, a title for the histogram. Default is `NULL`.
#' @param in_line Numeric, line spacing for the title. Default is `1`.
#' @param xlab Custom x-axis label. Default is `NULL`.
#' @param ylab Custom y-axis label. Default is `NULL`.
#' @param line_width Numeric, line width for the histogram. Default is `0.5`.
#' @param line_type Line type for the histogram ("solid", "dashed", etc.). Default is `"solid"`.
#' @param line_color Color of the histogram lines. Default is `"gray20"`.
#' @param ... Additional arguments passed to the function.
#'
#' @return A histogram plot with the desired styling and a list structure containing:
#' - `breaks`: The computed breaks for the histogram bins.
#' - `counts`: The frequency of observations in each bin.
#' - `density`: The density of observations in each bin.
#' - `mid_points`: The midpoints of each bin.
#'
#' @details
#' The choice of breaks method significantly influences the histogram's appearance. The function provides robust defaults while allowing customization to suit specific data distributions.
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#' histogram(x, shape = "rect", breaks = "fd", axes = TRUE, labels = TRUE)
#'
#' # Using formula argument
#' df <- data.frame(x = rexp(1000,2.5))
#' histogram(~x,df,shape = "line", breaks = 12)
#'
#' @seealso [hist()], [cut()],[geom_histogram()]
#' @export


# Notes from https://web.archive.org/web/20150501071703/http://www.stat.rice.edu/~scottdw/stat550/HW/hw3/c03.pdf
# https://en.wikipedia.org/wiki/Histogram
#
# A histogram is completely determined by the bin width,h,and the bin origin, t0.
# R implements Sturges' methods which takes the normal density as a point of
# reference when thinking about data, and assumes that th binomial distribution B(n, p = 0.5),
# could be used as a mode lof an optimally constructed histogram with appropriately scaled Normal
# data.
    # Sturges' number-of-bins rule: k = 1 + log2(n), n = sample size.
# A much simpler assumption to implement is that all histograms have an infinite
# number of bins, only a finite number of which are non-empty, and not all bins
# have equal width. These histograms are called adaptive histograms.
#---- --- ----- --- ---- --- ----- --- ---- --- ----#
# Scott's normal reference rule(1979):
# If f = N(mu,sigma^2), then R(f') = 1/(4*sqrt(pi*sigma^3)) approximately
# equal to  h = 3.5*sigma*(n^-1/3)
#---- --- ----- --- ---- --- ----- --- ---- --- ----#
# Friedman and Diaconis(1981) proposed a more robust rule to deal with outliers
# using the IQR instead of the sample standard deviation in Scott's rule.
# h = 2*IQR*(n^-1/3)
#---- --- ----- --- ---- --- ----- --- ---- --- ----#
# Terrell-Scott non-normal reference rule: in the interval (a,b),
# The minimum number of bins required for an asymptotically optimal histogram,
# where h = (b-a)/(2n)^1/3 is the over-smoothed bandwidth,
# and the number of bins is equal to (2n)^1/3.
# Terrell considered a version of this formula based on the more robust IQR:
# h = 3.729*IQR/(n^1/3)
#---- --- ----- --- ---- --- ----- --- ---- --- ----#
# Aspects to take care of:
# 1. Frequency (sample size per bin) - done
# 2. Number of bins - done
# 3. Bin width - done
# 4. Total area of histogram equal to 1 or not - density(freq = FALSE) - done
histogram <- function(x,...)
  UseMethod("histogram")


histogram.default <- function(x,
                      shape = NULL,
                      show_border = NULL,
                      na_rm = TRUE,
                      breaks = "scott", # used to compute number of bins
                      freq = TRUE, # if FALSE, histogram will display density on y-axis
                      right= TRUE,
                      fuzz = NULL,
                      include_lowest = TRUE,
                      xlim = NULL,
                      ylim = NULL,
                      axes = FALSE,
                      labels = TRUE,
                      label_names = NULL,
                      label_size = NULL,
                      main = NULL,
                      in_line = 1,
                      xlab = NULL,
                      ylab = NULL,
                      line_width = 0.5,
                      line_type = "solid",
                      line_color = "gray20",
                      ...
){

  stopifnot(is.numeric(x),
            is.logical(na_rm),
            is.logical(freq),
            is.logical(right),
            is.logical(include_lowest),
            is.logical(axes),
            is.logical(labels))

  shape <- match.arg(shape,c("segment","rect","lines"))
  show_border <- match.arg(show_border,c("left","right"))

  if(shape != "segment" && !is.null(show_border))
    warning("Ignoring 'show_border' argument.Borders only applied to shape 'segment'")

  if(shape == "segment" && is.null(show_border))
    stop("Please, specify a border to be drawn for shape 'segment'")

  # Check for identical data points
  if(all_the_same(x)){
    stop("All data values are identical. Histogram bins cannot be calculated.")
  }

  # Remove NA
  if(na_rm){
    x <- rid_na(x)
  }

  if(is_empty_object(x))
    stop("Data only contained NA values. Provide a valid numeric object or dataset ")

  # Remove Inf,-Inf
  x <- keep_finite(x)


  n <- length(x)

  use_breaks <- !missing(breaks)

  if(!use_breaks){
    warning("Using Scott's normal reference rule to compute breaks")
    breaks <- scott_breaks(x)
    breaks <- pretty(range(x),n = breaks)
  }

  if (is.character(breaks)) {
    breaks <- match.arg(breaks, c("sturges", "doane", "scott", "fd", "ts"))
    breaks <- switch(breaks,
                     sturges = sturges_breaks(x),
                     doane = doane_breaks(x),
                     scott = scott_breaks(x),
                     fd = fd_breaks(x),
                     ts = ts_breaks(x))
    breaks <- pretty(range(x), n = breaks)
  } else if (is.numeric(breaks) && length(breaks) == 1L) {
    breaks <- pretty(range(x), n = breaks, min.n = 1)
  } else if (is.numeric(breaks) && length(breaks) > 1L) {
    breaks <- sort(breaks)
  } else if (is.function(breaks)) {
    breaks <- breaks(x)
  } else {
    stop("Invalid breaks specification")
  }

  n_breaks <- length(breaks)
  bin_width <- diff(breaks) # Preferred over diff(range(x))/breaks, which gives only an average
  bin_width_range <- diff(range(bin_width)) # constant difference - linear seq.
  # Check if bin widths are equidistant if not specified by the user and constant difference of 0
  equidistant <- !use_breaks || all_the_same(bin_width_range, tol = 1e-8* mean(bin_width))

  # Check if bin-widths are set to zero or a negative number
  if(!use_breaks && any_zero_negative(bin_width)){
    stop("'breaks' are not strictly increasing")
  }

  # Adapted ggplot2::bin - this protects from floating point rounding errors
  fuzz <- fuzz %||% 1e-08 * stats::median(diff(breaks))
  if (!is.finite(fuzz)) { # happens when 0 or 1 finite breaks are given
    fuzz <- .Machine$double.eps * 1e3
  }
  if (right) {
    fuzzes <- c(-fuzz, rep.int(fuzz, n_breaks - 1))
  } else {
    fuzzes <- c(rep.int(-fuzz, n_breaks - 1), fuzz)
  }
  fuzzy <- breaks + fuzzes


  counts <- cut_counts(x,fuzzy,
                         right = right,
                         include.lowest = include_lowest)
  # Convert to density
  density <- counts / (n * bin_width)

  mid_points <- 0.5 * (breaks[-1L] + breaks[-n_breaks])

  data_structure <- structure(list(
    breaks = breaks,
    counts = counts,
    density = density,
    mid_points = mid_points),
    class = "histogram"
  )

  #---- --- ----- --- ---- --- ----#

  # Get the default pars
  op <- par(no.readonly = TRUE)


  # Add more margin space for labels
  if(labels){
    par(mar=c(7,5,5,3) + 0.1)
  }

  # Define plot arguments before plotting
  y <- if(freq) counts else density
  xlim <- if (is.null(xlim)) range(x) else xlim
  ylim <- if (is.null(ylim)) range(y) else ylim


  plot.new()
  plot.window(xlim = xlim,ylim = ylim)

  if(shape == "rect"){
    rect(breaks[-n_breaks], 0,
         breaks[-1L],y,
         col = NA,
         border = line_color,
         lwd = line_width,
         lty = line_type)
  } else if(shape == "segment"){

    # Draw the top lines of bins
    segments(x0 = breaks[-n_breaks],  # Start of each bin
             y0 = y,                  # Height of each bin (top edge)
             x1 = breaks[-1L],        # End of each bin
             y1 = y,                  # Same height for horizontal line
             col = line_color,
             lwd = line_width,
             lty = line_type)

    # Conditionally draw borders
    if (show_border == "left") {
      segments(x0 = breaks[-n_breaks],  # Left edge of bin
               y0 = 0,                  # Start at baseline
               x1 = breaks[-n_breaks],  # Same x-coordinate (vertical line)
               y1 = y,                  # Height of the bin
               col = line_color,
               lwd = line_width,
               lty = line_type)
    }

    if (show_border == "right") {
      segments(x0 = breaks[-1L],       # Right edge of bin
               y0 = 0,                 # Start at baseline
               x1 = breaks[-1L],       # Same x-coordinate (vertical line)
               y1 = y,                 # Height of the bin
               col = line_color,
               lwd = line_width,
               lty = line_type)
    }

  } else {
    # Draw histogram bin tops using segments
        segments(x0 = breaks[-n_breaks],  # Start of each bin
                   y0 = y,                # Height of each bin (top edge)
                   x1 = breaks[-1L],      # End of each bin
                   y1 = y,                # Same height to create a horizontal line
                   col = line_color,
                   lwd = line_width,
                   lty = "solid")

  }

  if (labels) {
    label_names <- label_names %||% ifelse(freq,"Frequency","Density")

    if(axes){
      axis(1,
           at = pretty(range(x, na.rm = na_rm)),
           cex.axis = label_size,
           las = 1,
           lwd = 0,
           lwd.ticks = line_width,
           tcl = -0.2)


      axis(2,
           at = pretty(range(y, na.rm = na_rm)),
           cex.axis = label_size,
           lwd = 0,
           lwd.ticks = line_width,
           tcl = -0.2,
           las = 1)
    }

  }

  if (!is.null(main)) {
    title(main = main,
          line = in_line,...)  # Add title with adjustable line spacing

    # Add axis labels
    if (!is.null(xlab) || !is.null(ylab)) {
      title(xlab = xlab, line = 2.5,...)
      title(ylab = ylab, line = 3.5,...)
    }
  }

# Restore graphical parameters
par(op)

invisible(data_structure)

}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Supply a formula of the form ~x instead of data$column

histogram.formula <- function(formula,
                              data = NULL,
                              shape = NULL,
                              show_border = NULL,
                              na_rm = TRUE,
                              breaks = "scott", # used to compute number of bins
                              freq = TRUE, # if FALSE, histogram will display density on y-axis
                              right= TRUE,
                              fuzz = NULL,
                              include_lowest = TRUE,
                              xlim = NULL,
                              ylim = NULL,
                              axes = FALSE,
                              labels = FALSE,
                              label_names = NULL,
                              label_size = NULL,
                              main = NULL,
                              in_line = 1,
                              xlab = NULL,
                              ylab = NULL,
                              line_width = 0.5,
                              line_type = "solid",
                              line_color = "gray20",
                              ...
){

  stopifnot(is.data.frame(data),
            is.logical(na_rm),
            is.logical(freq),
            is.logical(include_lowest),
            is.logical(axes),
            is.logical(labels))

  shape <- match.arg(shape,c("segment","rect","lines"))
  show_border <- match.arg(show_border,c("left","right"))

  if(shape != "segment" && !is.null(show_border))
    warning("Ignoring 'show_border' argument.Borders only applied to shape 'segment'")

  if(shape == "segment" && is.null(show_border))
    stop("Please, specify a border to be drawn for shape 'segment'")

  if(shape == "line" && line_type != "solid"){
    warning("'line_type' will be ignored. Default setting is 'solid' and cannot be changed for this shape.")
  }

  #---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
  # Handle formula
  vars <- handle_formula(formula,data)
  x <- vars$x %||% vars$y

  # Remove NA
  if(na_rm){
    x <- rid_na(x)
  }

  if(is_empty_object(x))
    stop("Data only contained NA values. Provide a valid numeric object or dataset ")

  # Remove Inf,-Inf
  x <- keep_finite(x)


  n <- length(x)

  use_breaks <- !missing(breaks)

  if(!use_breaks){
    warning("Using Scott's normal reference rule to compute breaks")
    breaks <- scott_breaks(x)
    breaks <- pretty(range(x),n = breaks)
  }

  if (is.character(breaks)) {
    breaks <- match.arg(breaks, c("sturges", "doane", "scott", "fd", "ts"))
    breaks <- switch(breaks,
                     sturges = sturges_breaks(x),
                     doane = doane_breaks(x),
                     scott = scott_breaks(x),
                     fd = fd_breaks(x),
                     ts = ts_breaks(x))
    breaks <- pretty(range(x), n = breaks)
  } else if (is.numeric(breaks) && length(breaks) == 1L) {
    breaks <- pretty(range(x), n = breaks, min.n = 1)
  } else if (is.numeric(breaks) && length(breaks) > 1L) {
    breaks <- sort(breaks)
  } else if (is.function(breaks)) {
    breaks <- breaks(x)
  } else {
    stop("Invalid breaks specification")
  }

  n_breaks <- length(breaks)
  bin_width <- diff(breaks) # Preferred over diff(range(x))/breaks, which gives only an average
  bin_width_range <- diff(range(bin_width)) # constant difference - linear seq.
  # Check if bin widths are equidistant if not specified by the user and constant difference of 0
  equidistant <- !use_breaks || all_the_same(bin_width_range, tol = 1e-8* mean(bin_width))

  # Check if bin-widths are set to zero or a negative number
  if(!use_breaks && any_zero_negative(bin_width)){
    stop("'breaks' are not strictly increasing")
  }

  # Adapted ggplot2::bin - this protects from floating point rounding errors
  fuzz <- fuzz %||% 1e-08 * stats::median(diff(breaks))
  if (!is.finite(fuzz)) { # happens when 0 or 1 finite breaks are given
    fuzz <- .Machine$double.eps * 1e3
  }
  if (right) {
    fuzzes <- c(-fuzz, rep.int(fuzz, n_breaks - 1))
  } else {
    fuzzes <- c(rep.int(-fuzz, n_breaks - 1), fuzz)
  }
  fuzzy <- breaks + fuzzes


  counts <- cut_counts(x,fuzzy,
                       right = right,
                       include.lowest = include_lowest)
  # Convert to density
  density <- counts / (n * bin_width)

  mid_points <- 0.5 * (breaks[-1L] + breaks[-n_breaks])

  data_structure <- structure(list(
    breaks = breaks,
    counts = counts,
    density = density,
    mid_points = mid_points),
    class = "histogram"
  )

  #---- --- ----- --- ---- --- ----#

  # Get the default pars
  op <- par(no.readonly = TRUE)


  # Add more margin space for labels
  if(labels){
    par(mar=c(7,5,5,3) + 0.1)
  }

  # Define plot arguments before plotting
  y <- if(freq) counts else density
  xlim <- if (is.null(xlim)) range(x) else xlim
  ylim <- if (is.null(ylim)) range(y) else ylim

  plot.new()
  plot.window(xlim = xlim,ylim = ylim)

  if(shape == "rect"){
    rect(breaks[-n_breaks], 0,
         breaks[-1L],y,
         col = NA,
         border = line_color,
         lwd = line_width,
         lty = line_type)
  } else if(shape == "segment"){

    # Draw the top lines of bins
    segments(x0 = breaks[-n_breaks],  # Start of each bin
             y0 = y,                  # Height of each bin (top edge)
             x1 = breaks[-1L],        # End of each bin
             y1 = y,                  # Same height for horizontal line
             col = line_color,
             lwd = line_width,
             lty = line_type)

    # Conditionally draw borders
    if (show_border == "left") {
      segments(x0 = breaks[-n_breaks],  # Left edge of bin
               y0 = 0,                  # Start at baseline
               x1 = breaks[-n_breaks],  # Same x-coordinate (vertical line)
               y1 = y,                  # Height of the bin
               col = line_color,
               lwd = line_width,
               lty = line_type)
    }

    if (show_border == "right") {
      segments(x0 = breaks[-1L],       # Right edge of bin
               y0 = 0,                 # Start at baseline
               x1 = breaks[-1L],       # Same x-coordinate (vertical line)
               y1 = y,                 # Height of the bin
               col = line_color,
               lwd = line_width,
               lty = line_type)
    }

  } else {
    # Draw histogram bin tops using segments
    segments(x0 = breaks[-n_breaks],  # Start of each bin
             y0 = y,                # Height of each bin (top edge)
             x1 = breaks[-1L],      # End of each bin
             y1 = y,                # Same height to create a horizontal line
             col = line_color,
             lwd = line_width,
             lty = "solid")

  }

  if (labels) {
    label_names <- label_names %||% ifelse(freq,"Frequency","Density")

    if(axes){
      axis(1,
           at = pretty(range(x, na.rm = na_rm)),
           cex.axis = label_size,
           las = 1,
           lwd = 0,
           lwd.ticks = line_width,
           tcl = -0.2)


      axis(2,
           at = pretty(range(y, na.rm = na_rm)),
           cex.axis = label_size,
           lwd = 0,
           lwd.ticks = line_width,
           tcl = -0.2,
           las = 1)
    }

  }

  if (!is.null(main)) {
    title(main = main,
          line = in_line,...)  # Add title with adjustable line spacing

    # Add axis labels
    if (!is.null(xlab) || !is.null(ylab)) {
      title(xlab = xlab, line = 2.5,...)
      title(ylab = ylab, line = 3.5,...)
    }
  }

  # Restore graphical parameters
  par(op)

  invisible(data_structure)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
#' Sturges Number-of-Bins Rule
#'
#' @description
#' Computes the number of histogram bins using Sturges' formula, which is based on a normal distribution approximation.
#'
#' @param x A numeric vector representing the data to be binned.
#'
#' @return An integer representing the number of bins.
#'
#' @examples
#' x <- rnorm(100)
#' sturges_breaks(x)
#'
#' @export
sturges_breaks <- function(x){
  n <- length(x)
  breaks <- ceiling(1 + log2(n))
  return(breaks)
}
#' Doane's Formula for Number of Bins
#'
#' @description
#' Computes the number of histogram bins using Doane's formula, which adjusts Sturges' rule to account for skewness in the data.
#'
#' @param x A numeric vector representing the data to be binned.
#'
#' @return An integer representing the number of bins.
#'
#' @examples
#' x <- rnorm(100)
#' doane_breaks(x)
#'
#' @export
# Doane's formula
doane_breaks<- function(x){
  x_bar <- mean(x)
  n <- length(x)
  m2 <- mean((x - x_bar)^2)
  m3 <- mean((x - x_bar)^3)
  g1 <- m3/(m2^(3/2))

  sigma <- sqrt(6 * (n - 2) / ((n + 1) * (n + 3)))
  breaks <- 1 + log2(n) + log2(1 + (abs(g1)/sigma))
  breaks <- ceiling(breaks)
  return(breaks)
}
#' Scott's Normal Reference Rule for Number of Bins
#'
#' @description
#' Computes the number of histogram bins using Scott's normal reference rule, which minimizes the integrated mean squared error for a normal density.
#'
#' @param x A numeric vector representing the data to be binned.
#'
#' @return An integer representing the number of bins.
#'
#' @examples
#' x <- rnorm(100)
#' scott_breaks(x)
#'
#' @export
# Scott's normal reference rule
scott_breaks <- function(x){
  sigma <- sd(x)
  n <- length(x)

  bin_width <- 3.5 * sigma * (n^(-1/3))

  x_range <- diff(range(x))
  breaks <- ceiling(x_range/bin_width)
  return(breaks)
}

#' Freedman-Diaconis Rule for Number of Bins
#'
#' @description
#' Computes the number of histogram bins using the Freedman-Diaconis rule, which is robust to outliers and uses the interquartile range (IQR) for bin width.
#'
#' @param x A numeric vector representing the data to be binned.
#'
#' @return An integer representing the number of bins.
#' @details If the interquartile range (IQR) is zero, the function falls back to Scott's rule.
#'
#' @examples
#' x <- rnorm(100)
#' fd_breaks(x)
#'
#' @export
fd_breaks <- function(x){
  iqr <- IQR(x)
  n <- length(x)

  if (iqr == 0) {
    x_sorted <- sort(x, na.last = NA)
    alpha <- 1 / 4
    alpha_min <- 1 / 512

    while (iqr == 0 && (alpha <- alpha / 2) >= alpha_min) {
      quantiles <- stats::quantile(x_sorted, c(alpha, 1 - alpha), names = FALSE)
      iqr <- diff(quantiles) / (1 - 2 * alpha)
    }
  }

  # Fall back to Scott's rule if no valid IQR is found
  if (iqr == 0) {
    bin_width <- 3.5 * sqrt(stats::var(x, na.rm = TRUE)) / n^(1/3)
  } else {
    # Freedman-Diaconis bin width
    bin_width <- 2 * iqr / n^(1/3)
  }

  # Handle edge case: bin width should not be zero or negative
  if (bin_width <= 0) {
    stop("Bin width is zero or negative. Check your data (e.g., constant values).")
  }

  x_range <- diff(range(x, na.rm = TRUE))
  breaks <- ceiling(x_range / bin_width)
  return(breaks)
}

#' Terrell-Scott Oversmoothed Histogram Formula
#'
#' @description
#' Computes the number of histogram bins using Terrell-Scott's oversmoothing method, which provides a larger bin width to reveal more structure in the data.
#'
#' @param x A numeric vector representing the data to be binned.
#' @param use_iqr Logical, whether to use the interquartile range (IQR) for a robust version of the formula. Default is `FALSE`.
#'
#' @return An integer representing the number of bins.
#'
#' @examples
#' x <- rnorm(100)
#' ts_breaks(x, use_iqr = TRUE)
#'
#' @export
ts_breaks <- function(x, use_iqr = FALSE) {
  n <- length(x)
  if (use_iqr) {
    # Robust version using IQR
    iqr <- IQR(x)

    bin_width <- 3.729 * (iqr / n^(1/3))
  } else {
    # Default version using range
    x_range <- diff(range(x))
    bin_width <- x_range / (2 * n)^(1/3)
  }

  x_range <- diff(range(x))
  breaks <- ceiling(x_range / bin_width)
  return(breaks)
}

#' Count Observations in Each Bin
#'
#' @description
#' Counts the number of observations within each bin of a histogram, given a vector of bin edges (`breaks`).
#'
#' @param x A numeric vector representing the data to be binned.
#' @param breaks A numeric vector of bin edges, or a scalar for the number of bins.
#' @param right Logical, whether bins should be right-closed. Default is `TRUE`.
#' @param include.lowest Logical, whether the lowest bin should include the smallest value. Default is `TRUE`.
#'
#' @return An integer vector containing the count of observations in each bin.
#' @details The function ensures that the `breaks` vector is strictly increasing. It uses the `cut()` function to assign bins to observations.
#'
#' @examples
#' x <- rnorm(100)
#' breaks <- seq(-3, 3, by = 1)
#' cut_counts(x, breaks)
#'
#' @export
cut_counts <- function(x,
                       breaks,
                       right = TRUE,
                       include.lowest = TRUE) {

  # Ensure 'breaks' is strictly increasing
  if (any(diff(breaks) <= 0)) {
    stop("The 'breaks' vector must be strictly increasing.")
  }

  # Use `cut` to assign bins
  bins <- cut(x,
              breaks = breaks,
              right = right,
              include.lowest = include.lowest,
              labels = FALSE)

  if(length(breaks) == 1L){
    # Count occurrences in each bin using scalar
    tabulate(bins, nbins = as.integer(breaks) - 1)
  } else{
    tabulate(bins, nbins = length(breaks) - 1)
  }
}
