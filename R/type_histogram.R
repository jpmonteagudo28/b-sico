#' Edward Tufte style histogram
#'
#' @description
#' Visualize the distribution of a single **continuous** variable
#'

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
                      na_rm = TRUE,
                      breaks = "Scott", # used to compute number of bins
                      freq = TRUE, # if FALSE, histogram will display density on y-axis
                      closed_on = "right",
                      include_lowest = TRUE,
                      labels = TRUE,
                      label_names = NULL,
                      label_size = NULL,
                      main = NULL,
                      in_line = 1,
                      xlab = NULL,
                      ylab = NULL,
                      n_ticks = NULL,
                      axis_type = NULL,
                      line_type = "solid",
                      line_color = "gray20"
){

  stopifnot(is.numeric(x),
            is.logical(na_rm),
            is.logical(freq),
            is.logical(include_lowest),
            is.logical(labels))

  closed_on <- match.arg(closed_on,c("right","left"))

  # Check for identical data points
  if(all_the_same(x)){
    stop("All data values are identical. Histogram bins cannot be calculated.")
  }

  # Remove NA
  if(na_rm){
    x <- rid_na(x)
  }
  # Remove Inf,-Inf
  x <- keep_finite(x)

  n <- length(x)

  use_breaks <- !missing(breaks)

  if(!use_breaks){
    warning("Using Scott's normal reference rule to compute breaks")
    breaks <- scott_breaks(n)
  }

  if(use_breaks){

    if(!is.finite(breaks) || length(breaks) < 1L){
      stop("Invalid number of breaks")
    }
    else if(is.numeric(breaks) && length(breaks) == 1L){
      breaks <- pretty(range(x),n = breaks,min.n = 1)
      n_breaks<- length(breaks)
    }
    else if(is.numeric(breaks) && length(breaks) > 1L){
      breaks <- sort(breaks)
      n_breaks <- length(breaks)
    }
  }
    else if(is.character(breaks)){

    breaks <- match.arg(breaks,
                        c("sturges","doane",
                          "scott","fd","ts"))

    breaks <- switch(breaks,
                     sturges = sturges_breaks(x),
                     doane = doane_breaks(x),
                     scott = scott_breaks(x),
                     fd = fd_breaks(x),
                     ts = ts_breaks(x))
    }
  else if (is.function(breaks)) {
    breaks <- breaks(x)
  } else {
    stop("Invalid breaks specification")
  }

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
  if (closed_on == "right") {
    fuzzes <- c(-fuzz, rep.int(fuzz, n_breaks - 1))
  } else {
    fuzzes <- c(rep.int(-fuzz, n_breaks - 1), fuzz)
  }
  fuzzy <- breaks + fuzzes

  if(freq){
    counts <- cut_counts(x,fuzzy,
                         right = closed_on,
                         include.lowest = include_lowest)
  } else {
    counts <- cut_counts(x, fuzzy,
                         right = closed_on == "right",
                         include.lowest = include_lowest)
    # Convert to density
    density <- counts / (n * bin_width)
  }
  mid_points <- 0.5 * (breaks[-1L] + breaks[-n_breaks])

  data_structure <- structure(list(
    breaks = breaks,
    counts = counts,
    density = density,
    mid_points = mid_points),
    class = "histogram"
  )

  #---- --- ----- --- ---- --- ----#
  # Add more margin space for labels
  if(labels){
    par(mar=c(9,5,5,3) + 0.1)
  }

  # Define plot arguments before plotting
  xlim <- range(x)
  y <- ifelse(freq,data_structure$counts,data_structure$density)
  ylim <- (range(y))
  n_breaks <- data_structure$breaks



  plot.new()
  plot.window(xlim = xlim,ylim = ylim)














}

histogram.formula <- function(formula,
                              data = NULL,
                              breaks = "Sturges", # used to compute number of bins
                              freq = TRUE, # if FALSE, histogram will display density on y-axis
                              closed_on = "right",
                              include_edges = TRUE, # include lowest and highest edge in first & last intervals
                              labels = TRUE,
                              label_names = NULL,
                              label_size = 1.2,
                              main = NULL,
                              in_line = 1,
                              xlab = NULL,
                              ylab = NULL,
                              n_ticks = 5,
                              axis_type = NULL,
                              line_type = "solid",
                              line_color = "gray20"
){

}

# Sturges number-of-bins rule
sturges_breaks <- function(x){
  n <- length(x)
  breaks <- 1 + log2(n)
  return(breaks)
}

# Doane's formula
doane_breaks<- function(x){
  x_bar <- mean(x)
  n <- length(x)
  m2 <- mean((x - x_bar)^2)
  m3 <- mean((x - x_bar)^3)
  g1 <- m3/(m2^(3/2))

  sigma <- sqrt(6 * (n - 2) / ((n + 1) * (n + 3)))
  breaks <- 1 + log2(n) + log2(1 + (abs(g1)/sigma))
  return(breaks)
}

# Scott's normal reference rule
scott_breaks <- function(x){
  sigma <- sd(x)
  n <- length(x)

  bin_width <- 3.5 * sigma * (n^(-1/3))

  x_range <- diff(range(x))
  breaks <- ceiling(x_range/bin_width)
  return(breaks)
}

# Friedman-Diaconis normal reference rule
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

# Terrell-Scott oversmoothed histogram formula
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

# Count observations in each bin
cut_counts <- function(x,
                       breaks,
                       right = right,
                       include.lowest = include_lowest) {

  bin_indices <- findInterval(x, breaks,
                              rightmost.closed = right,
                              all.inside = include.lowest)

  # Count occurrences in each bin
  tabulate(bin_indices, nbins = length(breaks) - 1)
}
