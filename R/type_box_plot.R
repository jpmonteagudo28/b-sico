#' Edward Tufte Style Boxplot
#'
#' Draws a minimalist boxplot inspired by Edward Tufte's visualization principles. 
#' The function supports customization for horizontal or vertical orientation, 
#' additional mean markers, and labeled groups.
#'
#' @param formula A formula of the form `y ~ x`, where `y` is the grouping variable, and `x` is the numeric variable(s).
#' @param .data A data frame containing the variables specified in the formula.
#' @param labels Logical; if `TRUE`, adds axis labels corresponding to the grouping variable.
#' @param label_names Optional vector of names for the labels (defaults to the levels of `y`).
#' @param label_size Numeric; size of the axis labels.
#' @param main Character; title of the plot.
#' @param in_line Numeric; line spacing for the title.
#' @param xlab Character; labels for the x- and y-axes.
#' @param ylab Character; labels for the x- and y-axes.
#' @param n_ticks Numeric; number of ticks for the axis representing the numeric variable.
#' @param horizontal Logical; if `TRUE`, creates a horizontal boxplot (default is `TRUE`).
#' @param line_type Character; type of line for whiskers and box edges (e.g., `"solid"`).
#' @param line_color Character; color of the boxplot lines (default is `"gray20"`).
#' @param median_symbol Character or numeric; symbol used to represent the median.
#' @param median_color Character; color of the median symbol.
#' @param add_mean Logical; if `TRUE`, adds a marker for the mean value.
#' @param mean_symbol Character or numeric; symbol used to represent the mean (default is `1`).
#' @param mean_color Character; color of the mean symbol (default is `"red"`).
#' @param outlier_symbol Character or numeric; symbol used to represent outliers.
#' @param outlier_size Numeric; size of the outlier symbols.
#' @param ... Additional graphical parameters passed to `par`.
#'
#' @details
#' This function generates a boxplot with reduced visual clutter, suitable for data comparison in publications or presentations.
#' Outliers are displayed as individual points, and optional mean markers can be included.
#'
#' @return A Tufte-style boxplot is drawn on the active graphical device.
#'
#' @examples
#' # Basic Example
#' data(mtcars)
#' box_plot(mpg ~ cyl, .data = mtcars, main = "MPG by Cylinder")
#'
#' # Horizontal Example with Mean Marker
#' box_plot(mpg ~ cyl, .data = mtcars, horizontal = TRUE, add_mean = TRUE)
#'
#' @export

box_plot <- function(formula,
                     .data,
                     labels = TRUE,
                     label_names = NULL,
                     label_size = 1.2,
                     main = NULL,
                     in_line = 1,
                     xlab = NULL,
                     ylab = NULL,
                     n_ticks = 5,
                     horizontal = TRUE,
                     line_type = "solid",
                     line_color = "gray20",
                     median_symbol = "|",
                     median_color = "black",
                     add_mean = TRUE,
                     mean_symbol = 1,
                     mean_color = "red",
                     outlier_symbol = 1,
                     outlier_size = 0.5,
                     ...
){
  stopifnot(
    is.data.frame(.data),
    is.logical(labels),
    is.logical(horizontal),
    is.character(line_type),
    is.character(line_color),
    is.logical(add_mean),
    is.numeric(outlier_size)
  )
  
  # Extract the dependent variable (LHS of the formula)
  group_var <- as.character(formula[[2]])
  
  if (!is.formula(formula))
    stop("Please, provide a valid formula object of the form 'y~x'")
  
  rhs_vars <- all.vars(formula[[3]])
  n_vars <- length(all.vars(formula))
  
  # Handle "." in RHS
  if (length(rhs_vars) == 1 && rhs_vars == ".") {
    rhs_vars <- setdiff(names(.data), group_var)
  }
  
  # Get the default pars
  op <- par(no.readonly = TRUE)
  
  for (iv in rhs_vars) {
    x <- .data[[iv]]
    y <- .data[[group_var]]
    
    # Ensure valid data
    valid_data <- complete.cases(x,y)
    y <- y[valid_data]
    x <- x[valid_data]
    
    # Convert grouping variable to factor if necessary
    if (!is.factor(y)) {
      y <- as.factor(y)
    }
    n_levels <- nlevels(y)
    
    if(labels){
      par(mar=c(5,12,4,2) + 0.1)
    }
    
    # Start plotting
    plot.new()
    
    if (horizontal) {
      plot.window(range(x), c(0.5, n_levels + 0.5))
    } else {
      plot.window(c(0.5, n_levels + 0.5), range(x))
    }
    
    for (i in seq_len(n_levels)) {
      level <- levels(y)[i]
      x_subset <- x[y == level]  # Subset x by the level of y
      
      # Calculate boxplot stats for this level
      stats <- calculate_box_stats(x_subset, add_mean = add_mean)
      
      if (horizontal) {
        
        # Horizontal boxplot
        segments(stats$lower_whisker, i, stats$q1, i, lwd = 1, col = line_color)
        segments(stats$q3, i, stats$upper_whisker, i, lwd = 1, col = line_color)
        
        points(stats$median, i, pch = median_symbol, cex = 0.851, col = median_color)
        
        if (add_mean) {
          points(stats$mean, i, pch = mean_symbol, cex = 0.851, col = mean_color)
        }
        
        if (length(stats$outliers) > 0) {
          points(stats$outliers, rep(i, length(stats$outliers)),
                 pch = outlier_symbol, cex = outlier_size, col = line_color)
        }
      } else {
        
        # Vertical boxplot
        segments(i, stats$lower_whisker, i, stats$q1, lwd = 1, col = line_color)
        segments(i, stats$q3, i, stats$upper_whisker, lwd = 1, col = line_color)
        
        points(i, stats$median, pch = median_symbol, cex = 0.851, col = median_color)
        
        if (add_mean) {
          points(i, stats$mean, pch = mean_symbol, cex = 0.851, col = mean_color)
        }
        
        if (length(stats$outliers) > 0) {
          points(rep(i, length(stats$outliers)), stats$outliers,
                 pch = outlier_symbol, cex = outlier_size, col = line_color)
        }
      }
    }
    
    # Add labels
    if (labels) {
      label_names <- label_names %||% levels(y)
      
      if (horizontal) {
        axis(2, 
             at = seq_len(n_levels), 
             labels = label_names, 
             cex.axis = label_size, 
             las = 1, 
             lwd = 0, 
             tcl = 0,
             ...)
        
        axis(1, 
             at = pretty(range(x, na.rm = TRUE), n = n_ticks), 
             cex.axis = label_size,
             lwd = 0, 
             tcl = -0.03,
             ...)
        
      } else {
        axis(1, 
             at = seq_len(n_levels), 
             labels = label_names, 
             cex.axis = label_size, 
             las = 1, 
             lwd = 0, 
             tcl = 0,
             ...)
        axis(2, 
             at = pretty(range(x, na.rm = TRUE), n = n_ticks), 
             cex.axis = label_size, 
             lwd = 0, 
             tcl = -0.03,
             las = 1,
             ...)
      }
    }
    
    if (!is.null(main)) {
      title(main = main, line = in_line , ...)  # Add title with adjustable line spacing
    }
    # Add axis labels
    if (!is.null(xlab) || !is.null(ylab)) {
      if (horizontal) {
        title(xlab = xlab, line = 3.5,...)
        title(ylab = ylab, line = 4.5,...)
      } else {
        title(ylab = xlab, line = 3.5,...)
        title(xlab = ylab, line = 4.5,...)
      }
    }
  }
  # Restore graphical parameters
  par(op)
}

#' Calculate Statistics for boxplot
#'
#' Computes the key statistics required to draw a boxplot, including the interquartile range, whiskers, and outliers.
#'
#' @param .data A numeric vector or data frame column for which to calculate boxplot statistics.
#' @param add_mean Logical; if `TRUE`, includes the mean in the output.
#' @param na_rm Logical; if `TRUE`, removes `NA` values before computation.
#'
#' @return A list containing:
#' - `q1`: First quartile (25th percentile).
#' - `q3`: Third quartile (75th percentile).
#' - `median`: Median value.
#' - `lower_whisker`: Lower whisker value.
#' - `upper_whisker`: Upper whisker value.
#' - `outliers`: Vector of outlier values.
#' - `mean`: (Optional) Mean value if `add_mean = TRUE`.
#'
#' @examples
#' # Example with Numeric Vector
#' stats <- calculate_box_stats(c(1, 2, 3, 4, 5, 100))
#' print(stats)
#'
#' @export
#'
calculate_box_stats <- function(.data, 
                                add_mean = TRUE,
                                na_rm = TRUE){
  
  stopifnot(is.numeric(.data) || is.data.frame(.data))
  
  q1 <- quantile(.data,.25, na.rm = na_rm)
  q3 <- quantile(.data,.75, na.rm = na_rm)
  median <- median(.data,na.rm = na_rm)
  iqr <- q3 - q1
  lower_whisker <- max(min(.data, 
                           na.rm = na_rm), q1 - 1.5*iqr,
                       na.rm = na_rm)
  upper_whisker <- min(max(.data,
                           na.rm = na_rm),q3 + 1.5*iqr,
                       na.rm = na_rm)
  outliers <- na.omit(
    .data[.data < lower_whisker | .data > upper_whisker]
  )
  
  stats <-  list(q1 = q1, q3 = q3,
                 median = median,
                 lower_whisker = lower_whisker,
                 upper_whisker = upper_whisker,
                 outliers = outliers)
  
  if (add_mean) {
    stats$mean <- mean(.data, na.rm = na_rm)
  }
  
  return(stats)
}