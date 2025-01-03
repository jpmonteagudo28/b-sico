#' Create Edward Tufte inspired slopegraphs using categorical and numeric data
#'
#' @export
#'

# Challenges to overcome
# 1. Vertical axis on left and right side of the plot must have numeric and
# character labels - how to position those? - done
# 2. Deal with unmolded/unshaped data - data must be in a specific format (long?)
# 3. How to trace the slopes from one point in time to the next - using segments and colors, minimally
# 4. Dealing with ties in data - have the points converge/side-by-side labels/expand axis section?
# 5. Handle colors for sub-groups - use slopes to distinguish among groups - done
# 6. Handle heavy data sets - frequency slopegraph?
# 7. Deal with crashing labels - Use NA's (empty rows) to separate rows of data - done

# Useful Resources
# https://simplexct.com/tufte-in-excel-the-slope-graph
#
# Very rough sketch of what I need
set_font("WarblerText-Regular", google = FALSE, locally = TRUE)

par(mar = c(3,10,3,10))

plot.new()

gdp <- readRDS("data/gdp_data.rds")
x <- gdp$Country
y <- jitter_labels(gdp$Year1970,x, amount = 1.5,buffer = 0.5, max_iterations = 245)$adjusted_value
z <- jitter_labels(gdp$Year1979,x, amount = 1.5,buffer = 0.5, max_iterations = 245)$adjusted_value

plot.window(c(1970,1979), c(20,60))

# X-axis
axis(1, at = c(1970,1979),
     labels = c("1970", "1979"),
     tcl = 0.0, lwd = 0,
     col.axis = "gray20",
     cex.axis = 0.75) # Set axis label color

# Y-axis (left)
axis(2, at = y,
     labels = round(y,0),
     line = -3,
     tcl = -0.05,
     lwd = 0,
     las = 2,
     gap.axis = 1,
     col.axis = "gray50",
     cex.axis = 0.75) # Set axis label color

# Y-axis (right)
axis(4, at = z, labels = round(z,0), line = -3,
     tcl = -0.05, lwd = 0,
     las = 2,
     gap.axis = 1,
     col.axis = "gray50",
     cex.axis = 0.75) # Set axis label color

segments(1970 + .15, y, 1979 - .15, z, # Don't let segments touch axis
         col = "gray70",
         lty = "solid",
         lwd = 0.5)
# Left labels
mtext(x, side = 2, line = 0,
      at = y, las = 2,
      cex = 0.75)
# Right labels
mtext(x, side = 4, line = 0,
      at = z, las = 2,
      cex = 0.75)
# Title
title(main = "Current Receipts of Government as a \nPercentage of Gross Domestic \nProduct, 1970 and 1979",
      cex.main = .80,
      font.main = 3,
      adj = 0.05,
      line = -5,
      outer = TRUE)

# Tufte handles overlapping in two ways:
# 1: Create separate labels for each overlapping point (i.e, Canada & Belgium overlap on the left, but
# he created a separate label for each and still used the same numeric values even though they're
#  placed on different points in the scale)
#  2: Align the labels horizontally on one point

slope_chart <- function(x,...) UseMethod("slope")

slope_chart.default <- function(x = NULL,     # Years/time
                          y = NULL,           # Values
                          na_rm = TRUE,
                          xlim = NULL,
                          ylim = NULL,
                          consecutive_labels = FALSE,
                          within_labels_gap = NULL,
                          jitter = FALSE,
                          amount = NULL,
                          highlight_slopes = FALSE,
                          line_type = "solid",
                          slope_color = NULL,
                          line_width = 0.5,
                          axes = FALSE,
                          x_axis_labels = NULL,
                          axis_color = "gray50",
                          title = NULL,
                          title_line = 1,      # lines from margin
                          label_names = NULL,  # character labels on y-axis
                          label_size = 1,      # cex
                          label_gap = 0,       # axis-gap
                          label_pos = 2,       # las
                          margins = NULL,      # mar
                          ...
){
  # Sanity checks
  stopifnot(is.numeric(x) ||is.character(x) || is.factor(x),
            is.numeric(y),
            length(x) == length(y),
            length(x) >= 2,
            length(y) >= 2,
            !is.null(x),
            !is.null(y))

  #---- --- ---- --- ---- ---- --- ---- --- ---- ---- ---#
  # Getting the data organized and transformed

  #---- --- ---- --- ---- ---- --- ---- --- ---- ---- ---#
  # Getting the data range for plot window
  xlim <- ifelse(is.null(xlim),extendrange(x),xlim)
  ylim <- ifelse(is.null(ylim),extendrange(y),ylim)

  #---- --- ---- --- ---- ---- --- ---- --- ---- ---- ---#
  # Working the slope and colors
  if (highlight_slopes) {
    # Assign default slope colors if none are provided
    slope_color <- slope_color %||% c("#D41159", "gray30", "gray70")

    # Calculate and assign colors based on slopes
    colors <- color_by_slope(x = x,
                             y = y,
                             cols = slope_color,
                             na_rm = na_rm)
  } else {
    # Assign a default or user-specified single color for non-highlighted slopes
    slope_color <- slope_color %||% "gray70"
    colors <- rep(slope_color, length(x) - 1)
  }

  # Plot the segments with the assigned colors
  for (i in seq_along(colors)) {
    segments(x[i], y[i], x[i + 1], y[i + 1],
             col = colors[i],
             lty = line_type,
             lwd = line_width)
  }
  #---- --- ---- --- ---- ---- --- ---- --- ---- ---- ---#

}

slope_chart.data.frame <- function(data,       # data frame of x and y values (2 columns)
                             na_rm = TRUE,
                             xlim = NULL, # based on wide data frame input
                             ylim = NULL,
                             title = NULL,
                             title_line = 1,
                             line_type = "solid",
                             line_color = "gray20",
                             line_width = 0.5,
                             axes = FALSE,
                             x_axis_labels = NULL,
                             right_label_names = NULL,
                             left_label_names = NULL,
                             right_label_size = 1,
                             left_label_size = 1,
                             label_gap = 1,
                             right_label_pos = 4,
                             left_label_pos = 2,
                             margins = NULL,
                             ...){

  stopifnot(is.data.frame(data),
            )

  xlim <- ifelse(is.null(xlim),extendrange(x),xlim)
  ylim <- ifelse(is.null(ylim),extendrange(y),ylim)

}

slope_chart.formula <- function(formula,  # formula of x and y values, data frame can have more than two columns
                          data,
                          na_rm = TRUE,
                          xlim = NULL,
                          ylim = NULL,
                          title = NULL,
                          title_line = 1,
                          line_type = "solid",
                          line_color = "gray20",
                          line_width = 0.5,
                          axes = FALSE,
                          x_axis_labels = NULL,
                          right_label_names = NULL,
                          left_label_names = NULL,
                          right_label_size = 1,
                          left_label_size = 1,
                          label_gap = 1,
                          right_label_pos = 2,
                          left_label_pos = 2,
                          margins = NULL,
                          ...){

  xlim <- if(is.null(xlim)) extendrange(x) else xlim
  ylim <- if(is.null(ylim)) extendrange(y) else ylim

  vars <- handle_formula(formula,data)
  x <- vars$x
  y <- vars$y

}

