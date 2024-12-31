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
gdp <- readRDS("data/gdp_data.rds")
x <- gdp$Country
y <- gdp$Year1970
z <- gdp$Year1979

set_font("WarblerText-Regular", google = FALSE, locally = TRUE)

par(mar = c(3,10,3,10))

plot.new()
plot.window(c(1970,1979), c(20,60))

# X-axis
axis(1, at = c(1970,1979),
     labels = c("1970", "1979"),
     tcl = 0.0, lwd = 0.5,
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

mtext(x, side = 2, line = 0,
      at = y, las = 2,
      cex = 0.75)
mtext(x, side = 4, line = 0,
      at = z, las = 2,
      cex = 0.75)
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

slope_chart <- function(data,...) UseMethod("slope")

slope_chart.default <- function(x = NULL,
                          y = NULL,
                          na_rm = TRUE,
                          xlim = extendrange(x,f = 0.05),
                          ylim = extendrange(y,f = 0.05),
                          consecutive_labels = FALSE,
                          jitter = FALSE,
                          highlight_slopes = FALSE,
                          main = NULL,
                          main_line = 1, # lines from margin
                          line_type = "solid",
                          line_color = "gray70",
                          line_width = 0.5,
                          axes = FALSE,
                          x_axis_labels = NULL,
                          axis_color = "gray50",
                          right_label_names = NULL,
                          left_label_names = NULL,
                          right_label_size = 1,#cex
                          left_label_size = 1, #cex
                          label_gap = 0, #axis-gap
                          right_label_pos = 2,#las
                          left_label_pos = 2, #las
                          margins = NULL, #mar
                          ...
){

  xlim <- ifelse(is.null(xlim),extendrange(x),xlim)
  ylim <- ifelse(is.null(ylim),extendrange(y),ylim)

}

slope_chart.data.frame <- function(data,
                             na_rm = TRUE,
                             xlim = extendrange(1:ncol(data),f = 0.05),
                             ylim = extendrange(y,f = 0.05),
                             main = NULL,
                             main_line = 1,
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

  xlim <- ifelse(is.null(xlim),extendrange(x),xlim)
  ylim <- ifelse(is.null(ylim),extendrange(y),ylim)

}

slope_chart.formula <- function(formula,
                          data,
                          na_rm = TRUE,
                          xlim = extendrange(),
                          ylim = extendrange(),
                          main = NULL,
                          main_line = 1,
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

