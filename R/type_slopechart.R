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

par(mar = c(3,7,3,7))

plot.new()
plot.window(c(1970,1979), c(20,60))

axis(1, at = c(1970,1979),
     labels = c("1970", "1979"),
     tcl = 0.07, lwd = 0.5,
     family = "serif")
axis(2, at = y, labels = round(y,0),line = -3,
     tcl = -0.05, lwd = 0,
     family = "serif", las = 2,
     gap.axis = 1, col.xaxt = "gray50")
axis(4, at = z, labels = round(z,0),line = -3 ,
     tcl = -0.05, lwd = 0, las = 2,
     family = "serif", gap.axis = 1,
     col.xaxt = "gray50")

segments(1970, y, 1979, z,
         col = "gray70",
         lty = "solid",
         lwd = 0.5)

mtext(x, side = 2, line = 0,
      at = y, las = 2,
      family = "serif")
mtext(x, side = 4, line = 0,
      at = z, las = 2,
      family = "serif")

# Tufte handles overlapping in two ways:
# 1: Create separate labels for each overlapping point (i.e, Canada & Belgium overlap on the left, but
# he created a separate label for each and still used the same numeric values even though they're
#  placed on different points in the scale)
#  2: Align the labels horizontally on one point

slope <- function(data,...)
  UseMethod("slope")

slope.default <- function(x = NULL,
                          y = NULL,
                          na_rm = TRUE,
                          xlim = extendrange(x,f = 0.05),
                          ylim = extendrange(y,f = 0.05),
                          main = NULL,
                          main_line = 1, # lines from margin
                          line_type = "solid",
                          line_color = "gray20",
                          line_width = 0.5,
                          axes = FALSE,
                          x_axis_labels = NULL,
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

slope.data.frame <- function(data,
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

slope.formula <- function(formula,
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

