#' Create Edward Tufte inspired slopegraphs using categorical and numeric data
#'
#' @export
#'

# Challenges to overcome
# 1. Vertical axis on left and right side of the plot must have numeric and
# character labels - how to position those?
# 2. Deal with unmolded/unshaped data - data must be in a specific format (long?)
# 3. How to trace the slopes from one point in time to the next - using segments
# 4. Dealing with ties in data - have the points converge/side-by-side labels?
# 5. Handle colors for sub-groups - use slopes to distinguish among groups
# 6. Handle heavy data sets - frequency slopegraph?
# 7. Deal with crashing labels - Use NA's (empty rows) to separate rows of data

# Useful Resources
# https://simplexct.com/tufte-in-excel-the-slope-graph
#
# Very rough sketch of what I need
gdp <- readRDS("data/gdp_data.rds")
par(mar = c(3,7,3,7))
plot.new()
plot.window(c(1970,1979), c(20,60))
axis(1, at = c(1970,1979), labels = c("1970", "1979"), tcl = 0.07, lwd = 0.5, family = "serif")
axis(2, at = gdp$Year1970, labels = gdp$Year1970,line = -3, tcl = -0.05, lwd = 0, family = "serif", las = 2, gap.axis = 0.5)
axis(4, at = gdp$Year1979, labels = gdp$Year1979,line = -3 ,tcl = -0.05, lwd = 0, las = 2, family = "serif", gap.axis = .5)
segments(1970, gdp$Year1970, 1979, gdp$Year1979, col = "gray70", lty = "solid", lwd = 0.5)
mtext(gdp$Country, side = 2, line = 0, at = gdp$Year1970, las = 2, family = "serif")
mtext(gdp$Country, side = 4, line = 0, at = gdp$Year1979, las = 2, family = "serif")

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
                             left_label_pos = 2,){

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
                          left_label_pos = 2,){

  xlim <- ifelse(is.null(xlim),extendrange(x),xlim)
  ylim <- ifelse(is.null(ylim),extendrange(y),ylim)

  vars <- handle_formula(formula,data)
  x <- vars$x
  y <- vars$y

}

#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Use function to determine position of text labels when consecutive
# label check returns TRUE and `use_consecutive_labels` set to TRUE
# Calculate length(in pixels) of character string
# Can take units(user", "inches", "figure"),font(1,2,3,4), and `cex` arguments
char_pixels <- function(.x,...) strwidth(.x,...)

#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# within_labels_gap in number of lines of margin
# To be supplied to `line` argument in `mtext`
# Ideally, the function will calculate each label position
# A more intuitive, but inflexible and less useful option
# is to find the maximum char. width in pixels and set every
# label at that distance + 0.5, but the whites pace may be too
# big for some labels
consecutive_label_position <- function(.x, within_labels_gap,...){
  char_len <- char_pixels(.x,...)


}
#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Recognize whether the labels should be consecutive or not based
# on numeric value of each character string
# Here use the height of the string
# Through testing a difference in values equal to 0.67 is the
# acceptable minimum difference for labels to not be consecutive
# on the same row
should_be_consecutive <- function(labels,values,cex,...){

  # Calculate the height of a single label using strheight()
  # Use floor() because labels will not overlap if diff in values
  # is close to 1
  label_height <- floor(strheight("M", cex = cex,...))

  # Initialize a logical vector to track overlapping labels
  overlaps <- logical(length(values))

  # Loop through each label and compare its distance to others
  for (i in seq_along(values)) {
    for (j in seq_along(values)) {
      if (i != j) { # Avoid comparing a label with itself
        # Check if the absolute difference in numeric values is smaller than label height
        if (abs(values[i] - values[j]) < label_height) {
          overlaps[i] <- TRUE
          break # No need to check further for this label
        }
      }
    }
  }

  return(overlaps)
}
