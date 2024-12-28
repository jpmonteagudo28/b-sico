#' Create Edward Tufte inspired slopegraphs using categorical and numeric data
#'
#' @export
#'

# Challenges to overcome
# 1. Vertical axis on left and right side of the plot must have numeric and
# character labels - how to position those?
# 2. Deal with unmolded/unshaped data - data must be in a specific format (long?)
# 3. How to trace the slopes from one point in time to the next - using segments
# 4. Dealing with ties in data - have the points converge?
# 5. Handle colors for sub-groups - use slopes to distinguish among groups
# 6. Handle heavy data sets - frequency slopegraph?
# 7. Deal with crashing labels - Use NA's (empty rows) to separate rows of data

# Useful Resources
# https://simplexct.com/tufte-in-excel-the-slope-graph
#

slope <- function(data,...)
  UseMethod("slope")

slope.default <- function(x = NULL,
                          y = NULL,
                          na_rm = TRUE,
                          xlim = extendrange(x,f = 0.05),
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
                          right_label_pos = 2,
                          left_label_pos = 2,
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

