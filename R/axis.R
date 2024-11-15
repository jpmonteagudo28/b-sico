#' Draw a Custom Axis with Formatted Labels
#'
#' This function adds a custom axis to a base graphics plot with user-defined formatting
#' for tick labels. It supports multiple formats, such as percentages, currency, dates,
#' and scientific notation.
#'
#' @param format A character string specifying the format of the axis labels. Options include:
#'   `"percent"`, `"dollar"`, `"date"`, `"POSIXt"`, `"integer"`, and `"scientific"`. Default is `"integer"`.
#' @param sides A character vector indicating which sides of the plot to draw the axis on.
#'   Options are `"bottom"`, `"left"`, `"top"`, and `"right"`. Default is `"bottom"`.
#' @param line_width Numeric. The width of the axis line. Default is `0.5`.
#' @param at Optional. A numeric vector specifying the exact positions for ticks. If `NULL`,
#'   positions are determined automatically using `pretty()` on `axTicks()`.
#' @param ... Additional graphical parameters passed to the `axis()` function.
#'
#' @details
#' The function creates a custom axis with tick marks on the specified sides of the plot.
#' The labels can be formatted according to different styles using the `format` parameter.
#'
#' The axis labels are formatted using the `format_axis()` function, allowing for different
#' display formats, including percentages, currency, and dates.
#'
#' @return The function is used for its side effects (modifying the plot). It does not
#' return a value.
#'
#' @examples
#' # Create a basic plot without default axes
#' plot(mpg ~ wt, data = mtcars, type = "p", axes = FALSE)
#' basic_axis(format = "integer", sides = "bottom")
#' basic_axis(format = "percent", sides = "left")
#'
#' @seealso \code{\link{axis}}, \code{\link{format_axis}}
#' @export

basic_axis <- function(format = "integer",
                     sides = "bottom",
                     line_width = 0.5,...,
                     at = NULL){

  args <- list(...)

  format <- match.arg(format,
                      c("percent","dollar","date",
                        "POSIXt","integer","scientific"),
                      several.ok = FALSE)

  sides <- match.arg(sides,
                     c("bottom","left","top","right"),
                     several.ok = TRUE)

  stopifnot(is.numeric(line_width),
            is.character(format),
            is.character(sides)
  )

  side_map <- c(bottom = 1, left = 2,
                top = 3, right = 4)
  numeric_sides <- side_map[sides]

  for (side in numeric_sides) {

    # Generate tick positions if `at` is not specified
    ticks <- if (is.null(at)) {
      pretty(graphics::axTicks(side))
    } else {
      at
    }

    # Format tick labels
    formatted_labels <- format_axis(ticks, format)

    axis_args <- c(
      list(
        side = side,
        at = ticks,
        labels = formatted_labels,
        lwd = line_width),
      args
    )

    # Apply custom axis with formatted labels
    do.call(graphics::axis, axis_args)
  }
}

#' Format Axis Labels Based on User-Specified Format
#'
#' This function formats a numeric vector according to the specified format, supporting
#' various styles like percentages, currency, dates, and scientific notation.
#'
#' @param .x A numeric vector to be formatted.
#' @param format A character string specifying the format type. Options include:
#'   `"percent"`, `"dollar"`, `"date"`, `"POSIXt"`, `"integer"`, and `"scientific"`.
#' @param digits Optional. An integer specifying the number of decimal places to round
#'   for `"integer"` formatting. Default is `NULL`.
#' @param ... Additional arguments passed to the formatting functions.
#'
#' @details
#' The function supports formatting in the following ways:
#' - `"percent"`: Converts the values to percentages (e.g., `0.25` becomes `"25%"`).
#' - `"dollar"`: Converts the values to currency (e.g., `100` becomes `"$100"`).
#' - `"date"`: Converts numeric values to `Date` objects.
#' - `"POSIXt"`: Converts numeric values to `POSIXct` datetime objects.
#' - `"integer"`: Rounds values to the nearest integer. If `digits` is provided, it rounds
#'   to the specified number of decimal places.
#' - `"scientific"`: Converts values to scientific notation.
#'
#' @return A character vector of formatted labels.
#'
#' @examples
#' # Format values as percentages
#' format_axis(c(0.1, 0.25, 0.75), format = "percent")
#'
#' # Format values as currency
#' format_axis(c(100, 250, 500), format = "dollar")
#'
#' # Format dates
#' format_axis(c(19000, 19500), format = "date")
#'
#' @seealso \code{\link{basic_axis}}
#' @export

format_axis <- function(.x,format,digits = NULL,...){

  .x <- switch(format,
               percent = sprintf("%1.0f%%", 100 * .x),
               dollar = sprintf("$%s", .x),
               date = as.Date(.x, format = "%D"),
               POSIXt = as.POSIXct(.x, format = "%D"),
               integer = ifelse(!is.null(digits),
                                round(as.integer(.x),
                                digits = digits),
                                as.integer(.x)),
               scientific = sprintf("%e", .x)
  )

  return(.x)
}

#' Range Frame Axis with Summary Statistics and Custom Ticks
#'
#' Adds a minimalist axis to a plot, inspired by Edward Tufte's range frames,
#' displaying summary statistics (min, Q1, median, mean, Q3, max) and custom ticks.
#' Supports both linear and logarithmic scales, with customizable colors,
#' tick positions, and axis properties.
#'
#' @param .x A numeric vector containing the data for which the summary statistics are calculated.
#' @param sides A character vector specifying which sides of the plot to draw the axis on.
#'   Options include `"bottom"`, `"left"`, `"top"`, and `"right"`. Defaults to `c("bottom", "left")`.
#' @param at Optional numeric vector specifying tick positions. If `NULL`,
#'   ticks are generated based on the summary statistics of `.x`.
#' @param draw_lines Logical indicating whether to draw lines extending from the axis for
#'   the lower and upper quartiles. Defaults to `FALSE`.
#' @param line_width Numeric value specifying the width of the drawn lines if `draw_lines = TRUE`.
#'   Defaults to `0`.
#' @param tick_width Numeric value for the width of tick marks. Defaults to `0.7`.
#' @param tick_gap Numeric value specifying the minimum gap between tick marks, in plot coordinates.
#'   Defaults to `0.5`.
#' @param tick_length Numeric value specifying the length of tick marks. Negative values indicate
#'   that ticks are drawn inward from the axis. Defaults to `-0.5`.
#' @param tick_color Color for the tick marks. Defaults to `"gray30"`.
#' @param mean_color Color for the label of the mean value. Defaults to `"red"`.
#' @param axis_gap Numeric value specifying the gap between the axis and the Q1/Q3 lines.
#'   Used for both linear and logarithmic scales. Defaults to `0.05`.
#' @param median_gap Numeric value specifying the gap between the median and its label.
#'   Used for both linear and logarithmic scales. Defaults to `0.05`.
#' @param digits Integer specifying the number of decimal places to round the tick labels.
#'   Defaults to `2`.
#'   @param label_size size of axis labels to be passed to `mtext`. Defaults to `0.9`
#' @param ... Additional arguments to be passed to other plotting functions.
#'
#' @details
#' This function is designed for minimalist data visualization, focusing on the
#' essential summary statistics of the data. It is especially useful for emphasizing
#' key statistical values while maintaining a clean, uncluttered plot appearance.
#'
#' The function handles both linear and logarithmic scales seamlessly, automatically
#' adjusting the positioning of summary statistics, labels, and tick marks based on
#' the scale of the axis.
#'
#' If the `draw_lines` parameter is set to `TRUE`, the function draws lines extending
#' from the axis to represent the quartiles. The axis labels include the mean, median,
#' Q1, and Q3 values, with the mean highlighted using the specified `mean_color`.
#'
#' @return Invisibly returns `NULL`, as the primary purpose is to modify the existing plot.
#'
#' @examples
#' # Basic usage with a simple vector
#' x <- rnorm(100)
#' plot(x, type = "n")
#' range_frame_axis(x, sides = c("bottom", "left"), draw_lines = TRUE)
#'
#' # Using logarithmic scale
#' x <- rexp(100)
#' plot(x, log = "y", type = "n")
#' range_frame_axis(x, sides = c("left", "bottom"), draw_lines = TRUE)
#'
#' # Create range frame plot with custom font
#' x <- mtcars$wt
#' y <- mtcars$mpg
#' set_font("Alegreya")
#' plot.new()
#' plot.window(range(x),range(y))
#' points(x,y, pch = 18, col = "black")
#' range_frame_axis(y,"left")
#'  range_frame_axis(x,"bottom")
#' mtext(
#'       "Car weight (lb/1000) v. \nMiles per gallon",
#'       side = 3,
#'       at = 4,
#'       line = -4.5,
#'       adj = 0,
#'       cex = 1.2,
#'       col = "black"
#'   )
#'
#'
#'x <- datasets::Puromycin$conc
#'y <- datasets::Puromycin::rate
#'
#' set_font("Cormorant)
#' plot.new()
#' plot.window(range(x), range(y))
#'
#' # Fit a loess regression model
#' loess_fit <- loess(y ~ x)
#' pred <- predict(loess_fit, interval = "confidence", level = 0.95)
#'
#' # Generate predictions along with confidence intervals
#' pred <- predict(loess_fit, se = TRUE)
#' x_seq <- seq(min(x), max(x), length.out = length(x))
#'
#' # Extract fitted values and confidence intervals
#' y_fit <- pred$fit
#' ci_upper <- y_fit + 1.96 * pred$se.fit
#' ci_lower <- y_fit - 1.96 * pred$se.fit
#'
#' # Add the smooth line
#'  lines(x_seq, y_fit, col = "red", lwd = 2)
#'
#' # Add confidence interval as a shaded area
#' polygon(c(x_seq, rev(x_seq)),
#'        c(ci_upper, rev(ci_lower)),
#'       col = adjustcolor("lightpink", alpha.f = 0.3), border = NA)
#' # Add plot title
#' mtext(
#'   text = "Predicted Reaction rate (min) vs.\nConcentration (ppm) \nof Puromycin",
#'  side = 3,
#'  at = c(180, 0.56),
#'  line = -8,
#'  col = "black"
#')
#' @seealso \code{\link{axis}}, \code{\link{summary}}, \code{\link{lines}}
#'
#' @export

range_frame_axis <- function(.x,
                             sides = "bottom",
                         at = NULL,
                         draw_lines = FALSE,
                         line_width = 0,
                         tick_width = 0.7,
                         tick_gap = 0.5,
                         tick_length = -0.5,
                         tick_color = "gray30",
                         mean_color = "red",
                         axis_gap = 0.05,
                         median_gap = 0.05,
                         digits = 2,
                         label_size = 0.9,
                         ...){

  sides <- match.arg(sides,
                   c("bottom","left",
                     "top","right"),
                   several.ok = TRUE)

  stopifnot(
          is.character(sides),
          is.numeric(tick_gap),
          is.numeric(axis_gap),
          is.numeric(median_gap),
          is.logical(draw_lines),
          is.numeric(line_width),
          is.numeric(tick_width)
      )

  # Calculate summary stats for variable
  summ_stats <- as.numeric(summary(.x))
  x_min <- summ_stats[1]
  x_q1 <- summ_stats[2]
  x_median <- summ_stats[3]
  x_mean <- summ_stats[4]
  x_q3 <- summ_stats[5]
  x_max <- summ_stats[6]

  # Recon sides to be plotted
  side_map <- c(bottom = 1, left = 2,
                top = 3, right = 4)
  numeric_sides <- side_map[sides]

  char_width <- par("cin")[1]

  # Handle side pars and tick marks
  for (side in numeric_sides) {

    # Define properties for each axis side
    # in the following order (b,l,t,r)
    flip_choice <- c(1, 1, -1, -1) # Use to place labels in/outside plot margin
    x_axis_choice <- c(TRUE, FALSE, TRUE, FALSE) # Use to work in x or y axes
    is_log_axis <- c("xlog", "ylog", "xlog", "ylog") # Work with log x-axis
    other_log_axis <- c("ylog", "xlog", "ylog", "xlog") # work with log x-axis
    usr_indices <- c(3, 1, 4, 2) # Extreme user coord. of plotting region

    # Extract the relevant values based on the 'side'
    flip <- flip_choice[side]
    x_axis <- x_axis_choice[side]
    is_log <- par(is_log_axis[side])
    other_log <- par(other_log_axis[side])
    par_side <- usr_indices[side]

    # Calculate the axis base position and dimensions
    base <- par("usr")[par_side]
    plot_width <- diff(par("usr")[1:2])
    plot_height <- diff(par("usr")[3:4])

    if (is_log) {
      # For log scale, work in log space
      shift <- log10(1 + axis_gap)
      med_gap <- log10(1 + median_gap)
      mean_shift <- log10(1 + char_width * 0.5 * flip)

      # Transform statistics to log space
      log_min <- log10(x_min)
      log_q1 <- log10(x_q1)
      log_median <- log10(x_median)
      log_mean <- log10(x_mean)
      log_q3 <- log10(x_q3)
      log_max <- log10(x_max)

      # Calculate gaps in log space
      gapt <- 10^(log_median + med_gap)
      gapb <- 10^(log_median - med_gap)
      q3_shifted <- 10^(log_q3 + shift)
      q1_shifted <- 10^(log_q1 - shift)
    } else {

      # Linear scale calculations
      shift <- par("pin")[1] * 0.02 * axis_gap
      med_gap <- par("pin")[1] * median_gap
      mean_shift <- char_width * 0.5 * flip

      gapt <- x_median + med_gap
      gapb <- x_median - med_gap
      q3_shifted <- x_q3 + shift
      q1_shifted <- x_q1 - shift
    }

    # Position of shifted quartiles
    # Position of shifted quartiles
    off_set <- if (is_log) {
      log10(base) + shift
    } else {
      base + shift
    }

    # Adjust the base position of the mean pointer based on quartile range
    if ((x_mean > x_q3) || (x_mean < x_q1)) {
      # Mean is outside Q1/Q3, so move relative to base
      mean_base <- if (is_log) {
        log10(base) - mean_shift
      } else {
        base - mean_shift
      }
    } else {
      # Mean is inside Q1/Q3, so move relative to shifted base
      mean_base <- if (is_log) {
        off_set - mean_shift
      } else {
        off_set - mean_shift
      }
    }

    if (other_log) {
      mean_base <- 10^mean_base
      off_set <- 10^off_set
      base <- 10^base
    }

    # Generate tick positions if `at` is not specified
    ticks <- if (is.null(at)) {
      # set the tick marks to match summ. stats
      ticks <- round(c(x_min, x_q1,x_median,x_mean,
                      x_q3, x_max), digits)
    } else {
      at
    }

    # Ensure ticks are within range and properly spaced
    ticks <- ticks[(ticks>=x_min) & (ticks<=x_max)]

    # Calculate minimum gap between ticks
    ticks <- adjust_axis_ticks(ticks,x_min,
                      x_max,tick_gap,
                      is_log, digits)

    # Draw the tick marks only, without drawing the axis line
    axis(side, ticks,
         labels = FALSE,
         col = "gray50",
         lwd = line_width,
         tcl = tick_length,
         col.tick = tick_color,
         lwd.tick = tick_width,
         ...)

    # Define `las` separately for x and y axes
    las_value <- if (side %in% c(1, 3)) 1 else 2

    # Add the labels, making the mean label red
    for (i in seq_along(ticks)) {

      label_color <- if(ticks[i] == round(x_mean, digits)){
        mean_color
      }  else {
          "black"}

      # Add the labels without drawing any
      # additional ticks or axis line
      mtext(
        text = ticks[i],
        side = side,
        at = ticks[i],
        line = 0.85,
        las = las_value,
        col = label_color,
        cex = label_size
      )
    }

    # Draw the Q1 and Q3 shifted segments only if draw_lines is TRUE
    if (draw_lines) {
      if (!x_axis) {
        lines(rep(base, 2), c(x_min, q1_shifted), xpd = TRUE)
        lines(rep(base, 2), c(q3_shifted, x_max), xpd = TRUE)
      } else {
        lines(c(x_min, q1_shifted), rep(base, 2), xpd = TRUE)
        lines(c(q3_shifted, x_max), rep(base, 2), xpd = TRUE)
      }
    }
  }
}
#' Draw Minimal Rug Axis with Selective Labeling
#'
#' This function adds a minimalist axis with rug ticks and selective labeling to a plot.
#' It allows for customized control over tick marks, tick lengths, label spacing, and
#' highlighting of key summary statistics or specific values, such as the mean.
#'
#' @param .x A numeric vector. The data used for the axis ticks and labels.
#' @param side A character vector indicating which sides of the plot to draw the axis on.
#'   Options are `"bottom"`, `"left"`, `"top"`, and `"right"`. Default is `"bottom"`.
#' @param at Optional. A numeric vector specifying the exact positions for ticks.
#'   If `NULL`, the function uses the values from `.x`.
#' @param line_width Numeric. The width of the axis line. Default is `0` (no line).
#' @param tick_width Numeric. The width of the tick marks. Default is `0.7`.
#' @param tick_length Numeric. The length of the tick marks as a fraction of the plot dimension.
#'   Default is `0.5`.
#' @param tick_color Character. The color of the tick marks. Default is `"black"`.
#' @param mean_color Character. The color used to highlight the mean value label. Default is `"red"`.
#' @param label_every_n Integer. Optional. If specified, labels will be drawn at every `n`-th tick.
#'   Default is `NULL`, meaning labels are determined dynamically.
#' @param label_summ_stats Logical. If `TRUE`, only summary statistics (`min`, `Q1`, `median`,
#'   `mean`, `Q3`, `max`) will be labeled. If `FALSE`, labels will be determined based on spacing.
#'   Default is `FALSE`.
#' @param label_gap Numeric. The minimum spacing between labels to prevent overlap.
#'   Expressed as a multiplier of the median spacing between values. Default is `1.5`.
#' @param digits Integer. The number of decimal places to round the labels. Default is `2`.
#' @param ... Additional graphical parameters passed to the `axis()` function.
#'
#' @details
#' The function draws rug ticks on specified sides of the plot using the values from `.x`
#' or the positions defined in `at`. It includes options for drawing only specific labels
#' (like summary statistics) or dynamically adjusting the labels to avoid overcrowding.
#'
#' If `label_summ_stats = TRUE`, the function will label only the minimum, first quartile,
#' median, mean, third quartile, and maximum values. Otherwise, it will label values
#' based on spacing criteria defined by `label_gap`.
#'
#' The function uses `mtext()` to add labels and `axis()` to draw ticks without a full axis line,
#' allowing for a clean, minimalist appearance similar to Edward Tufte's style.
#'
#' @return The function is used for its side effects (drawing on the existing plot). It does not
#' return a value.
#'
#' @examples
#' set_font("Alegreya")
#' plot.new()
#' plot.window(range(mtcars$wt),range(mtcars$mpg))
#' points(mtcars$wt,mtcars$mpg, pch = 21, col = "black")
#' minimal_rug_axis(mtcars$wt, sides = "bottom", tick_length = 0.75, label_summ_stats = TRUE)
#' minimal_rug_axis(mtcars$mpg, sides = "left", label_summ_stats = TRUE)
#' mtext(
#'   "Car weight (lb/1000) v. \nMiles per gallon",
#'   side = 3,
#'   at = 4.55,
#'   line = -4.5,
#'   adj = 0,
#'   cex = 1.2,
#'   col = "black",
#'   font = 3
#' )
#'
#' @seealso \code{\link{axis}}, \code{\link{mtext}}, \code{\link{rug}}
#' @export

minimal_rug_axis <- function(.x,
                             sides = "bottom",
                             at = NULL,
                             line_width = 0,
                             tick_width = 0.7,
                             tick_length = 0.5,
                             tick_color = "black",
                             mean_color = "red",
                             label_every_n = NULL,
                             label_summ_stats = FALSE,
                             label_gap = .85,
                             digits = 2,
                             ...){

  sides <- match.arg(sides,
                     c("bottom","left",
                       "top","right"),
                     several.ok = TRUE)

  stopifnot(
    is.character(sides),
    is.numeric(line_width),
    is.numeric(tick_width),
    is.numeric(tick_width),
    is.character(tick_color),
    is.character(mean_color),
    is.numeric(label_gap)
  )

  # Recon sides to be plotted
  side_map <- c(bottom = 1, left = 2,
                top = 3, right = 4)
  numeric_sides <- side_map[sides]


  #Compute summary stts
  summ_stats <- as.numeric(summary(.x))
  x_mean <- mean(.x)

  side_map <- c(bottom = 1, left = 2, top = 3, right = 4)
  numeric_sides <- side_map[sides]

  # Handle tick positions
  ticks <- if (is.null(at)) .x else at

  # Handle side parameters and tick marks
  for (side in numeric_sides) {
    # Draw the tick marks (rug plot)
    axis(side,
         at = ticks,
         labels = FALSE,
         lwd = line_width,
         tcl = tick_length,
         col.tick = tick_color,
         lwd.tick = tick_width,
         ...)

    # Define label orientation based on axis side
    las_value <- if (side %in% c(1, 3)) 1 else 2

    # Determine labels to draw based on `label_summ_stats` or dynamic spacing
    labels_to_draw <- if (label_summ_stats) {
      summ_stats
    } else {
      ticks
    }

    # Dynamically skip labels to prevent overlap
    if (!label_summ_stats && length(labels_to_draw) > 1) {
      label_positions <- labels_to_draw
      label_spacing <- abs(diff(label_positions))
      median_spacing <- median(label_spacing)
      labels_to_draw <- labels_to_draw[c(TRUE, diff(labels_to_draw) > label_gap * median_spacing)]
    }

    # Draw selective labels with appropriate gaps
    for (stat in labels_to_draw) {
      # Determine label color for mean vs. other statistics
      label_color <- if (round(stat, digits) == round(x_mean, digits)) mean_color else "black"

      # Draw labels using `mtext`
      mtext(
        text = round(stat, digits),
        side = side,
        at = stat,
        line = 0.85,
        las = las_value,
        col = label_color,
        cex = 0.9
      )
    }
  }
}
#' Add Marginal Histograms or Stripcharts to Plot Axes
#'
#' This function adds marginal histograms or stripcharts (depending on input) to the sides of an existing plot. The function allows for customization of the appearance of the histograms or stripcharts, including adjustments to axis labels, axis positioning, and summary statistics.
#'
#' @param .x A numeric vector. The data to be plotted in the marginal histogram or stripchart.
#' @param side A character string indicating which side of the plot the axis should be drawn on. Options are: "bottom", "left", "top", or "right".
#' @param scale_shift A numeric value to control the horizontal or vertical shift of the stripchart elements. Default is 0.15.
#' @param mean_shift A numeric value to control the shift of the mean label from its default position. Default is 0.5.
#' @param stack_color A character string specifying the color of the stripchart points. Default is "red".
#' @param mean_color A character string specifying the color of the mean line or label. Default is "red".
#' @param axis_gap A numeric value to control the gap between the plot axis and the stripchart or histogram. Default is 0.005.
#' @param point_character A numeric value specifying the plotting symbol to use for the stripchart points. Default is 15 (solid circle).
#' @param point_size A numeric value specifying the size of the points in the stripchart. Default is 0.15.
#' @param label_gap A numeric value to control the gap between the labels and the axis. Default is 0.45.
#' @param label_summ_stats A logical value. If `TRUE`, the function will display summary statistics (mean, median, etc.) as labels. Default is `FALSE`.
#' @param digits An integer specifying the number of digits to round the labels to. Default is 2.
#' @param na.rm A logical value. If `TRUE`, missing values are removed before processing the data. Default is `TRUE`.
#' @param ... Additional arguments passed to other functions (such as `stripchart`).
#'
#' @return This function does not return a value. It modifies the existing plot by adding a marginal histogram or stripchart.
#'
#' @details
#' The function adds a stripchart to the specified axis side, and it can display summary statistics (e.g., mean, median, quartiles) on the plot. The `side` argument determines which axis the marginal plot is drawn on, and the plot type can be customized using the `stack_color`, `point_character`, and `mean_color` arguments.
#'
#' If `label_summ_stats` is `TRUE`, summary statistics are drawn as labels along the axis. The `digits` argument controls the precision of these labels.
#'
#' The function makes adjustments to the plot's margins and axis positions to avoid overlap and ensure a clean presentation.
#'
#' @seealso \code{link{stripchart()}}, \code{\link{summary()}}, \code{\link{par()}}
#'
#' set_font("Alegreya")
#' x <- faithful$waiting
#' y <- faithful$eruptions
#' plot.new()
#' plot.window(c(min(x)/1.1,max(x)),
#'             c(min(y)/1.5, max(y))))
#' points(x,y, pch = 16, cex = 0.75)
#' marginal_hist_axis(x,side = "bottom",
#'               label_summ_stats = TRUE)
#' marginal_hist_axis(y, side = "left",
#'                label_summ_stats = TRUE)
#'
#' mtext("Time till next eruption (min)", side = 1, line = 2, col = "black")
#'
#' mtext("Duration (sec)", side = 2, line = 2, col = "black")
#' #' @export

marginal_hist_axis <- function(.x,
                               side,
                               scale_shift = 0.15,
                               mean_shift = 0.5,
                               stack_color = "red",
                               mean_color = "red",
                               axis_gap = 0.005,
                               point_character = 15,
                               point_size = 0.15,
                               label_gap = .45,
                               label_summ_stats = FALSE,
                               digits = 2,
                               na.rm = TRUE,
                               ...){

  side <- match.arg(side,
                    c("bottom", "left",
                      "top", "right"),
                    several.ok = FALSE)

  stopifnot(
    is.character(side),
    is.numeric(axis_gap),
    is.numeric(scale_shift),
    is.numeric(mean_shift),
    is.character(stack_color)
  )

  .x <- if(na.rm) .x[complete.cases(.x)] else .x

  # Compute summary stats
  summ_stats <- as.numeric(summary(.x))
  x_mean <- mean(.x)

  # Recon sides to be plotted
  side_map <- c(bottom = 1, left = 2,
                top = 3, right = 4)
  numeric_sides <- side_map[side]

  for(side in numeric_sides){

    # Define properties for each axis side
    flip_choice <- c(1, 1, -1, -1)
    x_axis_choice <- c(TRUE, FALSE, TRUE, FALSE)
    usr_indices <- c(3, 1, 4, 2)

    flip <- flip_choice[side]
    x_axis <- x_axis_choice[side]
    par_side <- usr_indices[side]

    base <- par("usr")[par_side]
    is_log <- if (x_axis) par("xlog") else par("ylog")
    if (is_log) base <- 10^base
    plot_width <- diff(par("usr")[1:2])
    plot_height <- diff(par("usr")[3:4])
    char_width <- par("cin")[1]
    char_height <- par("cin")[2]

    raw_axis_shift <- par("pin")[1]*axis_gap*flip
    raw_median_gap <- par("pin")[1]*axis_gap
    raw_mean_shift <- char_width * mean_shift * flip
    raw_strip_shift <- char_width * scale_shift*flip

    axis_shift <- shift_scale(raw_axis_shift, axis = if (!x_axis) "y" else "x", plot_width, plot_height)
    mean_shift <- shift_scale(raw_mean_shift, axis = if (!x_axis) "y" else "x", plot_width, plot_height)
    strip_shift <- shift_scale(raw_strip_shift, axis = if (!x_axis) "y" else "x", plot_width, plot_height)
    median_gap <- shift_scale(raw_median_gap, axis = if (!x_axis) "y" else "x", plot_width, plot_height, is_gap = TRUE)

    offset <- if (!x_axis) flip * char_height/char_width else flip

    old_xpd <- par("xpd")
    on.exit(par(xpd = old_xpd))

    # Adjust vertical based on the axis
    stripchart(.x,
               method = "stack",
               vertical = !x_axis, # FALSE for X-axis, TRUE for Y-axis
               offset = offset,
               pch = point_character,
               cex = point_size,
               add = TRUE,
               at = base + axis_shift + strip_shift,
               col = stack_color)

    las_value <- if (side %in% c(1, 3)) 1 else 2

    labels_to_draw <- if (label_summ_stats) summ_stats
    else  pretty(range(.x), n = 5)

    if (!label_summ_stats && length(labels_to_draw) > 1) {
      label_positions <- labels_to_draw
      label_spacing <- abs(diff(label_positions))
      median_spacing <- median(label_spacing)
      labels_to_draw <- labels_to_draw[c(TRUE, diff(labels_to_draw) > label_gap * median_spacing)]
    }

    for (stat in labels_to_draw) {
      label_color <- if (round(stat, digits) == round(x_mean, digits)) mean_color else "black"
      mtext(text = round(stat, digits),
            side = side,
            at = stat,
            line = 0.85,
            las = las_value,
            col = label_color,
            cex = 0.9)
    }
  }
}






