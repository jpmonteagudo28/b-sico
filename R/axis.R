# Create custom axis in base graphics plot
basic_axis <- function(format = "integer",
                     sides = c("bottom","left"),
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

# Format plot axis depending on user input
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

# Define a custom axis for range frame plots
#' @param name description
range_frame_axis <- function(.x,
                             sides = c("bottom","left"),
                         at = NULL,
                         draw_lines = FALSE,
                         line_width = 0.5,
                         tick_width = 0.7,
                         tick_gap = 0.5,
                         tick_length = -0.5,
                         tick_color = "gray30",
                         mean_color = "red",
                         axis_gap = 0.05,
                         median_gap = 0.05,
                         digits = 2,...){

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
      mean_shift <- log10(1 + par("cin")[1] * 0.5 * flip)

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
      mean_shift <- par("cin")[1] * 0.5 * flip

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
         lwd = 0,
         tcl = tick_length,
         col.tick = tick_color,
         lwd.tick = tick_width)

    # Define `las` separately for x and y axes
    las_value <- if (side %in% c(1, 3)){
      1
    }else{
      2
    }

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
        cex = 0.9
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







