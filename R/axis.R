# Create custom axis in base graphics plot
basic_axis <- function(format = "integer",
                     sides = c("bottom","left"),
                     line_width = 0.5,...,
                     at = NULL){

  args <- list(...)

  format <- match.arg(format,
                      c("percent","date","POSIXt","integer","scientific"),
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
summary_axis <- function(.x,format = "integer",
                         sides = c("bottom","left"),
                         at = NULL, gap = 0.5,
                         digits = NULL,
                         line_width = 0.5,...){

  args <- list(...)

  format <- match.arg(format,
                    c("percent","date","POSIXt",
                      "integer","scientific"),
                    several.ok = FALSE)

  sides <- match.arg(sides,
                   c("bottom","left",
                     "top","right"),
                   several.ok = TRUE)

  stopifnot(is.numeric(line_width),
          is.character(format),
          is.character(sides)
      )

  # Calculate summary stats for variable
  summ_stats <- summary(.x)
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
    xaxis <- x_axis_choice[side]
    is_log <- par(is_log_axis[side])
    other_log <- par(other_log_axis[side])
    par_side <- usr_indices[side]

    # Generate tick positions if `at` is not specified
    ticks <- if (is.null(at)) {
      pretty(graphics::axTicks(side))
    } else {
      at
    }

    ticks <- ticks[(ticks>=x_min) & (ticks<=x_max)]
    ticks_len <- length(ticks)

  # Calculate minimum gap between ticks
    adjust_axis_ticks(ticks,x_min,
                      x_max,gap,
                      is_log)

  # Format tick labels
  labels <- c(min_lab = ticks[1],
               max_lab = ticks[ticks_len],
               mid_lab = ticks[2:ticks_len-1])|>
  format_axis(ticks, format,...)

  bg <- par("bg")
  if (bg == "transparent"){
    bg <- "white"
    }



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







