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

  side_map <- c(bottom = 1, left = 2, top = 3, right = 4)
  numeric_sides <- side_map[sides]

  for (side in numeric_sides) {

    # Generate tick positions if `at` is not specified
    tick_positions <- if (is.null(at)) {
      pretty(graphics::axTicks(side))
    } else {
      at
    }

    # Format tick labels
    formatted_labels <- format_axis(tick_positions, format)

    axis_args <- c(
      list(
        side = side,
        at = tick_positions,
        labels = formatted_labels,
        lwd = line_width),
      args
    )

    # Apply custom axis with formatted labels
    do.call(graphics::axis, axis_args)
  }
}

# format plot axis depending on user input
format_axis <- function(.x,format){

  .x <- switch(format,
               percent = sprintf("%1.0f%%", 100 * .x),
               date = as.Date(.x, format = "%D"),
               POSIXt = as.POSIXct(.x, format = "%D"),
               integer = as.integer(.x),
               scientific = sprintf("%e", .x)
  )

  return(.x)
}
