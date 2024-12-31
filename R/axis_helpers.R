# Calculate gaps based on scale type
ticks_gap <- function(ticks, gap, is_log,
                      digits = NULL) {

  if (is_log) {

    # For log scale, work with log-transformed differences
    diffs <- diff(log10(ticks))
    min_diff <- min(diffs)
    adj_gap <- min(gap, min_diff)

  } else {

    # For linear scale, work with raw differences
    diffs <- diff(ticks)
    min_diff <- min(diffs)
    adj_gap <- min(gap, min_diff)
  }
  return(adj_gap)
}

# Function to adjust ticks to include min and max values
adjust_ticks <- function(ticks, x_min, x_max,
                         adj_gap, is_log) {

  if (length(ticks) == 0) {
    stop("The 'ticks' vector is empty.")
  }

  if (is_log) {
    if (x_min <= 0 || x_max <= 0) {
      stop("Values must be positive for logarithmic scale")
    }
  }

  ticks_len <- length(ticks)

  # Adjust maximum tick
  last_tick <- ticks[ticks_len]

  if (is_log) {
    log_diff_max <- log10(x_max) - log10(last_tick)

    if (log_diff_max < adj_gap) {
      ticks[ticks_len] <- x_max

    } else if (log_diff_max >= adj_gap) {
      ticks <- c(ticks, x_max)
    }
  } else {
    if (x_max - last_tick < adj_gap) {
      ticks[ticks_len] <- x_max

    } else {
      ticks <- c(ticks, x_max)
    }
  }

  # Adjust minimum tick
  first_tick <- ticks[1]

  if (is_log) {
    log_diff_min <- log10(first_tick) - log10(x_min)

    if (log_diff_min < adj_gap) {
      ticks[1] <- x_min

    } else if (log_diff_min >= adj_gap) {
      ticks <- c(x_min, ticks)
    }
  } else {

    if (first_tick - x_min < adj_gap) {
      ticks[1] <- x_min

    } else {
      ticks <- c(x_min, ticks)
    }
  }

  return(unique(ticks))
}

# Main logic for adjusting axis ticks
adjust_axis_ticks <- function(ticks, x_min, x_max, gap,
                              is_log,digits = NULL) {

  # Validate inputs
  if (!is.numeric(ticks) || !is.numeric(x_min) || !is.numeric(x_max) || !is.numeric(gap)) {
    stop("All numerical inputs must be numeric")
  }

  if (is_log && (x_min <= 0 || x_max <= 0)) {
    stop("Values must be positive for logarithmic scale")
  }

  # Calculate the gap
  adj_gap <- ticks_gap(ticks, gap, is_log)

  # Adjust ticks to include min and max if needed
  ticks <- adjust_ticks(ticks, x_min, x_max, adj_gap, is_log)

  if (!is.null(digits)) {
    ticks <- round(ticks, digits)
  }

  return(ticks)
}
