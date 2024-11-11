# Simple negation function to find excluded elements in a set
`%!in%` <- Negate(`%in%`)

# Calculate gaps based on scale type
ticks_gap <- function(ticks, gap, is_log,
                      digits = NULL) {

  ticks_len <- length(ticks)
  if (is_log) {

    # For logarithmic scale, calculate multiplicative gap
    if(!is.null(digits)){
      adj_gap <- round((log10(ticks[ticks_len]) - log10(ticks[ticks_len - 1])) * gap,
                       digits = digits)
    } else{
      adj_gap <- (log10(ticks[ticks_len]) - log10(ticks[ticks_len - 1])) * gap
    }


    return(adj_gap)
  } else {

    # For linear scale, calculate additive gap
    if(!is.null(digits)){
      adj_gap <- round((ticks[ticks_len] - ticks[ticks_len - 1]) * gap,
                       digits = digits)
    } else{
      adj_gap <- (ticks[ticks_len] - ticks[ticks_len - 1]) * gap
    }

    return(adj_gap)
  }
}

# Function to adjust ticks to include min and max values
adjust_ticks <- function(ticks, x_min, x_max,
                         adj_gap, is_log) {

  ticks_len <- length(ticks)

  # Adjust maximum tick
  last_tick <- ticks[ticks_len]

  if (islog) {

    if (log10(x_max) - log10(last_tick) < adj_gap) {
      ticks[ticks_len] <- x_max

    } else {
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

  if (islog) {

    if (abs(log10(first_tick) - log10(x_min)) < adj_gap) {
      ticks[1] <- x_min

    } else {
      ticks <- c(x_min, ticks)
    }

  } else {

    if (first_tick - x_min < adj_gap) {
      ticks[1] <- x_min

    } else {
      ticks <- c(x_min, ticks)
    }
  }

  return(ticks)
}

# Main logic for adjusting axis ticks
adjust_axis_ticks <- function(ticks, x_min, x_max, gap,
                              is_log,digits = NULL) {
  # Calculate the gap
  adj_gap <- ticks_gap(ticks, mingap, islog)

  # Adjust ticks to include min and max if needed
  ticks <- adjust_ticks(ticks, x_min, x_max, adj_gap, is_log)

  return(ticks)
}
