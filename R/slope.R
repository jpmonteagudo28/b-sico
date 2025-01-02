# Generic method
calculate_slope <- function(data, ...) {
  UseMethod("calculate_slope")
}

#' Calculate slopes for data in wide format
#' @param data A data frame in wide format
#' @param id Column name containing group identifiers
#' @param time_cols Names of columns containing time point values
#' @param na_rm Logical indicating whether to remove NA values
#' @return A data frame with slopes for each group
calculate_slope.wide <- function(data, id, time_cols, na_rm = FALSE) {
  # Input validation
  stopifnot(
    is.data.frame(data),
    is.character(id),
    is.character(time_cols),
    length(time_cols) == 2,
    all(c(id, time_cols) %in% names(data))
  )

  # Extract time values from column names
  times <- as.numeric(time_cols)
  if (any(is.na(times))) {
    times <- c(1, 2)  # Default to unit difference if times can't be extracted
  }

  # Calculate slopes
  result <- data.frame(
    id = data[[id]],
    slope = apply(data[time_cols], 1, function(row) {
      tryCatch({
        slope(times, as.numeric(row), na_rm = na_rm)
      }, error = function(e) NA)
    })
  )

  names(result)[1] <- id
  return(result)
}

#' Calculate slopes for data in long format
#' @param data A data frame in long format
#' @param id Column name containing group identifiers
#' @param time Column name containing time values
#' @param value Column name containing values to calculate slopes for
#' @param na_rm Logical indicating whether to remove NA values
#' @return A data frame with slopes for each group
calculate_slope.long <- function(data, id, time, value, na_rm = FALSE) {
  # Input validation
  stopifnot(
    is.data.frame(data),
    is.character(id),
    is.character(time),
    is.character(value),
    all(c(id, time, value) %in% names(data))
  )

  # Check if data is actually in long format
  if (!is_long(time, id, data)) {
    stop("Data appears to be in wide format. Use calculate_slope.wide instead.")
  }

  # Split data by group and calculate slopes
  slopes <- by(data, data[[id]], function(group_data) {
    if (nrow(group_data) < 2) return(NA)

    # Order by time
    ordered_data <- group_data[order(group_data[[time]]), ]

    # Calculate slope
    tryCatch({
      slope(ordered_data[[time]],
            ordered_data[[value]],
            na_rm = na_rm)
    }, error = function(e) NA)
  })

  # Create result data frame
  result <- data.frame(
    id = names(slopes),
    slope = unlist(slopes)
  )

  names(result)[1] <- id
  return(result)
}

#' Main function to calculate slopes that automatically determines format
#' @param data A data frame
#' @param id Column name containing group identifiers
#' @param ... Additional arguments passed to specific methods
#' @return A data frame with slopes for each group
calculate_slope.default <- function(data, id, ...) {
  # Check if data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  # Get all numeric columns except id
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, id)

  if (length(numeric_cols) < 2) {
    stop("Need at least two numeric columns to calculate slopes")
  }

  # Try to determine if data is long or wide
  dots <- list(...)
  if (!is.null(dots$time) && !is.null(dots$value)) {
    # If time and value columns specified, treat as long
    calculate_slope.long(data, id = id, ...)
  } else if (!is.null(dots$time_cols)) {
    # If time_cols specified, treat as wide
    calculate_slope.wide(data, id = id, ...)
  } else {
    # Auto-detect based on structure
    if (length(numeric_cols) == 2) {
      # If exactly two numeric columns besides id, assume wide
      calculate_slope.wide(data, id = id, time_cols = numeric_cols, ...)
    } else {
      stop("Cannot automatically determine data format. Please specify time and value columns for long format or time_cols for wide format")
    }
  }
}
