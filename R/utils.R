# Simple negation function to find excluded elements in a set
`%!in%` <- Negate(`%in%`)

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


# Adjust lengths to ensure both axes scale equally on the output device
shift_scale <- function(value, axis, plot_width, plot_height, is_gap = FALSE) {
  # Adjust shift based on whether it applies to the axis or to a gap
  if (axis == "y") {
    if (is_gap) {
      # For gaps on the y-axis, use the width
      value / par("pin")[2] * plot_height
    } else {
      # For shifts along the y-axis, use the width
      value / par("pin")[1] * plot_width
    }
  } else {
    if (is_gap) {
      # For gaps on the x-axis, use the height
      value / par("pin")[1] * plot_width
    } else {
      # For shifts along the x-axis, use the height
      value / par("pin")[2] * plot_height
    }
  }
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
all_different <- function(x){
  length(unique(x)) == length(x)
}
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
all_the_same <- function(x, tol = 1e-10){
  if(is.numeric(x)){
    return(max(x,na.rm = TRUE) - min(x,na.rm = TRUE) < tol  && !anyNA(x))
  }
  if (!is.list(x)) {
    return(length(unique(x)) == 1)
  }
}
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
are_equal = function(x, y,
                     check.names = TRUE,
                     check.attributes = TRUE,
                     ...) {

  test = all.equal(target = x,
                   current = y,
                   check.names = check.names,
                   check.attributes = check.attributes,
                   ...)

  if (is.logical(test)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
any_zero_negative <- function(x) any(x <= 0)

any_negative <- function(x) any(x < 0L)

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
rid_na <- function(x){
  x <- x[!is.na(x)]
  return(x)
}

keep_finite <- function(x){
  x <- x[is.finite(x)]
  return(x)
}
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
is_empty_object <- function(x) {
 (length(x) == 0)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
is.formula <- function(x) inherits(x,"formula")

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
formula_length <- function(x) {
  if (!is.formula(x)) {
    stop("Error: Object is not a formula.")
  }


  formula_length <- length(x)
  formula_parts <- as.list(x)

  # Check for NULL values in formula components based on the length of the formula
  if (formula_length >= 2 && is.null(formula_parts[[2]])) {
    stop("Error: Formula contains a NULL value on the left-hand side.")
  }
  if (formula_length >= 3 && is.null(formula_parts[[3]])) {
    stop("Error: Formula contains a NULL value on the right-hand side.")
  }

  # Return formula length
  return(formula_length)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
are_factors <- function(df,
                          verbose = FALSE) {

  stopifnot(is.data.frame(df))

  factor_columns <- sapply(df, is.factor)

  if (any(factor_columns)) {
    if (verbose) {
      cat("The following columns are factors:\n")
      print(names(df)[factor_columns])
    }
    return(TRUE)
  } else {
    if (verbose) cat("No columns are factors.\n")
    return(FALSE)
  }
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
handle_formula <- function(formula, data) {
  # Check if the input is a formula
  if (!is.formula(formula)) {
    stop("Error: Provide a valid formula object.")
  }

  # Validate the formula length
  n_terms <- formula_length(formula)
  if (n_terms < 2 || n_terms > 3) {
    stop("Error: Formula must be of the form '~ x' or 'y ~ x'.")
  }

  # Extract terms from the formula
  if (n_terms == 2) {
    # Handle formula '~ x'
    x_name <- as.character(formula[[2]]) # Extract variable name
    x <- data[[x_name]]                 # Retrieve the column from the data

    if (is.null(x)) stop(paste("Error: Variable", x_name, "not found in the dataset."))

    if (!is.numeric(x)) {
      stop(paste("Error: Variable", x_name, "is not continuous. Please provide a numeric variable."))
    }

    return(list(x = x, y = NULL))  # Return a list with x only
  } else {
    # Handle formula 'y ~ x'
    y_name <- as.character(formula[[2]]) # Left-hand side
    x_name <- as.character(formula[[3]]) # Right-hand side

    y <- data[[y_name]]
    x <- data[[x_name]]

    # Check for missing variables
    if (is.null(y)) stop(paste("Error: Variable", y_name, "not found in the dataset."))
    if (is.null(x)) stop(paste("Error: Variable", x_name, "not found in the dataset."))

    # Check data types
    if (!is.numeric(x)) {
      warning(paste("Variable", x_name, "is not continuous. Variable will be ignored."))
    }
    if (!is.numeric(y)) {
      warning(paste("Warning: Variable", y_name, "is not continuous and will be ignored."))
      y <- NULL  # Discard non-continuous 'y'
    }

    return(list(x = x, y = y))  # Return a list of valid variables
  }
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
