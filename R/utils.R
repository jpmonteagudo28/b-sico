# Simple negation function to find excluded elements in a set
`%!in%` <- Negate(`%in%`)

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
same_length <- function(x, y) {
  length(x) == length(y)
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
all_finite <- function(x) {
  if (is.data.frame(x) || is.matrix(x)) {
    # Flatten to check all elements
    x <- as.vector(x)
  }

  # Identify non-finite values
  non_finite_indices <- which(!is.finite(x))

  # Check for finiteness
  if (length(non_finite_indices) > 0) {
    non_finite_values <- x[non_finite_indices]
    stop(paste0(
      "Input contains non-finite values at indices: ",
      paste(non_finite_indices, collapse = ", "),
      ". Non-finite values include: ",
      paste(unique(non_finite_values), collapse = ", "),
      ". Please ensure all values are finite (no NA, NaN, or Inf)."
    ))
  }
  return(TRUE)
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
# Custom function to check for factors and convert them to numeric
factors_as_numeric <- function(df, verbose = FALSE) {

  stopifnot(is.data.frame(df))

  # Use are_factors() to check for factors
  if (are_factors(df, verbose = verbose)) {
    factor_columns <- names(df)[sapply(df, is.factor)]

    for (col in factor_columns) {
      # Attempt to convert to numeric
      original <- df[[col]]
      df[[col]] <- as.numeric(as.character(df[[col]]))

      # Check for NA coercion
      if (any(is.na(df[[col]]) & !is.na(original))) {
        stop(paste0("Conversion of factor column '", col, "' to numeric resulted in NA values. ",
                    "Please check the column's values for invalid entries."))
      }
    }
  }

  return(df)
}
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Design to work with character matrices instead of data frames
chars_as_numeric <- function(df, verbose = FALSE) {

  stopifnot(is.data.frame(df))

  char_columns <- sapply(df, is.character)

  if (any(char_columns)) {
    if (verbose) {
      cat("The following columns are characters:\n")
      print(names(df)[char_columns])
    }

    for (col in names(df)[char_columns]) {
      # Attempt to convert to numeric
      original <- df[[col]]
      df[[col]] <- as.numeric(df[[col]])

      # Check for NA coercion
      if (any(is.na(df[[col]]) & !is.na(original))) {
        stop(paste0("Conversion of character column '", col, "' to numeric resulted in NA values. ",
                    "Please check the column's values for invalid entries."))
      }
    }
  } else {
    if (verbose) cat("No columns are characters.\n")
  }

  return(df)
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
replace_char <- function(.input_string,
                         old_char,
                         new_char,
                         use_regex = FALSE) {

  if (old_char == "" || is.null(old_char)) {
    stop("Error: The character to replace cannot be empty.")
  }

  gsub(old_char,
       new_char,
       .input_string,
       fixed = !use_regex)

}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Check whether data frame is in long or wide format
# Since df possess columns of equal length, we only need to check the number of unique IDs and time points
# If the number of rows equals the product of unique IDs and time points, it's long
is_long <- function(id, values, df) {
  stopifnot(is.data.frame(df))

  n_unique1 <- length(unique(df[[id]]))
  n_unique2 <- length(unique(df[[values]]))

  # Check if columns are numeric
  is_numeric1 <- is.numeric(df[[id]])
  is_numeric2 <- is.numeric(df[[values]])

  # Heuristic to determine which is likely the time column:
  # 1. If one is numeric and other isn't, numeric is likely time
  # 2. If both/neither numeric, one with fewer unique values is likely time
  # (time columns typically have fewer unique values than ID columns)

  if (is_numeric1 != is_numeric2) {
    # If exactly one is numeric, use that as time
    time_col <- if(is_numeric1) id else values
    id_col <- if(is_numeric1) values else id
  } else {
    # If both or neither are numeric, use number of unique values
    time_col <- if(n_unique1 < n_unique2) id else values
    id_col <- if(n_unique1 < n_unique2) values else id
  }

  # Store the detected column roles
  attr(df, "detected_time_col") <- time_col
  attr(df, "detected_id_col") <- id_col

  # Calculate result
  n_ids <- length(unique(df[[id_col]]))
  n_time <- length(unique(df[[time_col]]))
  n_rows <- nrow(df)

  result <- n_ids * n_time == n_rows

  # Add informative message about the detection
  message(sprintf("Detected '%s' as ID column and '%s' as time column",
                  id_col, time_col))

  return(result)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Reorder columns based on position
reorder_cols <- function(.data,
                         cols,
                         new_pos = NULL){

  stopifnot(is.data.frame(.data),
            is.character(cols),
            is.numeric(new_pos),
            length(cols) == length(new_pos)
  )

  data_names <- names(.data)
  data_len <- length(data_names)
  col_pos <- new_pos

  stopifnot(
    all(cols %in% data_names), # All specified columns must exist
    all(new_pos > 0),          # Positions must be greater than 0
    all(new_pos <= data_len),  # Positions must be within bounds
    !any(duplicated(cols)),    # No duplicate columns
    !any(duplicated(new_pos))  # No duplicate positions
  )

  # Initialize the new, ordered df
  output_df <- character(data_len)

  output_df[col_pos] <- cols

  output_df[-col_pos] <- data_names[!(data_names %in% cols)]

  # Make sure no columns are lost and no new columns are added
  stopifnot(length(output_df) == data_len)

  .data <- .data[,output_df]

  return(.data)
}
