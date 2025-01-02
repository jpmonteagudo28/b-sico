# Simple negation function to find excluded elements in a set
`%!in%` <- Negate(`%in%`)

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
is_date <- function(x) inherits(x,c("Date","POSIXct","POSIXlt","POSIXt"))

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
  # Basic input validation
  stopifnot(
    is.data.frame(df),
    !is.null(id),
    !is.null(values),
    id %in% names(df),
    values %in% names(df)
  )

  if (nrow(df) == 0) {
    stop("Empty dataframe")
  }

  # Get column classes and unique counts
  id_col <- df[[id]]
  values_col <- df[[values]]

  n_unique_id <- length(unique(id_col))
  n_unique_values <- length(unique(values_col))

  # Check if columns are various types of time-like data
  is_time_like <- function(x) {
    # Use is_date function for time checks
    is_date(x) || (is.numeric(x) && !any(x %% 1 != 0)) || (is.factor(x) && all(grepl("^\\d+$", levels(x))))
  }

  is_id_like <- function(x) {
    is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(x)) == length(x))
  }

  # Determine roles based on data characteristics
  id_time_score <- sum(c(
    is_time_like(id_col) * 2,  # Strong indicator for time
    is_id_like(values_col) * 1, # Weak indicator for not time
    (n_unique_id < n_unique_values) * 1  # Weak indicator for time
  ))

  values_time_score <- sum(c(
    is_time_like(values_col) * 2,
    is_id_like(id_col) * 1,
    (n_unique_values < n_unique_id) * 1
  ))

  # Determine format
  if (id_time_score > values_time_score) {
    time_col <- id
    id_col_name <- values
  } else {
    time_col <- values
    id_col_name <- id
  }

  # Calculate expected format characteristics
  n_ids <- length(unique(df[[id_col_name]]))
  n_times <- length(unique(df[[time_col]]))
  n_rows <- nrow(df)

  # Check if data follows long format pattern
  is_long_format <- n_ids * n_times >= n_rows * 0.9 && # Allow for some missing combinations
    n_ids * n_times <= n_rows * 1.1

  # Store detection results as attributes
  attr(df, "detected_time_col") <- time_col
  attr(df, "detected_id_col") <- id_col_name

  # Add informative message
  message(sprintf(
    "Detected '%s' as %s column and '%s' as %s column",
    id_col_name,
    if(is_long_format) "ID" else "unknown role",
    time_col,
    if(is_long_format) "time" else "unknown role"
  ))

  return(is_long_format)
}
#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Extend range of character vector if used on any axis
extend_character_range <- function(x,...){

  if(is.character(x)){
    len <- length(unique(x))
    x <- seq(1,len)
  }
  extendrange(x,...)
}

labels <- extendrange(1:ncol(data)) # for wide data frames
num_range <- extendrange(data[,-labels]) # for wide data frames
num_range <- extendrange(y,f = 0.05) # for long data frames

#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# dplyr synonyms using base R
# taken from https://github.com/coolbutuseless/poorman/blob/master/R/utils.R
extract <- `[`
extract2 <- `[[`
inset <- `[<-`
set_colnames <- `colnames<-`

deparse_dots <- function(...) {
  vapply(substitute(...()), deparse, NA_character_)
}

check_is_dataframe <- function(.data) {
  parent_fn <- all.names(sys.call(-1L), max.names = 1L)
  if (!is.data.frame(.data)) stop(parent_fn, " must be given a data.frame")
  invisible()
}

#---- --- ---- --- ---- --- ---- --- ---- --- ----#
