#' Determine if a Data Frame is in Long Format
#'
#' This function evaluates whether a given data frame is in a long format or not, based on its structure.
#' It uses the relationship between the number of rows and the product of unique IDs and time points to make this determination.
#'
#' @param df A data frame to evaluate. The data frame must have at least two columns of equal length.
#' @param tolerance A numeric value between 0 and 1 specifying the allowed deviation between the actual number of rows
#'   and the expected number of rows. Default is 0.1.
#' @param min_rows An integer specifying the minimum number of rows required in the data frame. Default is 10.
#' @param sampling_threshold An integer specifying the maximum number of rows to sample for large datasets.
#'   If the data frame exceeds this threshold, a random subset is used for analysis. Default is 10,000.
#'
#' @details
#' The function pre-processes the input data frame by handling missing values, removing constant columns, and optionally
#' sampling rows if the data frame is large. It then calculates the cardinality (unique value counts) for each column and
#' evaluates combinations of columns to identify potential ID and time columns. If the number of rows closely matches the
#' expected number (based on unique combinations of ID and time), the data is classified as long format.
#'
#' The function invisibly returns additional information as attributes of the primary `TRUE` or `FALSE` output, including:
#' - `ratio`: The best match ratio between the expected and actual number of rows.
#' - `n_rows`: The number of rows in the data frame.
#' - `n_cols`: The number of columns in the data frame.
#'
#' @return A logical value: `TRUE` if the data frame is determined to be in long format, `FALSE` otherwise.
#'   Additional attributes (`ratio`, `n_rows`, `n_cols`) are attached to the result and can be accessed using `attributes()`.
#'
#' @examples
#' # Example data frame
#' df <- data.frame(
#'   ID = rep(1:3, each = 4),
#'   Time = rep(1:4, times = 3),
#'   Value = rnorm(12)
#' )
#'
#' # Check if the data frame is in long format
#' is_long(df)
#'
#' # Access additional attributes
#' result <- is_long(df)
#' attributes(result)
#'
#' @seealso
#' \code{\link{attributes}} for extracting attributes.
#'
#' @export

is_long <- function(df,
                    tolerance = 0.1,
                    min_rows = 10,
                    sampling_threshold = 10000) {

  if (!is.data.frame(df))
    stop("Input must be a data frame")

  if (nrow(df) < min_rows)
    stop("Data frame must have at least ", min_rows, " rows")

  if (ncol(df) < 2)
    stop("Data frame must have at least 2 columns")

  if (tolerance < 0 || tolerance > 1)
    stop("Tolerance must be between 0 and 1")

  df <- preprocess_data(df, sampling_threshold)
  results <- analyze_structure(df, tolerance)

  invisible(
    structure(is_long,
              ratio = results$ratio,
              n_rows = results$n_rows,
              n_cols = results$n_cols
    )
  )

  is_long <- results$is_long
}

# Data preprocessing
preprocess_data <- function(df, sampling_threshold) {
  df <- handle_missing_values(df)
  df <- sample_large_dataset(df, sampling_threshold)
  df <- remove_constant_columns(df)
  return(df)
}

# Missing value handling
handle_missing_values <- function(df) {
  na_proportions <- colMeans(is.na(df))
  valid_cols <- names(na_proportions[na_proportions < 0.1])
  if (length(valid_cols) < 2) stop("Too many missing values")
  return(df[valid_cols])
}

# Sampling for large datasets
sample_large_dataset <- function(df, threshold) {
  if (nrow(df) > threshold) {
    return(df[sample(nrow(df), threshold), ])
  }
  return(df)
}

# Remove constant columns
remove_constant_columns <- function(df) {
  varying_cols <- names(which(sapply(df, function(x) length(unique(x)) > 1)))
  if (length(varying_cols) < 2) stop("Need at least 2 varying columns")
  return(df[varying_cols])
}

# Structure analysis
analyze_structure <- function(df, tolerance) {
  cardinalities <- get_cardinalities(df)
  return(check_combinations(df, cardinalities, tolerance))
}

# Calculate column cardinalities
get_cardinalities <- function(df) {
  sapply(df, function(x) length(unique(x)))
}

# Check column combinations
check_combinations <- function(df, cardinalities, tolerance) {
  n_cols <- length(cardinalities)
  best_ratio <- Inf
  is_long <- FALSE

  for (i in 1:(n_cols-1)) {
    for (j in (i+1):n_cols) {
      expected_rows <- cardinalities[i] * cardinalities[j]
      ratio <- abs(nrow(df) - expected_rows) / nrow(df)

      if (ratio <= tolerance) {
        is_long <- TRUE
        best_ratio <- min(best_ratio, ratio)
      }
    }
  }

  return(list(
    is_long = is_long,
    ratio = best_ratio,
    n_rows = nrow(df),
    n_cols = ncol(df)
  ))
}

# Structure output
structure_output <- function(results) {
  structure(
    results$is_long,
    ratio = results$ratio,
    n_rows = results$n_rows,
    n_cols = results$n_cols
  )
}
