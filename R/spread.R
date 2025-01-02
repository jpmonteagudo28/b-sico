# Wide data contains a time-constant variable occupying a
# single column and a time-varying variable occupying multiple columns,
# one for each time point. This function reshapes the data into a wide format,
# preparing it for finding slopes across time points.
# If the values column is not discrete, the resulting data frame will contain a large
# number of missing values.
#' Reshape data from long to wide format
#'
#' @param id_cols Character vector of column names that identify each unique observation
#' @param names_from Column(s) whose values will become new column names
#' @param values_from Column(s) containing values to fill the new columns
#' @param data Data frame to reshape
#' @param sep Character string to separate column names when multiple names_from columns
#' @param fill Value to use for missing combinations (default NA)
#' @return A data frame in wide format
#' @examples
#' # Simple example
#' df <- data.frame(id = 1:3, key = c("a","b","a"), value = 1:3)
#' spread_wide(id_cols = "id", names_from = "key", values_from = "value", data = df)
spread_wide <- function(id_cols,
                        names_from,
                        values_from,
                        data,
                        sep = "_",
                        fill = NA,
                        ...) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (missing(id_cols) || missing(names_from) || missing(values_from)) {
    stop("'id_cols', 'names_from', and 'values_from' must all be provided")
  }

  # Check if columns exist in data
  all_cols <- c(id_cols, names_from, values_from)
  missing_cols <- setdiff(all_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Following columns not found in data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Handle multiple names_from columns
  if (length(names_from) > 1) {
    data$.names_temp <- do.call(paste, c(Map(function(x) data[[x]], names_from),
                                         list(sep = sep)))
    names_from <- ".names_temp"
  }

  # Handle multiple values_from columns
  if (length(values_from) > 1) {
    result <- list()
    for (value_col in values_from) {
      temp <- reshape(data,
                      direction = "wide",
                      idvar = id_cols,
                      timevar = names_from,
                      v.names = value_col,
                      sep = sep,
                      ...)
      result[[value_col]] <- temp
    }
    # Merge results
    final <- Reduce(function(x, y) merge(x, y, by = id_cols, all = TRUE), result)
    return(final)
  } else {
    # Single value column
    result <- reshape(data,
                      direction = "wide",
                      idvar = id_cols,
                      timevar = names_from,
                      v.names = values_from,
                      sep = sep,
                      ...)
    return(result)
  }
}
