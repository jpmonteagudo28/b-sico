
#' Reshape data from wide to long format
#'
#' @param data Data frame to reshape
#' @param id_cols Character vector of column names that identify each unique observation
#' @param names_to Name of the new "key" column to create
#' @param values_to Name of the new "value" column to create
#' @param cols_to_gather Columns to reshape into long format (if NULL, uses all non-id columns)
#' @param sep Pattern to split column names on when names contain multiple variables
#' @return A data frame in long format
#' @examples
#' # Simple example
#' df <- data.frame(id = 1:3, a = 1:3, b = 4:6)
#' gather(data = df, id_cols = "id", names_to = "key", values_to = "value")
gather <- function(data,
                        id_cols,
                        names_to = "variable",
                        values_to = "value",
                        cols_to_gather = NULL,
                        sep = NULL,
                        ...) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (missing(id_cols)) {
    stop("'id_cols' must be provided")
  }

  # Check if id columns exist in data
  missing_cols <- setdiff(id_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Following id columns not found in data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Determine columns to gather
  if (is.null(cols_to_gather)) {
    cols_to_gather <- setdiff(names(data), id_cols)
  } else {
    missing_gather <- setdiff(cols_to_gather, names(data))
    if (length(missing_gather) > 0) {
      stop("Following columns to gather not found in data: ",
           paste(missing_gather, collapse = ", "))
    }
  }

  if (length(cols_to_gather) == 0) {
    stop("No columns to gather")
  }

  # Store original column names before reshaping
  original_names <- cols_to_gather

  # Create a mapping between positions and original names
  name_mapping <- setNames(original_names, seq_along(original_names))

  # Reshape to long format
  result <- stats::reshape(data,
                    direction = "long",
                    varying = cols_to_gather,
                    idvar = id_cols,
                    timevar = names_to,
                    v.names = values_to,
                    ...)

  # Map the time values back to original column names
  result[[names_to]] <- name_mapping[as.character(result[[names_to]])]

  # If sep is provided, split the names column
  if (!is.null(sep)) {
    name_parts <- strsplit(as.character(result[[names_to]]), sep)
    max_parts <- max(sapply(name_parts, length))

    # Create new columns for each part
    for (i in 1:max_parts) {
      col_name <- paste0(names_to, i)
      result[[col_name]] <- sapply(name_parts, function(x) {
        if (length(x) >= i) x[i] else NA
      })
    }

    # Remove original names column if requested
    if (length(name_parts[[1]]) > 1) {
      result[[names_to]] <- NULL
    }
  }

  # Remove row names and sort by id columns
  rownames(result) <- NULL
  result <- result[do.call(order, result[id_cols]), ]

  return(result)
}

# Helper function to identify numeric columns in a data frame
# Data frame to check
# Character vector of numeric column names
get_numeric_cols <- function(data) {
  names(data)[sapply(data, is.numeric)]
}
