#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Calculate length(in pixels) of character string
# Can take units(user", "inches", "figure"),font(1,2,3,4), and `cex` arguments
# `units` argument set to "user" by default, however if `plot.new()` hasn't been called
# the function will return an error. To avoid this, set `units` to "inches" or "figure".
char_pixels <- function(.x, cex = 1, units = "user", ...) {
  # Calculate the width of a character string based on graphical parameters
  width <- strwidth(.x, cex = cex, units = units, ...)
  if (units == "user" && !par("plt")[1] > 0.09) {
    stop("`units` set to 'user', but no active plotting device found. Call `plot.new()` or use other units.")
  }
  return(width)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# NOTE: THIS FUNCTION SEEMS TO ONLY RETURN CORRECT LABEL POSITIONS AFTER `plot.new()` and `par()` calls.
# You'll have to call a new plot and set up the plot window before calling this function
# Use function to determine position of text labels when consecutive
# label check returns TRUE and `use_consecutive_labels` set to TRUE
# within_labels_gap in number of lines of margin
# To be supplied to `line` argument in `mtext`
# Ideally, the function will calculate each label position
# A more intuitive, but inflexible and less useful option
# is to find the maximum char. width in pixels and set every
# label at that distance + 0.5, but the whites pace may be too
# big for some labels
# Keep in mind that font choice (italic, bold, regular) will influence the width of the label
consecutive_label_position <- function(labels,
                                       values,
                                       within_labels_gap,
                                       cex,
                                       new_value_method = "first",
                                       ...){

  if (length(labels) != length(values)) {
    stop("`labels` and `values` must have the same length.")
  }

  overlaps <- should_be_consecutive(labels, values, cex, ...)

  # Initialize output data frame
  output <- data.frame(
    label = labels,
    value = values,
    line = NA,
    row = NA
  )

  # Group labels into rows based on overlaps
  current_row <- 1
  for (i in order(values)) {
    if (!is.na(output$line[i])) next # Skip already processed labels

    # Initialize the group for the current label
    group_indices <- i

    # Check for overlapping labels in both directions
    for (j in seq_along(values)) {
      if (j != i && overlaps[j] && abs(values[i] - values[j]) < 1) {
        group_indices <- c(group_indices, j)
      }
    }

    # Assign line and row information
    group_indices <- unique(group_indices)
    group_labels <- labels[group_indices]

    current_line <- 0
    current_position <- 0  # Track the total space occupied by labels

    for (k in seq_along(group_labels)) {
      idx <- group_indices[k]
      output$line[idx] <- current_line
      output$row[idx] <- current_row

      # Calculate string width in pixels for each label
      label_width <- char_pixels(group_labels[k], ...)

      # Add string width to the current position if labels overlap
      if (k < length(group_labels)) {
        current_position <- current_position + label_width + within_labels_gap
        current_line <- current_position
      }

    }


    # Calculate the new value for the row group
    group_values <- values[group_indices]
    new_value <- switch(new_value_method,
                        "average" = mean(group_values),
                        "min" = min(group_values),
                        "max" = max(group_values),
                        "first" = group_values[1])

    # Assign the same new_value for all labels in the group
    output$new_value[group_indices] <- new_value

    # Move to the next row for non-overlapping groups
    current_row <- current_row + 1
  }

  # Round line positions and new_value to ensure precision
  output$line <- signif(output$line, digits = 3)
  output$new_value <- signif(output$new_value, digits = 3)

  return(output)
}
#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Recognize whether the labels should be consecutive or not based
# on numeric value of each character string
# Here use the height of the string
# Through testing a difference in values equal to 0.67 is the
# minimum acceptable difference for labels to not overalap.
# Assumes font = 1 and cex = 2 but can handle
# any font and `cex` combination.
should_be_consecutive <- function(labels,
                                  values,
                                  cex,
                                  ...){

  # Calculate the height of a single label using `strheight()`
  # Use floor() because labels will not overlap if diff in values
  # is close to 1
  label_height <- floor(strheight("M", cex = cex,...))

  # Initialize a logical vector to track overlapping labels
  overlaps <- logical(length(values))

  # Sort values to check consecutive differences only
  sorted_indices <- order(values)
  sorted_values <- values[sorted_indices]

  # Check differences between consecutive sorted values
  consecutive_overlap <- c(FALSE, diff(sorted_values) < 0.67 * label_height)
  overlaps[sorted_indices] <- consecutive_overlap

  return(overlaps)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Color by slope function in which the slopes are calculated
# and colored based on their sign (positive, negative, neutral(0))
color_by_slope <- function(x, y = NULL,
                           cols = c("red","gray30","gray70"),
                           ...){

  # Calculate the slopes between consecutive points
  slopes <- slope(x = x, y = y, ...)

  # Initialize a vector to store colors
  colors <- character(length(slopes))

  # Assign colors based on the sign of the slope
  colors[max(slopes)] <- cols[1]
  colors[min(slopes)] <- cols[2]
  colors[slopes == 0] <- cols[3]

  return(colors)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# This function dynamically adjusts the plot margins (`par(mar)`) in R to ensure that
# all text labels added to the plot fit without overlap. It calculates the margins
# based on the positions of the labels, their sizes, and their arrangement into rows.

# What it does:
# 1. Dynamically adjusts plot margins to ensure all text labels fit without overlap.
# 2. Calculates bottom and top margins based on the vertical positions of labels.
# 3. Calculates left and right margins based on the widest row of labels (including gaps).
# 4. Accounts for label size scaling (`cex`) and adds configurable buffer space.
# 5. Groups labels into rows and ensures sufficient horizontal spacing between labels.
# 6. Automatically applies the calculated margins to the plot using `par(mar)`.
adjust_margins_for_labels <- function(labels,
                                      label_positions = NULL,
                                      row = NULL,
                                      cex = 1,
                                      margin_buffer = 0.5,
                                      within_labels_gap = 0.5,
                                      ...) {

  # Input validation
  if (is.null(label_positions) || is.null(row)) {
    stop("Both label_positions and row must be provided")
  }
  if (length(labels) != length(label_positions) || length(labels) != length(row)) {
    stop("'labels', 'label_positions', and 'row' must have the same length")
  }

  # Estimate the vertical space required for the labels
  label_height <- strheight("M", cex = cex,...)
  label_lines <- range(label_positions)

  # Calculate the required bottom margin based on the vertical distance of the labels
  bottom_margin <- (label_lines[2] - label_lines[1]) * label_height + margin_buffer

  # Calculate the required top margin (some buffer based on the highest position)
  top_margin <- max(label_positions) * label_height + margin_buffer

  # Estimate left and right margins based on the row with the largest cumulative label width
  label_widths <- sapply(labels, function(x) strwidth(x, cex = cex))

  # Group labels by rows and include gaps between labels
  unique_rows <- unique(row)
  row_widths <- sapply(unique_rows, function(current_row) {
    # Get the labels in the current row
    row_labels <- row == current_row
    # Count how many labels are in this row to calculate gaps needed
    n_labels_in_row <- sum(row_labels)
    # Calculate total width: sum of label widths plus gaps between labels
    sum(label_widths[row_labels + (within_labels_gap * (n_labels_in_row - 1))])
  })

  max_row_width <- max(row_widths)

  left_margin <- max_row_width + margin_buffer
  right_margin <- left_margin

  # Set the margins
  margins <- c(bottom_margin, left_margin, top_margin, right_margin)
  par(mar = margins)

  # Return margins invisibly for potential further use
  invisible(margins)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# This function adjusts numeric values slightly to avoid label overlap by adding jitter.
# It is useful when placing overlapping labels on a plot without grouping them into rows.

jitter_labels <- function(values,
                          labels,
                          amount = NULL, # Amount of jitter to apply; default based on range of values.
                          cex = 1,              # Scaling factor for label size.
                          buffer = 0.5,         # Minimum vertical spacing between labels.
                          max_iterations = 100, # Maximum iterations to resolve overlaps.
                          ...) {

  # Input validation
  if (length(values) != length(labels)) {
    stop("The length of 'values' and 'labels' must be the same.")
  }

  # Calculate default jitter amount if not provided
  if (is.null(amount)) {
    amount <- 0.01 * diff(range(values)) # Default: 1% of the value range.
  }

  # Calculate label heights
  label_height <- strheight("M", cex = cex, ...)
  min_spacing <- max(label_height, buffer) # the minimum spacing is the maximum of the label height and buffer

  # Initialize adjusted values
  adjusted_values <- values
  iterations <- 0
  overlaps <- TRUE
  adjusted <- rep(FALSE, length(values)) # Track which values are adjusted
  row <- seq_along(values) # Initialize all labels in the first row

  overlaps <- TRUE

  # Function to check for overlaps
  check_overlap <- function(vals) {
    any(diff(sort(vals)) < min_spacing) # Check if any overlaps exist
  }

  # Iteratively adjust values to avoid overlaps
  while (overlaps && iterations < max_iterations) {
    iterations <- iterations + 1

    # Check for overlaps
    overlaps <- check_overlap(adjusted_values)

    if (overlaps) {
      # Apply jitter to overlapping values
      jittered_values <- adjusted_values + runif(length(values), -amount, amount)

      # Detect overlaps and adjust only the overlapping values
      sorted_indices <- order(adjusted_values)
      diffs <- diff(adjusted_values[sorted_indices])
      overlap_indices <- which(diffs < min_spacing)

      if (length(overlap_indices) > 0) {
        for (idx in overlap_indices) {
          # Jitter the overlapping values
          adjusted_values[sorted_indices[c(idx, idx + 1)]] <- jittered_values[sorted_indices[c(idx, idx + 1)]]
          adjusted[sorted_indices[c(idx, idx + 1)]] <- TRUE
        }
      }
    }
  }

  if (iterations == max_iterations && overlaps) {
    warning("Maximum iterations reached. Some overlaps may remain.")
  }

  # Create output similar to `consecutive_label_position`
  result <- data.frame(
    label = labels,
    value = values,
    adjusted_value = adjusted_values,
    adjusted = adjusted,
    row = row
  )

  return(result)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Helper function to calculate slopes
calculate_slope <- function(x, y, na_rm) {

  if (na_rm) {
    x <- rid_na(x)
    y <- rid_na(y)
  }

  if (length(x) < 2 || length(y) < 2) {
    stop("Not enough valid data points to calculate the slope.")
  }

  if (length(x) != length(y)) {
    stop("Input vectors must have the same length.")
  }

  # Calculate slopes
  return(diff(y) / diff(x))
}

slope <- function(x, na_rm = FALSE, ...) UseMethod("slope")

slope.default <- function(x, y = NULL, na_rm = FALSE, ...) {

  if (missing(y)) stop("Missing argument `y`.")

  stopifnot(
    is.numeric(x),
    is.numeric(y),
    is.logical(na_rm),
    all_finite(x),
    all_finite(y)
  )

  calculate_slope(x, y, na_rm)
}

slope.data.frame <- function(data, na_rm = FALSE, ...) {

  if (!is.data.frame(data)) stop("Input must be a data frame or matrix.")
  if (!is.logical(na_rm)) stop("`na_rm` must be a logical value.")

  data <- factors_as_numeric(data, verbose = TRUE)

  x <- data[[1]]
  y <- data[[2]]

  stopifnot(
    is.numeric(x),
    is.numeric(y)
  )

  calculate_slope(x, y, na_rm)
}

slope.matrix <- function(data, na_rm = FALSE, ...) {

  if (!is.matrix(data)) stop("Input must be a matrix or data frame.")
  if (!is.logical(na_rm)) stop("`na_rm` must be a logical value.")

  data <- as.data.frame(data)
  slope.data.frame(data, na_rm, ...)
}
