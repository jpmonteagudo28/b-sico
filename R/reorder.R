# Reorder columns based on position
reorder <- function(.data,
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
