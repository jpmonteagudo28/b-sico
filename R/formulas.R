# Check if the input is a formula
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
