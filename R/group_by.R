group_by <- function(.data, ...) {
  is_dataframe(.data)
  groups <- deparse_dots(...)
  unknown <- !(groups %in% colnames(.data))
  if (any(unknown)) stop("Invalid groups: ", extract(groups, unknown))
  structure(.data, class = c("grouped_data", class(.data)), groups = groups)
}

#' @param x A `data.frame`.
#' @rdname groups
#' @export
ungroup <- function(x, ...) {
  is_dataframe(x)
  rm_groups <- deparse_dots(...)
  groups <- attr(x, "groups")
  if (length(rm_groups) == 0L) rm_groups <- groups
  attr(x, "groups") <- extract(groups, !(groups %in% rm_groups))
  if (length(attr(x, "groups")) == 0L) {
    attr(x, "groups") <- NULL
    class(x) <- extract(class(x), !(class(x) %in% "grouped_data"))
  }
  x
}

apply_grouped_function <- function(.data, fn, ...) {
  groups <- attr(.data, "groups", exact = TRUE)
  grouped <- split_into_groups(.data, groups)
  res <- do.call(rbind, unname(lapply(grouped, fn, ...)))
  if (any(groups %in% colnames(res))) {
    class(res) <- c("grouped_data", class(res))
    attr(res, "groups") <- extract(groups, groups %in% colnames(res))
  }
  res
}

split_into_groups <- function(.data, groups) {
  class(.data) <- "data.frame"
  group_factors <- lapply(groups, function(x, .data) as.factor(extract2(.data, x)), .data)
  res <- split(x = .data, f = group_factors)
  res
}

#' @export
print.grouped_data <- function(x, ...) {
  class(x) <- "data.frame"
  print(x, ...)
  cat("\nGroups: ", paste(attr(x, "groups", exact = TRUE), collapse = ", "), "\n\n")
}
