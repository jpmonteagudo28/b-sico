# Add jitter to plots
type_jitter <- function(factor = 1,
                       amount = NULL){

  out = list(
    draw =,
    data = ,
    name = "p"

  )

  class(out) = "chart_type"
  return(out)
}

data_jitter = function(factor, amount){

  fun = function(data, ...) {
    x = data$x
    y = data$y
    if (is.character(x)) x = as.factor(x)
    if (is.character(y)) y = as.factor(y)
    if (is.factor(x)) {
      x_lvls = levels(x)
      x_labs = seq_along(x_lvls)
      names(x_labs) = x_lvls
      x = as.integer(x)
    } else {
      x_labs = NULL
    }
    if (is.factor(y)) {
      y_lvls = levels(y)
      y_labs = seq_along(y_lvls)
      names(y_labs) = y_lvls
      y = as.integer(y)
    } else {
      ylabs = NULL
    }
    x = jitter(x, factor = factor, amount = amount)
    y = jitter(y, factor = factor, amount = amount)

    data$x = x
    data$y = y

    out = list(
      data = data,
      x = x,
      y = y,
      xlabs = xlabs,
      ylabs = ylabs
    )
    return(out)
  }
}
