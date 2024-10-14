#' @title A lightweight extension of the base R plotting function
#'

chart = function(x,...){
  UseMethod("chart")
}

chart.default = function(
    )
