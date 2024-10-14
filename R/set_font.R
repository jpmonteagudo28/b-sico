#' @title  Set Google font for base R and ggplot2 plots
#'
#' @description
#' Set Google font as default font for all graphing devices prior to starting new plot. If font set
#' after graphing device initialized, R will use default font family parameters.
#'
#' @param name Google font name
#' @param family font family name to be used in R. 'family' can take any character argument.
#' If no family name provided, R will use the Google font name as default.
#'
#' @export
#' @return temporarily invisible copy of Google font name
#'
#' @examples
#' set_font("Montserrat","monse")
#'
#' hist(
#' rnorm(100),
#' xlab = "x-axis"),
#' ylab = "y-axis"
#' )
set_font <- function(name,family){

  if(missing(family)){
    family = name
  }

  required_packages <- c("showtext", "sysfonts")
  missing_packages <- required_packages[!sapply(required_packages,
                                                requireNamespace, quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {

    stop("The following required package(s) are not installed: ",
         paste(missing_packages, collapse = ", "),
         ". Please install them using install.packages().")
  }

  # Add the Google font
  tryCatch({
    sysfonts::font_add_google(name, family)
  }, error = function(e) {
    stop("Failed to add Google font. Error: ", e$message)
  })

  # Enable custom font for all devices
  showtext::showtext_auto()

  # Set the font as the default for base R plots
  graphics::par(family = family)

  message("Plot font set to '", name, "'. Use 'reset_font' to reset to default.")

  invisible(family)
}

#' @title Reset previous or default font used in every graphing device
#'
#' @description
#' Reset fonts to default system fonts, 'sans','serif', or 'mono' or previously used font
#'
#' @param family Optional argument to reset fonts to previously used font. No argument needed
#' if setting font to default system fonts
#'
#' @export
#' @return message confirming default font change
#'
#' @examples
#' set_plot_font("Libre Franklin")
#'
#' hist(
#' rnorm(100),
#' xlab = "x-axis",
#' ylab = "y-axis"
#' )
#'
#' reset_plot_font()
#'
#'hist(
#' rnorm(100),
#' xlab = "x-axis",
#' ylab = "y-axis"
#' )
reset_font <- function(family){

  if(missing(family)){
    family <- "sans"
  }

  graphics::par(family = family)
  showtext::showtext_auto(enable = FALSE)

  message("Plot font set to '", family, "'.")
}

