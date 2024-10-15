#' @title Set custom theme for base R plots
#'
#' @description
#' Sets and returns base plotting theme parameters
#'
#' @param ... arguments to be passed as \code{parameter = value} pairs
#'
#' @details
#' 1. The user can pass no arguments, in which case, the current theme will be returned
#' as a list \cr
#' 2. NULL - the current theme settings will be removed \cr
#' 3. A list can be passed as an argument. The new list will supersede the current theme.
#'
#' @return a list of all theme settings, invisibly.
#' @export
#'
#' @examples
#' # Set theme by name
#' set_theme("crisp")
#' plot(0:10,0:10)
#'
#' # Get list of theme parameters
#' set_theme()
#'
#' # Reset theme
#' set_theme(NULL)
#'

set_theme <- function(...){

  # Get current theme if no arguments passed
  if(nargs() == 0){
    return(current_theme())
  }

  # Collect and organize parameters
  pars <- list(...)
  n <- length(pars)

  nome <- names(pars)
  old_pars <- current_theme()
  novo <- names(old_pars)

    # Create fallback color palette
  .fall_back <- hcl.colors(8,"RdYlBu")

  if(is.null(nome)){
    names(pars) <- rep("",n)
  }

  # Check the palette setting
  if(nome[1] == ""){
    if(is.null(pars[[1]])){
      grDevices::palette(.fall_back)
      old_pars <- list()
      pars <- pars[-1]
    } else if(is.list(pars[[1]])){
      pars <- c(pars[[1]],pars[-1])
    } else if(is.character(pars[[1]])){
      pars <- c(get_theme[[1]], pars[-1])
    }
  }

  if(any(nome == "")){
    warning("unnamed arguments were not used")
    pars <- pars[nome != ""]
  }

  if(any(duplicated(nome))){
    pars <- pars[!duplicated(nome), fromLast = TRUE]
  }

  # Use default theme
  template <- get_theme("default")
  if(n > 0 & any(!nome %in% names(template))){
    warning("unnamed arguments were not used")
    pars <- pars[nome %in% names(template)]
  }

  pars <- c(old_pars[!novo %in% nome], pars)
  pars <- Filter(Negate(is.null),pars)

  hook_it <- getHook("before.plot.new")
  hook_it$par <- set_pars(pars)
  hook_it$palette <- set_palette(pars)
  setHook("before.plot.new",hook_it,"replace")

  hook_that <- getHook("plot.new")
  hook_that$rect <- set_rect(pars)
  setHook("plot.new",hook_that,"replace")

  invisible(pars)

}
