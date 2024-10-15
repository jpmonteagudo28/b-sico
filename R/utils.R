# Simple negation function to find excluded elements in a set
`%!in%` <- Negate(`%in%`)

# Check current theme used in graphing device
current_theme <- function(){

  hook_before <- getHook("before.plot.new")
  hook_now <- getHook("plot.new")

  theme <- list()

  if(is.function(hook_before$par)){
    # retrieve setting but don't evaluate
    theme <- c(theme,hook_before$par(FALSE))
  }

  if(is.function(hook_before$palette)){
    # retrieve settings but don't evaluate
    theme <- c(theme,hook_before$palette(FALSE))
  }

  if(is.function(hook_now$rect)){
    # retrieve settings but don't evaluate
    theme <- c(theme,hook_now$rect(FALSE))
  }

  return(theme)
}

# Get the specified theme
get_theme <- function(theme = NULL){

  themes <- list_themes()

  if(theme %in% names(themes)){
    themes[[theme]]()
  } else{

    stop("The theme specified doesn't exist:",theme,".\nAvailable themes:",
         paste(names(list_themes())), collapse = ",")
  }
}

set_pars <- function(pars){

  pars_list <- pars

  # Exclude 'palette' and 'rect' from 'par()'
    pars_list[grep("^palette\\.?",names(pars))] <- NULL
    pars_list[grep("^rect\\.?",names(pars))] <- NULL

    function(set= TRUE){

      if(set){
        pars_list$new <- graphics::par('new')
        do.call(graphics::par,pars_list)
      } else{

        pars_list
      }
    }
}

set_palette <- function(pars) {

  # Try to get palette from `pars`; if absent, call `use_current_palette()`
  palette_pars <- pars[grep("^palette\\.?", names(pars))]
  palette_from_env <- if (is.null(palette_pars$palette)) {

    tryCatch(
      use_current_palette(),
      error = function(e) NULL
    )
  } else {
    palette_pars$palette
    }

  # Check if a palette was found either in `pars` or via `use_current_palette()`
  if (!is.null(palette_from_env)) {
    # Set the palette
    grDevices::palette(value = palette_from_env)


    function(set = TRUE) {
      if (set) {
        grDevices::palette(value = palette_from_env)
      } else {
        list(palette = palette_from_env)
      }
    }
  } else {
    NULL
  }
}

# Check if palette has been set prior to using themes with default palettes.
use_current_palette <- function(){

  if(!exists("current_palette",envir = palette_env)){
    stop("No palette has been set yet.
         \nUse basic_palette() to set a palette before calling
         'use_current_palette()'.")
  }

  return(get("current_palette", envir = palette_env))
}

# Set rectangle settings in graphing device
set_rect <- function(pars){

  pars <- pars[grep("^rect\\.?",names(pars))]

  if(length(pars) > 0){
    function(set = TRUE){
      if(set){
        rect_list <- list(
          xleft   = graphics::par("usr")[1],
          ybottom=graphics::par("usr")[3],
          xright  = graphics::par("usr")[2],
          ytop=graphics::par("usr")[4],
          col     = ifelse(is.null(pars$rect.col), NA, pars$rect.col),
          density = ifelse(is.null(pars$rect.density), numeric(), pars$rect.density),
          angle   = ifelse(is.null(pars$rect.angle), 35, pars$rect.angle),
          lwd     = ifelse(is.null(pars$rect.lwd), 1, pars$rect.lwd),
          lty     = ifelse(is.null(pars$rect.lty), 1, pars$rect.lty),
          border  = ifelse(is.null(pars$rect.border), NA, pars$rect.border)
        )
      } else {
        pars
      }
    }
  } else{
    NULL
  }
}
