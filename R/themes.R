
default_theme <- function(){




}


minimal_theme <- function(...) {

  args <- list(...)

  old_par <- graphics::par(no.readonly = TRUE)

  graphics::par(
    adj = 0.5,
    fg = "black",
    font.axis = 1,
    font.lab = 1,
    font.main = 1,
    pch = 16,
    pch.col = "",
    bty = "n",
    col = palette,
    col.axis = "black",
    col.lab  = "black",
    col.main = "black",
    col.sub = "black",
    lab = c(4,4,6),
    bg = "white",
    pin = c(9, 6),
    tcl = -0.01,
    xaxs = "r",
    xaxt = "s",
    xpd  = FALSE,
    yaxs = "r",
    yaxt = "s",
    mar = c(5, 4, 4, 2) + 0.1,
    mgp = c(3, 1, 0),
    omi = c(0, 0, 0, 0),
    cex.axis = 0.75,
    cex.lab = 1,
    cex.main = 1.25,
    cex.sub = 1,
    rect.col        = NA,
    rect.density    = numeric(),
    rect.angle      = 45,
    rect.lwd        = 1,
    rect.lty        = 1,
    rect.border     = NA,
    args
  )

  invisible(old_par)

}


# Reset original plot parameters
reset_theme <- function(old_par = NULL){

  if (is.null(old_par)) {

    # Reset to R's default graphical parameters
    grDevices::dev.new()
    default_par <- graphics::par(no.readonly = TRUE)
    grDevices::dev.off()

    graphics::par(default_par)
  } else {

    # Reset to the provided old_par settings
    graphics::par(old_par)
  }
  message("Plot parameters reset.")
}


list_themes <- function(){
  c(
    default = default_theme,
    minimal = minimal_theme,
    dark = dark_theme,
    clean = clean_theme,
    chalk = chalk_theme,
    grid = grid_theme,
    void = void_theme
  )
}
