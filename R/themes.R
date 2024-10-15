
default_theme <- function(...){

  palette <-c("#FFBF00","#B92F0A","#792A26","#394DAA","#242424")
  palette_labels <- c()
  palette_numbers <- c()
  palette_missing <- "#97878E"

  args <- list(...)

  list(
    fg  = "black",
    adj = 0.5,
    ann = TRUE,
    bg  = "transparent",
    bty = "l",
    cex = 1,
    cex.axis = 1,
    cex.lab  = 1,
    cex.main = 1.2,
    cex.sub  = 1,
    col = "black",
    col.axis = "black",
    col.lab  = "black",
    col.main = "black",
    col.sub  = "black",
    family   = font_family,
    font  = 1,
    font.axis = 1,
    font.lab  = 1,
    font.main = 2,
    font.sub  = 1,
    lab = c(5, 5, 7),
    las = 0,
    lend  = "round",
    ljoin = "round",
    lmitre = 10,
    lty = "solid",
    lwd = 1,
    mgp = c(3, 1, 0),
    pch = 1,
    tck = NA,
    xaxs = "r",
    xaxt = "s",
    xpd  = FALSE,
    yaxs = "r",
    yaxt = "s",
    rect.col  = NA,
    rect.density = numeric(),
    rect.angle = 45,
    rect.lwd = 1,
    rect.lty= 1,
    rect.border = NA,
    palette = palette,
    palette.numbers = palette_numbers,
    palette.labels = palette_labels,
    palette.missing = palette_missing
  )
}


minimal_theme <- function(...) {

  palette <- c("#121D59","#F29D52","#F24405","#8C1C03","#59190B")
  palette_labels <- c("#121D59","#59190B","#8C1C03","#E5D065","#F29D52")
  palette_numbers <- c("#BF522A","#8C513B","#F24405","#594037","#332C2A")
  palette_missing <- "#97878E"

  args <- list(...)

  list(
    adj = 0.5,
    fg = "black",
    font.axis = 1,
    font.lab = 1,
    font.main = 1,
    pch = 20,
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
    mgp = c(2.5, 0.2, 0),
    omi = c(0, 0, 0, 0),
    cex.axis = 0.75,
    cex.lab = 1,
    cex.main = 1.25,
    cex.sub = 1,
    rect.col = NA,
    rect.density = numeric(),
    rect.angle  = 45,
    rect.lwd = 1,
    rect.lty = 1,
    rect.border = NA,
    palette = palete,
    palette.numbers = palette_numbers,
    palette.labels = palette_labels,
    palette.missing = palette_missing,
    args
  )
}

chalk_theme <- function(...){

  palette <- c("gray89","gray95","#CFCFCF","#4B4B4B","#2A2A2A")
  palette_labels <- c("gray95","gray55","gray25","#2a2a2a","black")
  palette_numbers <- c("gray89","gray25","#2a2a2a","black")
  palette_missing <- "white"

  args <- list(...)

  list()
}

dark_theme <- function(...){

  palette <- c("#BAC1BF", "#ED2939", "#3870C2", "#F2D63B", "#777B7E", "#F2A83B", "#8A6842")
  palette_numbers <- c("#3870C2", "#00B7EB", "#F2D63B", "#FC6600", "#ED2939")
  palette_labels  <- c("#3870C2", "#8AC1D4", "#DA2E20", "#EB8677", "#8A6842")
  palette_missing <- c("#777B7E")

  args <- list(...)

  list(fg              = "#7E848C",
       adj             = 0.5,
       ann             = TRUE,
       bg              = "#2E3947",
       bty             = "n",
       cex             = 0.8,
       cex.axis        = 1,
       cex.lab         = 1,
       cex.main        = 1.5,
       cex.sub         = 1,
       col             = "#BEBEBE",
       col.axis        = "#7E848C",
       col.lab         = "#BEBEBE",
       col.main        = "#EFF0F2",
       col.sub         = "#737D89",
       family          = "",
       font            = 1,
       font.axis       = 1,
       font.lab        = 2,
       font.main       = 2,
       font.sub        = 3,
       lab             = c(5, 5, 7),
       las             = 1,
       lend            = "round",
       ljoin           = "round",
       lmitre          = 10,
       lty             = 1,
       lwd             = 1,
       mgp             = c(3, 0.7, 0),
       pch             = 19,
       tck             = -0.01,
       xaxs            = "r",
       xaxt            = "s",
       xpd             = FALSE,
       yaxs            = "r",
       yaxt            = "s",
       rect.col        = NA,
       rect.density    = numeric(),
       rect.angle      = 45,
       rect.lwd        = 1,
       rect.lty        = 1,
       rect.border     = NA,
       palette         = palette,
       palette.numbers = palette_numbers,
       palette.labels  = palette_labels,
       palette.missing = palette_missing,
       args
  )
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
