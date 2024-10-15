
default_theme <- function(...){

  palette         <- c("black","#F15854","#dc8951", "#4E8AC9", "#F5D06D", "#69ca97", "#c978b5")
  palette_numbers <- c()
  palette_labels  <- c()
  palette_missing <- "#97878E"

  args <- list(...)

  list(
    fg               = "black",
    adj              = 0.5,
    ann              = TRUE,
    bg               = "transparent",
    bty              = "o",
    cex              = 1,
    cex.axis         = 1,
    cex.lab          = 1,
    cex.main         = 1.2,
    cex.sub          = 1,
    col              = "black",
    col.axis         = "black",
    col.lab          = "black",
    col.main         = "black",
    col.sub          = "black",
    font             = 1,
    font.axis        = 1,
    font.lab         = 1,
    font.main        = 2,
    font.sub         = 1,
    lab              = c(5, 5, 7),
    las              = 0,
    lend             = "round",
    ljoin            = "round",
    lmitre           = 10,
    lty              = "solid",
    lwd             = 1,
    mgp             = c(3, 1, 0),
    pch             = 1,
    tck             = NA,
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
    palette.missing = palette_missing
  )
}


minimal_theme <- function(...) {

  palette         <- c("#121D59","#F15854","#F24405","#8C1C03","#59190B")
  palette_numbers <- c("#BF522A","#8C513B","#F24405","#594037","#332C2A")
  palette_labels  <- c("#121D59","#59190B","#8C1C03","#E5D065","#F29D52")
  palette_missing <- "#97878E"

  args <- list(...)

  list(
    adj             = 0.5,
    ann             = FALSE,
    fg              = "black",
    font.axis       = 1,
    font.lab        = 1,
    font.main       = 1,
    pch             = 20,
    pch.col         = "",
    bty             = "n",
    col             = "black",
    col.axis        = "black",
    col.lab         = "black",
    col.main        = "black",
    col.sub         = "black",
    lab             = c(4,4,6),
    bg              = "white",
    pin             = c(9, 6),
    tcl             = -0.01,
    xaxs            = "r",
    xaxt            = "s",
    xpd             = FALSE,
    yaxs            = "r",
    yaxt            = "s",
    mar             = c(5, 4, 4, 2) + 0.1,
    mgp             = c(2.5, 0.2, 0),
    omi             = c(0, 0, 0, 0),
    cex.axis        = 0.75,
    cex.lab         = 1,
    cex.main        = 1.25,
    cex.sub         = 1,
    rect.col        = NA,
    rect.density    = numeric(),
    rect.angle      = 45,
    rect.lwd        = 0.5,
    rect.lty        = 1,
    rect.border     = NA,
    palette         = palete,
    palette.numbers = palette_numbers,
    palette.labels  = palette_labels,
    palette.missing = palette_missing,
    args
  )
}

grey_theme <- function(...){

  palette         <- c("gray95","gray89","#CFCFCF","#4B4B4B","#2A2A2A")
  palette_numbers <- c("gray89","gray25","#2a2a2a","black")
  palette_labels  <- c("gray95","gray55","gray25","#2a2a2a","black")
  palette_missing <- "orangered"

  args <- list(...)

  list(
    adj                 = 0.5,
    fg                  = "grey20",
    font.axis           = 1,
    font.lab            = 1,
    font.main           = 1,
    pch                 = 20,
    pch.col             = "",
    bty                 = "l",
    col                 = "gray20",
    col.axis            = "gray20",
    col.lab             = "gray20",
    col.main            = "black",
    col.sub             = "gray40",
    lab                 = c(10,10,7),
    las                 = 1,
    bg                  = "#ffffff",
    pin                 = c(9, 6),
    tcl                 = -0.01,
    xaxs                = "r",
    xaxt                = "s",
    xpd                 = FALSE,
    yaxs                = "r",
    yaxt                = "s",
    mar                 = c(5, 4, 4, 2) + 0.1,
    mgp                 = c(2.5, 0.2, 0),
    omi                 = c(0, 0, 0, 0),
    cex.axis            = 0.85,
    cex.lab             = 1,
    cex.main            = 1.25,
    cex.sub             = 1,
    rect.col            = NA,
    rect.density        = numeric(),
    rect.angle          = 45,
    rect.lwd            = 2,
    rect.lty            = 1,
    rect.border         = "gray20",
    palette             = palete,
    palette.numbers     = palette_numbers,
    palette.labels      = palette_labels,
    palette.missing     = palette_missing,
    args
  )
}

dark_theme <- function(...){

  palette         <- c("#BAC1BF", "#ED2939", "#3870C2", "#F2D63B", "#777B7E", "#F2A83B", "#8A6842")
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

void_theme <- function(...){

  palette         <- c("#071216","#A61103", "#F15854", "#590202", "#5DA5DA", "#B276B2", "#DECF3F", "#260101")
  palette_numbers <- c("#4F61A1", "#5DA5DA", "#66E3D9", "#590202", "#FAA43A", "#F15854")
  palette_labels  <- c("#5DA5DA", "#FAA43A", "#60BD68", "#F15854", "#B276B2", "#8D4B08", "#DECF3F", "#F17CB0")
  palette_missing <- "#B4B4B4"

  args <- list(...)

  list(fg              = "black",
       adj             = 0.5,
       ann             = FALSE,
       bg              = "white",
       bty             = "n",
       cex             = 1,
       cex.axis        = 1.1,
       cex.lab         = 1.1,
       cex.main        = 1.5,
       cex.sub         = 1,
       col             = "black",
       col.axis        = "black",
       col.lab         = "black",
       col.main        = "black",
       col.sub         = "black",
       family          = "",
       font            = 1,
       font.axis       = 1,
       font.lab        = 1,
       font.main       = 2,
       font.sub        = 2,
       lab             = c(4,4,7),
       las             = 1,
       lend            = 0,
       ljoin           = 0,
       lmitre          = 10,
       lty             = 1,
       lwd             = 2,
       mgp             = c(3,0.5,0),
       pch             = 19,
       tck             = 0,
       xaxs            = "r",
       xaxt            = "n",
       xpd             = FALSE,
       yaxs            = "r",
       yaxt            = "n",
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

crisp_theme <- function(...){

  palette         <- c("#071216","#BF0413","#03178C","#0378A6","#03A688","#038C73","#F2B705","#F27405","#F2E6CE")
  palette_numbers <- c("#4F61A1", "#5DA5DA", "#66E3D9", "#590202", "#FAA43A", "#F15854")
  palette_labels  <- c("#5DA5DA", "#FAA43A", "#60BD68", "#F15854", "#B276B2", "#8D4B08", "#DECF3F", "#F17CB0")
  palette_missing <- "#B4B4B4"

  args <- list(...)

  list(
    fg              = "#1B2D2E",
    adj             = 0.5,
    ann             = TRUE,
    bg              = "#FFFCFA",
    bty             = "o",
    cex             = 1,
    cex.axis        = 0.8,
    cex.lab         = 0.8,
    cex.main        = 1.2,
    cex.sub         = 0.8,
    col             = "#822107",
    col.axis        = "#822107",
    col.lab         = "#1B2D2E",
    col.main        = "#1B2D2E",
    col.sub         = "#9DB094",
    family          = "serif",
    font            = 1,
    font.axis       = 1,
    font.lab        = 2,
    font.main       = 3,
    font.sub        = 1,
    lab             = c(5, 5, 7),
    las             = 1,
    lend            = "round",
    ljoin           = "round",
    lmitre          = 10,
    lty             = 1,
    lwd             = 1.2,
    mgp             = c(3, 0.7, 0),
    pch             = 21,
    tck             = -0.015,
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
    palette.missing = palette_missing
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
    dark    = dark_theme,
    crisp   = crisp_theme,
    grey    = grey_theme,
    void    = void_theme
  )
}
