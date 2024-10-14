#' @title List of Cuban paintings color palettes and the order in which they're printed
#'
#' @description Complete list of Cuban paintings color palette
#'
#' @details
#' Use names(basic_palettes) to return a list of all possible palette names. Currently,
#' the following palettes are available:
#' \code{imagenes},\code{primvera_descanso},\code{no_aguanto_mas},\code{emilio_rodriguez},
#' \code{paisaje_bohio},\code{productivismo}, \code{retrato_marin}, \code{helena_herrera},
#' \code{emarque_colon}, \code{havana}, and \code{guajiros}.
#' Use \code{\link{basic_palettes}} to construct palettes.
#'
#'@export

basic_palettes <- list(

  imagenes = c("#400330","#049DD9","#F2B705","#D97904","#D92546","#A60303"),

  primavera_descanso = c("#034AA6","#034C8C","#BACBD9","#8F8B89","#025939","#012619","#8C4303"),

  no_aguanto_mas = c("#BF0413","#03178C","#0378A6","#03A688","#038C73","#F2B705","#F27405","#F2E6CE"),

  emilio_rodriguez = c("#034C8C","#0367A6","#02735E","#012623","#D9B959","#A65D03","#8C0303","#400101"),

  paisaje_bohio = c("#8A9EA6","#A6813C","#402C13","#594A1D","#8C5E35","#BFA68F","#260801"),

  productivismo = c("#8C530D","#F2CEA2","#D9831A","#401201","#8C1F07","#F2F2F2","#736758"),

  retrato_marin = c("#3F6CA6","#BF9C5A","#F2DCC2","#59321C","#D98841","#F2522E","#A63E26","#0D0D0D"),

  helena_herrera = c("#D9B036","#BF7F5A","#D9965B","#F2C7AE","#590202","#260101","#735C5C"),

  embarque_colon = c("#9CBCD9","#0378A6","#A9CBD9","#D9C2A7","#F28322","#A63F03","#590202","#594B02","#401801"),

  havana = c("#E9F2EF","#6DA0A6","#A3BFBF","#BFB78E","#D2D9D3","#687372","#260101"),

  guajiros = c("#735D34","#D9BB84","#F2D5A0","#A65526","#A6926D","#BF622C","#592918")
)

basic_palette <- function(name,
                           n,
                           type = c("discrete","continuous"),
                           direction = c("r","l")
                           ){

  type <- match.arg(type,
                    c("discrete","continuous"),
                    several.ok = FALSE)

  palette <- basic_palettes[[name]]

  if(is.null(palette))
    stop("Palette not found")

  if(missing(n))
    n <- length(palette)

  if (missing(direction)) {
    direction <- "l"
  }

  if(direction %!in% c("r","l"))
    stop("Invalid direction. Use 'l' for the standard order palette or 'r' for reversed palette")

  if(missing(type)){

    if(n > length(palette[[1]])){

      type <- "continuous"
    } else{
      type <- "discrete"
    }
  }

  if(type == "discrete" && n > length(palette)){
    stop("Requested number of colors exceeds palette")
  }

  out <- switch(
    type,
    continuous = grDevices::colorRampPalette(palette)(n),
    discrete = palette[1:n]
  )

  structure(out,class = "palette", name = name)
}

print.palette <- function(.x,...){

  n <- length(.x)
  old_par <- graphics::par(mar = c(0.5,0.5,0.5,0.5))
  on.exit(graphics::par(old_par))

  image(
    1:n,
    1,
    as.matrix(1:n),
    col = .x,
    ylab = NULL,
    xaxt = "n",
    yaxt = "n",
    bty = "n")

  rect(0,
       0.9,
       n+1,
       1.1,
       col = rgb(1,1,1,0.8),
       border = NA)

  text(
    (n+1)/2,
    1,
    labels = attr(.x,"name"),
    cex = 1,
    family = "sans"
  )
}
