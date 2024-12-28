#' Add and Set Custom Fonts for Plotting
#'
#' @description
#' Loads and sets custom fonts for use in R plots. The function can load fonts from
#' Google Fonts, system-wide font directories, or local user font directories.
#'
#' @param name Character string. The name of the font to load. For Google Fonts,
#'   this should be the font family name. For system fonts, this should be the
#'   font file name without extension.
#' @param family Character string. Optional name to use for the font family in R.
#'   If not provided, defaults to the value of `name`.
#' @param google Logical. If `TRUE`, loads font from Google Fonts. If `FALSE`,
#'   loads from system or local font directories. Defaults to `TRUE`.
#' @param locally Logical. Only used when `google = FALSE`. If `TRUE`, searches in
#'   user's local font directory. If `FALSE`, searches in system-wide font
#'   directory. Defaults to `FALSE`.
#'
#' @return Invisibly returns the font family name.
#'
#' @details
#' The function provides different ways to load fonts depending on their source:
#'
#' \strong{Font Names vs Font Families:}
#' Font names and font families are not always equivalent. Compound family names
#' often get joined together without spaces and include style indicators. For example:
#' - Family: "Source Sans Pro"
#' - Name: "SourceSansPro-Bold"
#'
#' When choosing a locally installed font, you should provide the name as it appears
#' in your system. You can discover available font names using
#' `systemfonts::system_fonts()`, which returns a tibble containing paths, names,
#' families, styles, weights, and other parameters for all installed fonts.
#'
#' \strong{Google Fonts vs System Fonts:}
#' - Google Fonts uses family names (e.g., "Roboto", "Source Sans Pro")
#' - System fonts use font names (e.g., "Roboto-Bold", "SourceSansPro-Regular", "times" for Times New Roman)
#' The function attempts to create the font name by joining family name words, but
#' this may not always succeed. It's recommended to check your font folder or use
#' `systemfonts::system_fonts()` to confirm correct names.
#'
#' \strong{Font Installation Locations:}
#' System fonts are installed in different locations depending on installation method
#' and operating system:
#'
#' Windows:
#' - System-wide: C:\\Windows\\Fonts
#' - Local user: C:\\Users\\[username]\\AppData\\Local\\Microsoft\\Windows\\Fonts
#'
#' macOS:
#' - System-wide: /Library/Fonts
#' - Local user: ~/Library/Fonts
#'
#' Linux:
#' - System-wide: /usr/share/fonts or /usr/local/share/fonts
#' - Local user: ~/.fonts
#'
#' By default, fonts are saved locally unless installed system-wide during
#' installation (e.g., by right-clicking and selecting "Install for all Users" on
#' Windows).
#'
#' @examples
#' \dontrun{
#' # Load a Google Font
#' set_font("Roboto")
#'
#' # Load a system-wide installed font
#' set_font("Arial", google = FALSE)
#'
#' # Load a locally installed font
#' set_font("SourceSansPro-Bold", google = FALSE, locally = TRUE)
#'
#' # Reset to default font
#' reset_font()
#' }
#'
#' @note
#' The function requires the packages 'showtext' and 'sysfonts' to be installed.
#' For system fonts, the font file must exist in the appropriate directory and
#' have a .ttf or .otf extension.
#'
#' @seealso
#' \code{\link[sysfonts]{font_add_google}} for adding Google Fonts
#' \code{\link[systemfonts]{system_fonts}} for listing available system fonts
#'
#' @export
#' hist(
#' rnorm(100),
#' xlab = "x-axis",
#' ylab = "y-axis",
#' main = "Example Plot",
#' cex.main = 2
#' )
set_font <- function(name,
                     family,
                     google = TRUE,
                     locally = FALSE) {

 if(missing(name) && missing(family)){
    stop("Please provide a font name or family name.")
 }

  if(missing(family)) {
    family <- name
  }

  if(missing(name)){
    name <- replace_char(family, " ", "")
  }

  required_packages <- c("showtext", "sysfonts")
  missing_packages <- required_packages[!sapply(required_packages,
                                                requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("The following required package(s) are not installed: ",
         paste(missing_packages, collapse = ", "),
         ". Please install them using install.packages().")
  }

  if (google) {
    tryCatch({
      sysfonts::font_add_google(name, family)
      message("Google font '", name, "' added successfully.")
    }, error = function(e) {
      stop("Failed to add Google font. Error: ", e$message)
    })
  } else {
    tryCatch({
      if (.Platform$OS.type == "windows") {
        # Choose directory based on locally parameter
        if(locally) {
          font_dir <- file.path(Sys.getenv("LOCALAPPDATA"),
                                "Microsoft/Windows/Fonts")
          dir_type <- "local"
        } else {
          font_dir <- file.path(Sys.getenv("SystemRoot"), "Fonts")
          dir_type <- "system"
        }

        # Create search pattern
        pattern <- gsub(" ", ".*", name)
        font_extensions <- c(".ttf", ".otf", ".TTF", ".OTF")
        ext_pattern <- paste0(".*", pattern, ".*(",
                              paste(font_extensions, collapse = "|"),
                              ")$")

        # Search for fonts
        font_files <- list.files(font_dir,
                                 pattern = ext_pattern,
                                 full.names = TRUE,
                                 ignore.case = TRUE)

        if (length(font_files) == 0) {
          # Show available fonts if none found
          available_fonts <- list.files(font_dir,
                                        pattern = ".*\\.(ttf|otf)$",
                                        ignore.case = TRUE)
          message("\nAvailable fonts in ", dir_type, " directory (", font_dir, "):")
          print(grep(pattern, available_fonts, value = TRUE, ignore.case = TRUE))
          stop("Font file not found for '", name, "' in the ", dir_type, " fonts directory")
        }

        # Use the first matching font file
        font_path <- normalizePath(font_files[1], winslash = "/")

        # Add the font
        sysfonts::font_add(family, regular = font_path)
        message("Font '", name, "' added successfully from ", dir_type, " directory: ", font_path)

      } else if (.Platform$OS.type == "unix") {
        if(locally) {
          possible_dirs <- c(
            "~/Library/Fonts",                    # macOS user
            paste0(Sys.getenv("HOME"), "/.fonts") # Linux user
          )
        } else {
          possible_dirs <- c(
            "/Library/Fonts",                     # macOS system
            "/usr/share/fonts",                   # Linux system
            "/usr/local/share/fonts"              # Linux local
          )
        }
        font_dir <- Filter(dir.exists, possible_dirs)[1]
        if (is.na(font_dir)) {
          stop("Could not find ", ifelse(locally, "local", "system"), " font directory")
        }
        # Rest of Unix font handling...
      } else {
        stop("Unsupported operating system")
      }
    }, error = function(e) {
      stop("Failed to add font: ", e$message)
    })
  }

  # Enable custom font for all devices
  showtext::showtext_auto()

  # Set the font as the default for base R plots
  graphics::par(family = family)

  message("Plot font set to '", family, "'. Use 'reset_font()' to reset to default.")
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
    family <- "serif"
  }

  graphics::par(family = family)
  showtext::showtext_auto(enable = FALSE)

  message("Plot font set to '", family, "'.")
}

#---- --- ---- --- ---- --- ---- --- ---- --- ----#
install_font_locally <- function(font_folder) {

  # Check if the folder exists
  if (!dir.exists(font_folder)) {
    stop("The specified folder does not exist. Please provide a valid folder path.")
  }

  # Find all font files (assumes .ttf and .otf files)
  font_files <- list.files(font_folder, pattern = "\\.(ttf|otf)$", full.names = TRUE)
  if (length(font_files) == 0) {
    stop("No font files found in the specified folder.")
  }

  # Determine system font directory
  font_dir <- switch(
    .Platform$OS.type,
    "windows" = normalizePath(file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft", "Windows", "Fonts"), winslash = "/"),
    "unix" = if (Sys.info()[["sysname"]] == "Darwin") {
      # macOS font directory
      "~/Library/Fonts"
    } else {
      # Linux font directory
      "~/.fonts"
    },
    stop("Unsupported operating system.")
  )

  # Expand path if necessary
  font_dir <- path.expand(font_dir)

  # Copy font files to the font directory
  for (font_path in font_files) {
    font_name <- basename(font_path)
    target_path <- file.path(font_dir, font_name)

    if (!file.exists(target_path)) {
      message("Installing font: ", font_name)
      file.copy(font_path, target_path, overwrite = TRUE)
    } else {
      message("Font already installed: ", font_name)
    }
  }

  # Notify user to restart applications
  message("Font installation complete. You may need to restart applications to use the new fonts.")

  # Optionally register fonts for immediate use in R
  for (font_path in font_files) {
    font_name <- tools::file_path_sans_ext(basename(font_path))
    message("Registering font in R: ", font_name)
    systemfonts::register_font(name = font_name, plain = font_path)
  }

  showtext::showtext_auto() # Automatically enable showtext rendering
  message("Fonts registered and ready for use!")
}
