## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Configuration file
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     October 20, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(magrittr, include.only = c("%>%"))

# SharePoint Paths

paths <- list(
  "carlostoruno" = list(
    "path2DA"  = "/Users/carlostoruno/OneDrive - World Justice Project/Data Analytics",
    "path2GPP" = "/Users/carlostoruno/OneDrive - World Justice Project/General Population Poll" 
  ),
  "santiagopardo" = list(
    "path2DA"  = "/Users/santiagopardo/OneDrive - World Justice Project/Data Analytics",
    "path2GPP" = "/Users/santiagopardo/OneDrive - World Justice Project/General Population Poll" 
  )
)

# Helper function to print verbose messages
verbose_message <- function(message) {
  if (opt$verbose) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(paste0("[INFO ", timestamp, "] ", message, "\n"))
  }
}

# Load Inter Tight fonts
# --> The WJPr package needs a fix to enable flexible font families beyond Lato,
# --> so I'm just doing this manually and link the "Lato" family to the 
# --> Inter Tight files

load_inter_fonts <- function(){
  path2DASP  <- paths[[Sys.info()[['user']]]][['path2DA']]
  path2fonts <-  glue::glue("{path2DASP}/6. Country Reports/0. Fonts")
  
  sysfonts::font_add(
    family     = "Lato Full",
    regular    = glue::glue("{path2fonts}/InterTight-Regular.ttf"),
    bold       = glue::glue("{path2fonts}/InterTight-Bold.ttf"),
    italic     = glue::glue("{path2fonts}/InterTight-Italic.ttf"),
    bolditalic = glue::glue("{path2fonts}/InterTight-BoldItalic.ttf")
  )
  sysfonts::font_add(
    family     = "Lato Light",
    regular    = glue::glue("{path2fonts}/InterTight-Light.ttf"),
    bold       = glue::glue("{path2fonts}/InterTight-Light.ttf"),
    italic     = glue::glue("{path2fonts}/InterTight-LightItalic.ttf"),
    bolditalic = glue::glue("{path2fonts}/InterTight-LightItalic.ttf")
  )
  sysfonts::font_add(
    family     = "Lato Black",
    regular    = glue::glue("{path2fonts}/InterTight-Black.ttf"),
    bold       = glue::glue("{path2fonts}/InterTight-Black.ttf"),
    italic     = glue::glue("{path2fonts}/InterTight-BlackItalic.ttf"),
    bolditalic = glue::glue("{path2fonts}/InterTight-BlackItalic.ttf")
  )
  sysfonts::font_add(
    family     = "inter",
    regular    = glue::glue("{path2fonts}/InterTight-Regular.ttf"),
    bold       = glue::glue("{path2fonts}/InterTight-Bold.ttf"),
    italic     = glue::glue("{path2fonts}/InterTight-Italic.ttf"),
    bolditalic = glue::glue("{path2fonts}/InterTight-BoldItalic.ttf")
  )
  showtext::showtext_auto()
}
