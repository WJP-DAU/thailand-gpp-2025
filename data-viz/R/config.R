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
  )
)

# Helper function to print verbose messages
verbose_message <- function(message) {
  if (opt$verbose) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(paste0("[INFO ", timestamp, "] ", message, "\n"))
  }
}