## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Entry point
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     October 20, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Define command line options
option_list <- list(
  optparse::make_option(
    c("--data"),
    action  = "store_true",
    default = FALSE,
    help    = "Perform data loading and wrangling routines"
  ),
  optparse::make_option(
    c("--viz"),
    action  = "store_true",
    default = FALSE,
    help    = "Perform data viz routines"
  ),
  optparse::make_option(
    c("--verbose"),
    action  = "store_false",
    default = TRUE,
    help    = "Print verbose output during execution"
  )
)

# Parse command line options
opt_parser <- optparse::OptionParser(
  option_list     = option_list,
  add_help_option = TRUE,
  description     = "R project for the 2025 Thailand GPP Report"
)
opt <- optparse::parse_args(opt_parser)


# Entry Point Main Function
main <- function(){
  
  renv::activate()
  
  source("R/config.R")
  source("R/outline.R")
  
  if (opt$data){
    verbose_message("Performing data wrangling routine:")
    source("R/data_loading.R")
    verbose_message("Data wrangling routine successful ✅")
  }
  
  if (opt$viz){
    verbose_message("Performing data visualization routine:")
    source("R/data_visualization.R")
    verbose_message("Data visualization routine successful ✅")
  }
  
}


if(!interactive()){
  main()
  quit(save = "no", status = 0)
}