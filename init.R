my_packages <- c("sf","leaflet", "shiny", "tibble", "ggplot2", "dplyr", "tidyverse", "rlist", "lubridate", "plotly", "waiter")
 install_if_missing <- function(p) {
 if(p %in% rownames(installed.packages())==FALSE){
 install.packages(p)}
 }
invisible(sapply(my_packages, install_if_missing))