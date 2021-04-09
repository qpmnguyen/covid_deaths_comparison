library(tidyverse)
library(lubridate)
library(RSocrata)

# data_streams <- list("CTP Total" = "Deaths CTP", "Johns Hopkins" = "Deaths JHU", 
#                      "CTP Confirmed" = "Deaths (confirmed) CTP", 
#                      "CTP Probable" = "Deaths (probable) CTP", 
#                      "New York Times" = "Deaths NYT", 
#                      "National Center of Health Statistics" = "Deaths NCHS")

data_streams <- list("CTP Total" = "ctp",
                     "CTP Confirmed" = "ctp_conf", 
                     "CTP Probable" = "ctp_prob", 
                     "National Center of Health Statistics" = "nchs",
                     "CDC Data Tracker" = "cdc")

# process weird dates from JHU data 
process_weird_dates <- function(date){
  date <- str_replace(date, "X", "") 
  date <- strsplit(date, ".", fixed = T)[[1]]
  date <- paste(date, collapse = "-")
  return(date)
}

theme_peter <- function(base_size = 11, base_family = "roboto"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        legend.position = "right", 
        plot.caption.position = "plot", 
        legend.justification = "right",
        plot.caption = element_text(hjust = 0, color = "#9ca1a2"))
}


