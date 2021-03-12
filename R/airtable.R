library(tidyverse)
library(httr)
library(glue)
library(rlist)


get_airtable <- function(offset = NULL){
  store <- readRDS(file = "store.rds")
  if (!is.null(offset)){
    query_list <- list(view = "DCs: Deaths Sources", offset = offset)
  } else {
    query_list <- list(view = "DCs: Deaths Sources")
  }
  r <- GET("https://api.airtable.com/v0/app2tJkNU6YXNrDwc/Annotations", 
           query = query_list,
           add_headers(Authorization = glue("Bearer {api_key}", api_key = store$airtable_key)))
  warn_for_status(r)
  cont <- content(r)
  return(cont)
}

retr_airtable <- function(){
  # hard coding this becase we don't have that many records (114)
  init_r <- get_airtable()
  rem_r <- get_airtable(offset = init_r$offset)
  requests <- c(init_r$records, rem_r$records)
  data <- do.call(rbind, requests) %>% as_tibble()
  data <- data %>% unnest(c(id, createdTime)) %>% select(-id) %>%
    mutate(fields = purrr::map(fields, process_fields)) %>% unnest(fields)
  return(data)
}

process_fields <- function(list){
  results <- tibble(
    abb = list$State[[1]] %>% convert_null(),
    metric = list$`States Daily Column`[[1]] %>% convert_null(),
    annotation = list$`Annotation Summary` %>% convert_null(),
    evidence = list$Evidence %>% convert_null(),
    source = list$`Evidence Source Cleaned` %>% convert_null()
  )
  return(results)
}

convert_null <- function(entry){
  if (is.null(entry)){
    return("None")
  } else {
    return(entry)
  }
}


