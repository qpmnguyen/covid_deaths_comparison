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

retreive_annotation <- function(view = "Deaths Sources"){
  store <- readRDS(file = "store.rds")
  r <- GET("https://api.airtable.com/v0/app2tJkNU6YXNrDwc/Definitions", 
           query = list(view = view), 
           add_headers(Authorization = glue("Bearer {api_key}", api_key = store$airtable_key)))
  warn_for_status(r)
  cont <- content(r)
  df_list <- vector(mode = "list", length = length(cont$records))
  for (i in seq_along(cont$records)){
    df_list[[i]] <- tibble(id = cont$records[[i]]$id, 
           name = cont$records[[i]]$fields$Name, 
           definitions = cont$records[[i]]$fields$Definition)
  }
  df <- reduce(df_list, bind_rows)
  return(df)
}


retr_airtable <- function(){
  # hard coding this becase we don't have that many records (114)
  init_r <- get_airtable()
  rem_r <- get_airtable(offset = init_r$offset)
  requests <- c(init_r$records, rem_r$records)
  data <- do.call(rbind, requests) %>% as_tibble()
  key <- retreive_annotation()
  data <- data %>% unnest(c(id, createdTime)) %>% select(-id) %>%
    mutate(fields = purrr::map(fields, process_fields, key = key)) %>% unnest(fields) %>% 
    filter(!is.na(abb))
  return(data)
}

process_fields <- function(list, key){
  idx <- map_dbl(list$Annotation, ~which(key$id == .x))
  names <- key$name[idx]
  is_revise <- map_lgl(names, ~str_detect(.x, "revising"))
  if (sum(is_revise) == 0){
    revise <- FALSE
  } else {
    revise <- TRUE
  }
  names <- names[!is_revise]
  if (length(names) == 0){
    names <- "None"
  }
  results <- tibble(
    abb = process_entries(list, "abb"),
    metric = process_entries(list, "metric"),
    annotation = paste(names, collapse = ", "),
    #annotation = list$`Annotation Summary` %>% convert_null(),
    evidence = process_entries(list, "evidence"),
    source = process_entries(list, "source"),
    revise = revise, 
    annotation_details = list(names)
  )
  return(results)
}

# master function to process entries 
process_entries <- function(entry, type){
  if (type == "abb"){
    if (length(entry$State) == 0){
      output <- NA
    } else {
      output <- entry$State[[1]]
    }
  } else if (type == "metric"){
    output <- entry$`States Daily Column`[[1]]
  } else if (type == "evidence"){
    output <- entry$Evidence
  } else if (type == "source"){
    output <- entry$`Evidence Source Cleaned`
  }
  # If null return as None
  if (is.null(output)){
    return("None")
  } else {
    return(output)
  }
}


convert_null <- function(output){
  if (is.null(output)){
    return("None")
  } else {
    return(output)
  }
}

