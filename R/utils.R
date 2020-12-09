library(tidyverse)
library(lubridate)
library(RSocrata)

data_streams <- list("CTP Total" = "Deaths CTP", "Johns Hopkins" = "Deaths JHU", 
                     "CTP Confirmed" = "Deaths (confirmed) CTP", 
                     "CTP Probable" = "Deaths (probable) CTP", 
                     "New York Times" = "Deaths NYT", 
                     "National Center of Health Statistics" = "Deaths NCHS")

# process weird dates from JHU data 
process_weird_dates <- function(date){
  date <- str_replace(date, "X", "") 
  date <- strsplit(date, ".", fixed = T)[[1]]
  date <- paste(date, collapse = "-")
  return(date)
}

#' @title Function to load and wrangle data of different types
#' @param type Either "nchs", "nyt", "jhu" or "ctp"
#' @param state What state to get 
load_data <- function(type = c("nchs", "nyt", "jhu", "ctp")){
  type <- match.arg(type)
  if (type == "nchs"){
    access <- readRDS(file = "store.rds")
    df <- read.socrata(
      "https://data.cdc.gov/resource/r8kw-7aab.json", app_token = access$app_token, 
        password = access$password, email = access$email,
    )
    df$end_week <- as_date(strptime(df$end_week, format = "%Y-%m-%d", tz = "UTC"))
    df <- df %>% select(end_week, state, covid_deaths) %>% filter(state != "United States") %>% 
      rename("date" = "end_week") %>% group_by(state) %>% 
      mutate(deaths = cumsum(replace_na(covid_deaths, 0))) %>% select(-covid_deaths) %>% 
      rename("Deaths NCHS" = "deaths")
  } else if (type == "nyt"){
    df <- as_tibble(read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"))
    df <- df %>% mutate(date = as_date(strptime(date, "%Y-%m-%d", tz = "UTC"))) %>% 
      rename("Deaths NYT" = "deaths") %>% 
      select(c(date, state, `Deaths NYT`))
  } else if (type == "jhu"){
    df <- as_tibble(read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
    df <- df %>% select(c(Province_State, Admin2, starts_with("X"))) %>% 
      pivot_longer(starts_with("X"))
    df$date <- sapply(df$name, process_weird_dates)
    df <- df %>% mutate(date = as_date(strptime(date, "%m-%d-%y", tz = "UTC"))) %>% 
      dplyr::group_by(Province_State, date) %>% dplyr::summarise(deaths = sum(value)) %>%
      rename(c("state" = "Province_State", "Deaths JHU" = "deaths")) %>% select(date, state, `Deaths JHU`)
  } else if (type == "ctp"){
    df <- as_tibble(read.csv("https://api.covidtracking.com/v1/states/daily.csv"))
    df <- df %>% mutate(date = as_date(strptime(date, "%Y%m%d"))) %>% select(date, state, starts_with("death")) %>% 
      select(-deathIncrease) %>% rename(c("Deaths CTP" = "death", "Deaths (confirmed) CTP" = "deathConfirmed", 
                                          "Deaths (probable) CTP" = "deathProbable")) 
    df <- df %>% filter(state %in% state.abb)
    df$state <- sapply(df$state, function(x){
      state.name[which(state.abb == x)]
    })
    df <- df %>% unnest(state)
  }
  return(df)
  
}
