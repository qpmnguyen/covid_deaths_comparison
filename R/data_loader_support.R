library(tidyverse)
library(glue)
library(RSocrata)
library(lubridate)
library(plotly)
library(gt)


# wrapper function for loading data 
load_data <- function(type = c("nchs", "ctp", "cdc", "cdc_cases", "nchs_old", "nchs_march"), cutoff_month = 3){
  type <- match.arg(type)
  states <- data.frame(abb = c(state.abb, "DC"), name = c(state.name, "District of Columbia"))
  df <- do.call(type, args = list(states = states, cutoff_month=cutoff_month), envir = environment())
  return(df)
}

# function for loading nchs data  
nchs <- function(states, cutoff_month){
  access <- readRDS(file = "store.rds")
  df <- read.socrata(
    "https://data.cdc.gov/resource/r8kw-7aab.json", app_token = access$app_token, 
    password = access$password, email = access$email,
  )
  df <- df %>% mutate(week_ending_date = as_date(week_ending_date)) %>% 
    select(week_ending_date, state, covid_19_deaths) %>% 
    rename("date" = "week_ending_date", "nchs_new_deaths" = "covid_19_deaths", "name" = "state") %>% 
    mutate(nchs_new_deaths = as.numeric(nchs_new_deaths)) %>% 
    group_by(name) %>% mutate(nchs_tot_deaths = cumsum(replace_na(nchs_new_deaths,0))) %>%
    arrange(name,date) %>% 
    filter(date >= "2020-02-01") %>%
    filter(date <= "2021-03-07") %>%
    inner_join(states, by = "name") %>% ungroup()
  return(df)
}

nchs_march <- function(states, cutoff_month){
  df <- read_csv("data/nchs_as_of_2021_03_17.csv")
  df <- df %>% rename("date" = "Week Ending Date", "data_as_of" = "Data as of", 
                "name" = "State", "nchs_new_deaths" = "COVID-19 Deaths") %>%
    select(data_as_of, date, name , nchs_new_deaths) %>% filter(name != "United States") %>%
    mutate(data_as_of = as_date(data_as_of, format = "%m/%d/%Y"), 
           date = as_date(date, format = "%m/%d/%Y"))  %>% 
    group_by(name) %>% 
    mutate(nchs_tot_deaths = cumsum(replace_na(nchs_new_deaths,0))) %>% 
    arrange(name, date) %>% 
    filter(date >= "2020-02-01") %>%
    filter(date <= "2021-03-07") %>% 
    inner_join(states, by = "name") %>% ungroup()
  return(df)
}

nchs_march_nat <- function(states, cutoff_month){
  df <- read_csv(file = "data/nchs_as_of_2021_03_17.csv")
  df <- df %>% rename("date" = "Week Ending Date", "data_as_of" = "Data as of", 
                      "name" = "State", "nchs_new_deaths" = "COVID-19 Deaths") %>%
    select(data_as_of, date, name , nchs_new_deaths) %>% 
    filter(name == "United States") %>%
    mutate(data_as_of = as_date(data_as_of, format = "%m/%d/%Y"), 
           date = as_date(date, format = "%m/%d/%Y"))  %>% 
    group_by(name) %>% 
    mutate(nchs_tot_deaths = cumsum(replace_na(nchs_new_deaths,0))) %>% 
    arrange(name, date) %>% 
    filter(date >= "2020-02-01") %>%
    filter(date <= "2021-03-07") %>% ungroup()
  return(df)
}


# function for loading cdc data  
cdc <- function(states, cutoff_month){
  access <- readRDS(file = "store.rds")
  df <- read.socrata(
    "https://data.cdc.gov/resource/9mfq-cb36.json", app_token = access$app_token, 
    password = access$password, email = access$email
  )
  df <- df %>% as_tibble() %>% rename(c("date" = "submission_date", "abb" = "state")) %>% 
    select(date, abb, tot_death, new_death) %>% inner_join(states, by = "abb") %>% 
    mutate(tot_death = as.numeric(tot_death), new_death = as.numeric(new_death), 
           date = as_date(date)) %>% 
    rename("cdc_new_deaths" = "new_death", "cdc_tot_deaths" = "tot_death") %>% 
    arrange(name, date) %>% 
    filter(date >= "2020-02-01") %>%
    filter(date <= "2021-03-07")
  return(df)
}

# function for loading ctp data  
ctp <- function(states, cutoff_month){
  df <- as_tibble(read.csv("https://api.covidtracking.com/v1/states/daily.csv"))
  df <- df %>% mutate(date = as_date(strptime(date, "%Y%m%d"))) %>% 
    select(date, state, starts_with("death")) %>% 
    rename(c("ctp_tot_deaths" = "death", "ctp_tot_confirmed" = "deathConfirmed", 
             "ctp_tot_probable" = "deathProbable", "abb" = "state", "ctp_new_deaths" = "deathIncrease")) %>%
    inner_join(states, by = "abb") %>% 
    arrange(name,date) %>% 
    filter(date >= "2020-02-01") %>% 
    filter(date <= "2021-03-07")
  return(df)
}

cdc_cases <- function(states, cutoff_month){
  access <- readRDS(file = "store.rds")
  df <- read.socrata(
    "https://data.cdc.gov/resource/9mfq-cb36.json", app_token = access$app_token, 
    password = access$password, email = access$email
  )
  df <- df %>% as_tibble() %>% rename(c("date" = "submission_date", "abb" = "state")) %>%
    select(date, abb, tot_cases, new_case) %>% inner_join(states, by = "abb") %>%
    mutate(tot_cases = as.numeric(tot_cases), 
           new_case = as.numeric(new_case), date = as_date(date)) %>%
    rename("cdc_new_cases" = "new_case", "cdc_tot_cases" = "tot_cases") %>%
    arrange(name,date) %>% 
    filter(date >= "2020-02-01") %>% 
    filter(date <= "2021-03-07")
  return(df)
}

nchs_old <- function(states, cutoff_month){
  dates <- c("2020_10_02", "2020_10_30", "2020_12_28")
  # load data set 
  data_list <- map(dates, ~{
    read_csv(file = glue("data/nchs_as_of_{date}.csv", date = .x))
  })
  # load display this as a list 
  data_list <- map(data_list, ~{
    .x %>% rename("data_as_of" = "Data as of", "date" = "End Week", 
                  "nchs_new_deaths" = "COVID-19 Deaths", 
                  "name" = "State") %>% 
      select(date, data_as_of, name, nchs_new_deaths) %>% 
      mutate(date = as_date(date, format = "%m/%d/%Y"), 
             data_as_of = as_date(data_as_of, format = "%m/%d/%Y")) %>%
      group_by(name) %>% 
      mutate(nchs_tot_deaths = cumsum(replace_na(nchs_new_deaths,0)))
  })

  final_df <- reduce(data_list, full_join) %>% inner_join(states, by = "name") %>% ungroup()
  
  
  
  #data_list <- map2(data_list, dates, ~{
  #  date_names <- glue("nchs_{date}", date = .y)
  #  .x %>% rename("data_as_of" = "Data as of", "date" = "End Week", 
  #                !!date_names := "COVID-19 Deaths", "name" = "State") %>% 
  #    select(date, name, !!date_names) %>% 
  #    mutate(date = as_date(date, format = "%m/%d/%Y")) %>% 
  #    group_by(name) %>% 
  #    mutate(across(starts_with("nchs"), ~cumsum(.x), .names = "{.col}_tot_deaths")) %>% 
  #    rename_with(~glue("{colnames}_new_deaths", colnames = .x), .cols = !ends_with("tot_deaths") & starts_with("nchs")) 
  #})
  # combine everything 
  #final_df <- reduce(data_list, full_join, by = c("date", "name")) %>% 
  #  inner_join(states, by = c("name"))
  return(final_df)
  
  
}


# national data for cdc
national_ctp <- function(cutoff_month, ...){
  df <- read.csv(file = "https://api.covidtracking.com/v1/us/daily.csv")
  df <- df %>% mutate(date = strptime(date, format = "%Y%m%d", tz = "EST")) %>% 
    select(date, death, deathIncrease) %>% 
    rename("ctp_tot_deaths" = "death", "ctp_new_deaths" = "deathIncrease") 
  return(df)
}




