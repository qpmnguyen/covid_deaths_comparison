library(tidyverse)
library(RSocrata)
library(lubridate)
library(plotly)

data_loader_ui <- function(id, label = "Loading Data"){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("state"), "Select State", choices = c(state.name, "Washington DC")),
        selectInput(ns("data_type"), "Select Data type", choices = c("Daily", "Total")),
        #checkboxGroupInput("dat", "Select Data Type", choices = data_streams, selected = "Deaths CTP"),
        actionButton(ns("load"), "Load Data")
      ), 
      mainPanel(
        plotlyOutput(ns("main_plot"))
      )
    )
  )
}

data_loader_server <- function(id){
  moduleServer(
    id, 
    function(input, output, session){
      dat <- eventReactive(input$load, {
        types <- c("ctp", "nchs", "cdc")
        df_list <- vector(mode = "list", length = length(types))
        withProgress(message = "Loading data...", {
          for (i in 1:length(types)){
            incProgress(1/length(types), detail = paste("Loading data ", types[i]))
            df_list[[i]] <- load_data(type = types[i])
          }
        })
        df_list %>% reduce(full_join, by = c("abb", "date", "name"))
      })
      # select filter type  
      filt_string <- reactive({
        if(input$data_type == "Total"){
          "tot_deaths"
        } else {
          "new_deaths"
        }
      })
      
      df <- reactive({
        dat() %>% 
          pivot_longer(ends_with(filt_string()), names_to = "data_sets", values_to = "deaths") %>%
          select(date, abb, name, data_sets, deaths) %>% 
          mutate(data_sets = str_remove(data_sets, paste0("_", filt_string()))) %>% 
          mutate(data_sets = str_to_upper(data_sets)) %>% 
          filter(name == input$state) %>% filter(!is.na(deaths))
      })
      output$main_Table <- renderTable({
        head(df())
      })
      output$main_plot <- renderPlotly({
          fig <- ggplot(df(), aes(x = date, y = deaths, col = data_sets)) + 
            geom_line(size = 1.5, alpha = 0.8) + theme_bw() + 
            scale_color_manual(values = c("#0091ea", "#6164ba", "#ffad4a")) + 
            labs(x = "Date", y = "Total Deaths", 
                 title = "Fatality Counts by MMWR week ending date",
                 subtitle = glue("{state}", state = input$state),
                 col = "Data Sets", 
                 caption = "Source: CDC Data Tracker, NCHS Provisional Death Counts, The COVID Tracking Project") + 
            theme_peter()
          fig <- ggplotly(fig)
          fig
      })
    }  
  )
}


# individual functions  

# wrapper function for loading data 
load_data <- function(type = c("nchs", "ctp", "cdc"), cutoff_month = 3){
  type <- match.arg(type)
  states <- data.frame(abb = c(state.abb, "DC"), name = c(state.name, "Washington DC"))
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
    filter(state != "United States") %>% 
    rename("date" = "week_ending_date", "nchs_new_deaths" = "covid_19_deaths", "name" = "state") %>% 
    mutate(nchs_new_deaths = as.numeric(nchs_new_deaths)) %>% 
    group_by(name) %>% mutate(nchs_tot_deaths = cumsum(replace_na(nchs_new_deaths,0))) %>%
    arrange(name,date) %>% 
    filter(month(date) >= cutoff_month & year(date) == 2020) %>% 
    inner_join(states, by = "name")
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
    arrange(name, date) %>% filter(month(date) >= cutoff_month & year(date) == 2020)
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
    arrange(name,date) %>% filter(month(date) >= cutoff_month & year(date) == 2020)
  return(df)
}
# load_data <- function(type = c("nchs", "nyt", "jhu", "ctp")){
#   type <- match.arg(type)
#   if (type == "nyt"){
#     df <- as_tibble(read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"))
#     df <- df %>% mutate(date = as_date(strptime(date, "%Y-%m-%d", tz = "UTC"))) %>% 
#       rename("Deaths NYT" = "deaths") %>% 
#       select(c(date, state, `Deaths NYT`))
#   } else if (type == "jhu"){
#     df <- as_tibble(read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
#     df <- df %>% select(c(Province_State, Admin2, starts_with("X"))) %>% 
#       pivot_longer(starts_with("X"))
#     df$date <- sapply(df$name, process_weird_dates)
#     df <- df %>% mutate(date = as_date(strptime(date, "%m-%d-%y", tz = "UTC"))) %>% 
#       dplyr::group_by(Province_State, date) %>% dplyr::summarise(deaths = sum(value)) %>%
#       rename(c("state" = "Province_State", "Deaths JHU" = "deaths")) %>% select(date, state, `Deaths JHU`)
#   }
#   return(df)
#   
# }
