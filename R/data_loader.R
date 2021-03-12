library(tidyverse)
library(RSocrata)
library(lubridate)
library(plotly)
library(gt)
source("R/airtable.R")
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
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotlyOutput(ns("main_plot"))),
                    tabPanel("Annotation", gt_output(ns("annotation")))
        )
        
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
      states <- data.frame(abb = c(state.abb, "DC"), name = c(state.name, "Washington DC"))
      annotations <- retr_airtable() %>%  inner_join(states, by = "abb") %>% 
        select(c(name, metric, annotation, evidence))

      table <- reactive({
        annotations %>% filter(name == input$state) %>% 
          rename(c("State" = "name", "Metric" = "metric", "Annotation" = "annotation", 
                 "Evidence" = "evidence")) %>%
          gt(rowname_col = "Metric", groupname_col = "State") %>%
          cols_align(
            align = "right"
          ) %>% 
          cols_width(
            gt::vars(Evidence) ~ gt::pct(80)
          ) %>% 
          tab_style(
            style = list(
              cell_borders(
                sides = "left", 
                color = "black"
              )
            ),
            locations = list(
              cells_body(
                columns = everything()
              )
            )
          )
          
      }) 
      df <- reactive({
        dat() %>% 
          pivot_longer(ends_with(filt_string()), names_to = "data_sets", values_to = "deaths") %>%
          select(date, abb, name, data_sets, deaths) %>% 
          mutate(data_sets = str_remove(data_sets, paste0("_", filt_string()))) %>% 
          mutate(data_sets = str_to_upper(data_sets)) %>% 
          filter(name == input$state) %>% filter(!is.na(deaths))
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
      output$annotation <- render_gt(table())
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
    filter(date >= "2020-03-01") %>%
    filter(date <= "2021-03-07") %>%
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
    arrange(name, date) %>% filter(month(date) >= cutoff_month & year(date) == 2020) %>%
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
    arrange(name,date) %>% filter(date >= "2020-03-01") %>% 
    filter(date <= "2021-03-07")
  return(df)
}

