library(tidyverse)

national_view_ui <- function(id, label = "National View"){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("data_type"), "Select Data type", choices = c("Daily", "Total")),
        sliderInput(ns("lag"),"Lag (in days) for cases in CFR calculations", min = 0, max = 30, value = 0),
        #checkboxGroupInput("dat", "Select Data Type", choices = data_streams, selected = "Deaths CTP"),
        actionButton(ns("load"), "Load Data")
      ), 
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotlyOutput(ns("main_plot"))),
                    tabPanel("Case Fatality Ratio", plotlyOutput(ns("cfr_plot")))
        )
      )
    )
  )
}


national_view_server <- function(id){
  moduleServer(
    id, 
    function(input, output, session){
      dat <- eventReactive(input$load, {
        types <- c("ctp", "nchs", "cdc", "cdc_cases", "nchs_old")
        df_list <- vector(mode = "list", length = length(types))
        withProgress(message = "Loading data...", {
          for (i in 1:length(types)){
            incProgress(1/length(types), detail = paste("Loading data ", types[i]))
            df_list[[i]] <- load_data(type = types[i])
          }
        })
        df_list <- map(df_list, ~{
          .x %>% group_by(date) %>% summarise(across(ends_with("deaths") | ends_with("cases"), sum, na.rm = T))
        })
        
        df_list <- df_list %>% reduce(full_join, by = "date")
        df_list
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
          select(date,data_sets, deaths) %>% 
          mutate(data_sets = str_remove(data_sets, paste0("_", filt_string()))) %>% 
          mutate(data_sets = str_to_upper(data_sets)) %>% 
          filter(!is.na(deaths))
      })
      
      df_cfr <- reactive({
        dat() %>% 
          select(c(date, contains("tot"))) %>%
          mutate(cases = lag(cdc_tot_cases, n = input$lag)) %>% 
          mutate(across(ends_with("tot_deaths"), ~(.x/cases)*100)) %>%
          pivot_longer(ends_with("tot_deaths"), names_to = "data_sets", values_to = "cfr") %>% 
          mutate(data_sets = str_remove(data_sets, "tot_deaths")) %>%
          mutate(data_sets = str_to_upper(data_sets)) %>%
          filter(!is.na(cfr))
      })
      
      output$main_plot <- renderPlotly({
        fig <- ggplot(df(), aes(x = date, y = deaths, col = data_sets)) + 
          geom_line(alpha = 0.8) + theme_bw() + 
          scale_color_d3() + 
          labs(x = "Date", y = "Total Deaths",
               title = "Death counts nationally by data source",
               col = "Data Sets", 
               caption = "Source: CDC Data Tracker, NCHS Provisional Death Counts, The COVID Tracking Project") + 
          theme_peter()
        fig <- ggplotly(fig)
        fig
      })
      output$cfr_plot <- renderPlotly({
        fig <- ggplot(df_cfr(), aes(x = date, y = cfr, col = data_sets)) + 
          geom_line( alpha = 0.8) + theme_bw() + 
          scale_color_d3() + 
          labs(x = "Date", y = "Case Fatality Ratio (%)", 
               title = "Case Fataility Ratio Nationally",
               col = "Data Sets", 
               caption = "Source: CDC Data Tracker, NCHS Provisional Death Counts, The COVID Tracking Project") + 
          theme_peter()
        fig <- ggplotly(fig) 
        fig
      })
    }  
  )
}