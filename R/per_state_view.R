library(tidyverse)
library(RSocrata)
library(lubridate)
library(plotly)
library(gt)
library(ggsci)
source("R/airtable.R")
source("R/data_loader_support.R")

per_state_view_ui <- function(id, label = "State Level View"){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("state"), "Select State", choices = c(state.name, "Washington DC")),
        selectInput(ns("data_type"), "Select Data type", choices = c("Daily", "Total")),
        sliderInput(ns("lag"),"Lag (in days) for cases in CFR calculations", min = 0, max = 30, value = 0),
        #checkboxGroupInput("dat", "Select Data Type", choices = data_streams, selected = "Deaths CTP"),
        actionButton(ns("load"), "Load Data")
      ), 
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotlyOutput(ns("main_plot"))),
                    tabPanel("Annotation", gt_output(ns("annotation"))),
                    tabPanel("Case Fatality Ratio", plotlyOutput(ns("cfr_plot")))
        )
        
      )
    )
  )
}

per_state_view_server <- function(id){
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
        df_list <- df_list %>% reduce(full_join, by = c("abb", "date", "name"))
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
      states <- data.frame(abb = c(state.abb, "DC"), name = c(state.name, "Washington DC"))
      annotations <- retr_airtable() %>%  inner_join(states, by = "abb") %>% 
        select(c(name, metric, annotation, evidence, annotation_details, revise))
      
      # Generating annotation table from airtable 
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
      
      plot_annotation <- reactive({
        annotations %>% filter(name == input$state, metric == "Deaths (confirmed and probable)")
      })
      
      
      df <- reactive({
        dat() %>% 
          pivot_longer(ends_with(filt_string()), names_to = "data_sets", values_to = "deaths") %>%
          select(date, abb, name, data_sets, deaths) %>% 
          mutate(data_sets = str_remove(data_sets, paste0("_", filt_string()))) %>% 
          mutate(data_sets = str_to_upper(data_sets)) %>% 
          filter(name == input$state) %>% filter(!is.na(deaths))
      })

      df_cfr <- reactive({
        dat() %>% filter(name == input$state) %>% 
          select(c(date, abb, name, contains("tot"))) %>%
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
                 title = "ABCD",
                 subtile = "DEFG",
                 col = "Data Sets", 
                 caption = "Source: CDC Data Tracker, NCHS Provisional Death Counts, The COVID Tracking Project") + 
            theme_peter()
          fig <- ggplotly(fig) %>% layout(
            title = list(
              text = paste0("Fatality Counts by MMWR week ending date",
                            "<br>", "<sup>",
                            glue("Death Sources: {annotation}; Revise: {revise}", 
                                 annotation = plot_annotation() %>% pull(annotation), 
                                 revise = plot_annotation() %>% pull(revise)), 
                            "</sup>")
            )
          )
          fig
      })
      output$annotation <- render_gt(table())
      output$cfr_plot <- renderPlotly({
        fig <- ggplot(df_cfr(), aes(x = date, y = cfr, col = data_sets)) + 
          geom_line(alpha = 0.8) + theme_bw() + 
          scale_color_d3() + 
          labs(x = "Date", y = "Case Fatality Ratio (%)", 
               title = "ABCD",
               subtitle = "Placeholder",
               col = "Data Sets", 
               caption = "Source: CDC Data Tracker, NCHS Provisional Death Counts, The COVID Tracking Project") + 
          theme_peter()
        fig <- ggplotly(fig) %>% layout(
          title = list(
            text = paste0("Case Fatality Ratio (%) by MMWR week ending date",
                          "<br>", "<sup>",
                          glue("Death Sources: {annotation}; Revise: {revise}", 
                              annotation = plot_annotation() %>% pull(annotation), 
                              revise = plot_annotation() %>% pull(revise)), 
                          "</sup>")
          )
        )
        fig
      })
    }  
  )
}


