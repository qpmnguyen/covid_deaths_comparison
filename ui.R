library(shiny)
library(plotly)

navbarPage("Comparing death trajectories across different data sets",
  tabPanel("Per State View",
    fluidPage(
      titlePanel("Viewing interactive plots per state"),
      h5("This is an interactive comparison of fatality counts from the National Center of Heatlh Statistics, The CDC Data Tracker, 
         and The COVID Tracking Project"),
      per_state_view_ui("state_data")
    )
  ), 
  tabPanel("National View", 
    fluidPage(
      titlePanel("Viewing interactive plots nationally"),
      national_view_ui("national_data")
    )
  )
  # tabPanel("Canvas View",
  #   titlePanel("Canvas view of all states"),
  #   h5("If there is no graph loading, please load data in the Per State View first before graph can be loaded. Since
  #      the graph is pretty big, it won't be interactive and the button 'Load Graph' will need to be re-pushed every time
  #      you want to change the different data sets to be plotted"),
  #   sidebarLayout(
  #     sidebarPanel(
  #       checkboxGroupInput("dat_2", "Select Data Type", choices = data_streams, selected = "Deaths CTP"),
  #       actionButton("load_canvas", "Load Graph")
  #     ),
  #     mainPanel(
  #       plotOutput("canvas_plot", height = "2000px")
  #     )
  #   )
  # )
)
