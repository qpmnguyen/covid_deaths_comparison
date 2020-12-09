library(shiny)
library(plotly)

fluidPage(
  titlePanel("Comparing death trajectories across different data sets"),
  h5("by Quang Nguyen"),
  h5("We can compare the trajectories of death counts per state across different data sets. Here we have data from the New York Times, 
     JHU, CTP (total, confirmed, and probable), as well as weekly COVID-19 data from the National Center of Health Statistics (NCHS). \n
     Data from Johns' Hopkins requires a bit more processing, so it takes a while to load. As such, a radio button is set up as an option to load data from JHU. 
     Finally, states and data set choices (depending whether or not they're loaded based on the radio button), can be dynamically altered without having
     to load the data in again."),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State", choices = state.name),
      checkboxGroupInput("dat", "Select Data Type", choices = data_streams, selected = "Deaths CTP"),
      radioButtons("choose_jhu", label = "Try to load JHU data", choices = list("Yes" = "Y", "No" = "N")),
      actionButton("load", "Load Data")
    ), 
    mainPanel(
      plotlyOutput("main_plot")
    )
  )
)