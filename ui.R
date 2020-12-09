library(shiny)
library(plotly)

navbarPage("Comparing death trajectories across different data sets",
  tabPanel("Per State View",
    fluidPage(
      titlePanel("Viewing interactive plots per state"),
      h5("We can compare the trajectories of death counts per state across different data sets. Here we have data from the New York Times, 
     JHU, CTP (total, confirmed, and probable), as well as weekly COVID-19 data from the National Center of Health Statistics (NCHS). \n
     Data from Johns' Hopkins requires a bit more processing, so it takes a while to load. As such, a radio button is set up as an option to load data from JHU. 
     Finally, states and data set choices (depending whether or not they're loaded based on the radio button), can be dynamically altered without having
     to load the data in again."),
      sidebarLayout(
        sidebarPanel(
          selectInput("state", "Select State", choices = state.name),
          checkboxGroupInput("dat", "Select Data Type", choices = data_streams, selected = "Deaths CTP"),
          radioButtons("choose_jhu", label = "Try to load JHU data", choices = list("Yes" = "Y", "No" = "N"), selected = "N"),
          actionButton("load", "Load Data")
        ), 
        mainPanel(
          plotlyOutput("main_plot")
        )
      )
    )
  ), 
  tabPanel("Canvas View",
    titlePanel("Canvas view of all states"),
    h5("If there is no graph loading, please load data in the Per State View first before graph can be loaded. Since
       the graph is pretty big, it won't be interactive and the button 'Load Graph' will need to be re-pushed every time
       you want to change the different data sets to be plotted"),
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("dat_2", "Select Data Type", choices = data_streams, selected = "Deaths CTP"),
        actionButton("load_canvas", "Load Graph")
      ),
      mainPanel(
        plotOutput("canvas_plot", height = "2000px")
      )
    )
  )
)
