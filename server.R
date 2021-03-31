library(plotly)
library(tidyverse)
library(ggsci)
library(glue)
#dir.create("~/.fonts")
#file.copy("www/SF-Pro-Text.ttf", "~/.fonts")
#system("fc-cache -f ~/.fonts")


function(input, output, session){
  per_state_view_server("state_data")
  national_view_server("national_data")
  
  # canvas_plt <- eventReactive(input$load_canvas, {
  #   df <- data() %>% filter(data_type %in% input$dat_2) %>% filter(state %in% state.name)
  #   fig <- ggplot(df, aes(x = date, y = value, color = data_type)) + 
  #     geom_line() + theme_bw() + geom_point() + scale_color_nejm() +  
  #     labs(x = "Date", y = "Cumulative Deaths", color = "Data Types", 
  #          title = "Cumulative deaths by date and data source") + 
  #     facet_wrap(~state, scales = "free_y", ncol = 5)
  #   fig
  # })
  # 
  # output$canvas_plot <- renderPlot({
  #   canvas_plt()
  # })
}

