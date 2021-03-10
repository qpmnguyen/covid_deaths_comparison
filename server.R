library(plotly)
library(tidyverse)
library(ggsci)
library(glue)
#dir.create("~/.fonts")
#file.copy("www/SF-Pro-Text-Regular.otf", "~/.fonts")
#system("fc-cache -f ~/.fonts")


function(input, output, session){
  data_loader_server("state_data")
  # data <- eventReactive(input$load,{
  #   types <- c("nyt", "ctp", "nchs")
  #   dfs <- vector(mode = "list", length = length(types))
  #   withProgress(message = "Loading data...", {
  #     for (i in 1:length(types)){
  #       incProgress(1/length(types), detail = paste("Loading data ", types[i]))
  #       dfs[[i]] <- load_data(type = types[i])
  #     }
  #   })
  #   dfs <- dfs %>% reduce(left_join, by = c("date", "state")) 
  #   dfs <- dfs %>% pivot_longer(starts_with("Death"), names_to = "data_type")
  #   dfs
  # })
  # 
  # output$main_plot <- renderPlotly({
  #   df <- data() %>% filter(data_type %in% input$dat, state == input$state)
  #   fig <- ggplot(df, aes(x = date, y = value, color = data_type)) + 
  #     geom_line() + theme_bw() + geom_point() + scale_color_nejm() +  
  #     labs(x = "Date", y = "Cumulative Deaths", color = "Data Types", 
  #          title = "Cumulative deaths by date and data source")
  #   fig <- ggplotly(fig)
  #   fig
  # })
  # 
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

