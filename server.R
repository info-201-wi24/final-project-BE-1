library(ggplot2)
library(plotly)
library(dplyr)

# read csv
df <- read.csv("https://github.com/info-201-wi24/final-project-BE-1/raw/main/cleaned_dataframe.csv")
  
server <- function(input, output){
  
  output$income_plot1 <- renderPlotly({
    
    # filtered df by income levels low, medium, high
    filtered_income_levels_df <- df %>% filter(Income_Level %in% input$viz_1_select)
    # group by years
    year_total_case_count <- filtered_income_levels_df %>%
      group_by(Year) %>%
      summarize(total_case_count = sum(case_count))
    # line plot
    my_plot <- ggplot(year_total_case_count) +
      geom_line(mapping = aes(
        x = Year,
        y = total_case_count
        
      )) + labs(y = "Total Case Count") # simpler name for y axis

    return(ggplotly(my_plot))
  })
  
  output$scatterPlot <- renderPlotly({
    # Filter data based on the selected year
    print(class(df))
    filteredData <- df %>% 
      dplyr::filter(Year == as.numeric(input$yearSelect))
    # Generate the scatterplot
    plot_ly(filteredData, 
            x = ~Income, 
            y = ~case_count, 
            type = "scatter", 
            mode = "markers",
            marker = list(size = 8, 
                          color = ~Income, 
                          colorscale = "Rocket"),
            text = ~paste("Geo ID:", geo_id, 
                          "<br>Income Level:", Income_Level)) %>%
      layout(title = "Total Case Count vs. Household Income",
             xaxis = list(title = "Household Income ($)"),
             yaxis = list(title = "Total Case Count"),
             hovermode = "closest")
  })
  
  output$income_hosp_count <- renderPlotly({
    # Adjusting for "All" option in Year and Income Level
    filtered_df <- df %>%
      filter((Year == input$selectYearViz1 | input$selectYearViz1 == "All"),
             (Income_Level == input$selectIncomeLevelViz1 | input$selectIncomeLevelViz1 == "All"))
    
    # When "All" is selected for Income Level, plot each level in different colors
    if (input$selectIncomeLevelViz1 == "All") {
      p <- plot_ly()
      income_levels <- unique(df$Income_Level)
      
      # Loop through each income level and add to the plot
      for (level in income_levels) {
        level_df <- filtered_df %>% filter(Income_Level == level)
        p <- add_trace(p, data = level_df, x = ~Income, y = ~hosp_count, type = 'scatter', mode = 'markers',
                       marker = list(size = 10),
                       name = level, # Name used in legend
                       hoverinfo = 'text',
                       text = ~paste('Income:', Income, '<br>Hospitalization Count:', hosp_count))
      }
      
      p <- layout(p, title = 'Income vs. Hospitalization Counts',
                  xaxis = list(title = 'Income'),
                  yaxis = list(title = 'Hospitalization Count'),
                  showlegend = TRUE)
    } else {
      # For specific income level selection
      p <- plot_ly(data = filtered_df, x = ~Income, y = ~hosp_count, type = 'scatter', mode = 'markers',
                   marker = list(size = 10),
                   hoverinfo = 'text',
                   text = ~paste('Income:', Income, '<br>Hospitalization Count:', hosp_count)) %>%
        layout(title = 'Income vs. Hospitalization Counts',
               xaxis = list(title = 'Income'),
               yaxis = list(title = 'Hospitalization Count'))
    }
    
    p # Return the plot
  })  
  
}