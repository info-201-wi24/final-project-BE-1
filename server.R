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
  
  # VIZ 3: Income vs. Measure (Case, Hospitalization, or Death Counts)
  output$income_measure_count <- renderPlotly({
    filtered_df <- df %>%
      filter((Year == input$selectYearViz3 | input$selectYearViz3 == "All"),
             (Income_Level == input$selectIncomeLevelViz3 | input$selectIncomeLevelViz3 == "All"))
    
    measure <- input$selectMeasure
    p <- ggplot(filtered_df, aes_string(x = "Income", y = measure, color = "Income_Level")) +
      geom_point() +
      labs(title = paste('Income vs.', measure), x = 'Income', y = measure)
    
    ggplotly(p)
  })
}
  
