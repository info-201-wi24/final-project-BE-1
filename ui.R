library(ggplot2)
library(plotly)

library(bslib)

# read csv
df <- read.csv("https://github.com/info-201-wi24/final-project-BE-1/raw/main/cleaned_dataframe.csv")


## OVERVIEW TAB INFO

overview_tab <- tabPanel("Project Overview",
   h1("An Overview of Our Project"),
   includeMarkdown("Project Overview.txt"),
   img(src="covid-income.jpeg")
)

## VIZ 1 TAB INFO

# income levels
income_level = c("Low", "Medium", "High")

viz_1_sidebar <- sidebarPanel(
  h2("Line Graph"),
  selectInput(
    inputId = "viz_1_select",
    label = "choose income level",
    choices = income_level,
    selected = "Low"
  )
)

viz_1_main_panel <- mainPanel(
  h2("Total Case Count Over Time for Different Income Levels"),
  plotlyOutput(outputId = "income_plot1")
)


viz_1_tab <- tabPanel("Case Count Over Time",
  sidebarLayout(
    viz_1_sidebar,
    viz_1_main_panel
  )
)

## VIZ 2 TAB INFO

df$Year <- as.numeric(df$Year)
viz_2_sidebar <- sidebarPanel(
  h2("Scatterplot"),
  #TODO: Put inputs for modifying graph here
  selectInput("yearSelect", 
              "Select Year", 
              choices = sort(unique(df$Year), decreasing = TRUE),
              selected = max(df$Year)) 
)

viz_2_main_panel <- mainPanel(
  h2("Visualization of Case Count vs. Household Income"),
  plotlyOutput(outputId = "scatterPlot")
)



viz_2_tab <- tabPanel("Case Count Differences",
                      sidebarLayout(
                        viz_2_sidebar,
                        viz_2_main_panel
                      )
)

ui <- fluidPage(
  titlePanel("Case Count vs. Household Income Visualization"),
  tabsetPanel(viz_2_tab) 
)
## VIZ 3 TAB INFO

hosp_death_by_income <- renderPlotly({
  filtered_df <- df %>% 
    filter(Year == input$selectYearViz2, 
           Race == input$selectRaceViz2)
  
  plot_ly(filtered_df) %>%
    add_trace(x = ~Income_Level, y = ~hosp_count, type = 'bar', name = 'Hospitalizations',
              marker = list(color = 'rgba(50, 171, 96, 0.7)',
                            line = list(color = 'rgba(50, 171, 96, 1.0)', width = 2))) %>%
    add_trace(x = ~Income_Level, y = ~death_count, type = 'bar', name = 'Deaths',
              marker = list(color = 'rgba(219, 64, 82, 0.7)',
                            line = list(color = 'rgba(219, 64, 82, 1.0)', width = 2))) %>%
    layout(title = 'Hospitalization and Death Count by Income Level',
           barmode = 'group',
           xaxis = list(title = 'Income Level'),
           yaxis = list(title = 'Count'))
})
## CONCLUSIONS TAB INFO

conclusion_tab <- tabPanel("Conclusion Tab Title",
                           h1("Disclaimer"),
                           p("To start the conclusion, it’s necessary to acknowledge that the data lacks
 cases that fit into our low income level (less than $20000 yearly). There’s 
 only one case that fulfills the standard. Hence, we only are going to classify 
 the income level into medium and high. 

In addition, there’s a limitation on the data sourcing. We want to acknowledge 
that there might be an issue with the data. The data shows that most cases 
are detected in 2019 while 2020 often is known to be the peak of the number
of COVID cases. We strived to address this, but we lack capital resources to 
do the survey ourselves, and we’ve spent 2 weeks finding data for this project.
The best data we can obtain is the data set we’re using.  
"),
                           h2("First Takeaway"),
                           p("In the first graph, there are years with an increment of half a year from
   2019 to 2021 for the x axis. As for the y-axis, there’s the total case count.
   We make 3 graphs, in addition, for low, medium, and high income levels.
   Hence, the graphs compare the total case count over years for different 
   income levels. From the graphs, we can infer that there’s a similarity 
   between the income levels. For both medium income level ($20000 to $50000)
   and that of high (more than $50000), the drop in cases is steeper throughout
   2019-2020 than throughout 2020-2021. Also, both income levels show a 
   decreasing trend."),
                           h3("Second Takeaway"),
                           p("In the second graph, the x-axis is the yearly household income while the 
   y-axis is the total case count. This’s similar to the first graph, but we 
   switched the x-axis to the income. We also make 3 graphs, which correspond
   to different years. Also, the data point is by region. The first observation 
   is that there seems to be a shape of triangle in all years. We can infer that
   the case count has more spread for lower income than for higher household 
   income with the higher income tend to have less case count.")
)


# overall UI navbar
ui <- navbarPage("INFO 201 Group BE-1",
  overview_tab,
  viz_1_tab,
  viz_2_tab,
  viz_3_tab,
  conclusion_tab
)