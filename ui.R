library(ggplot2)
library(plotly)

library(bslib)
library(markdown)

# read csv
df <- read.csv("https://github.com/info-201-wi24/final-project-BE-1/raw/main/cleaned_dataframe.csv")


## OVERVIEW TAB INFO

overview_tab <- tabPanel("Project Overview",
   h1("An Overview of Our Project"),
   includeMarkdown("Project Overview.txt"),
   img(src="covid-income.jpeg"),
   fluidRow(
     url <- a("Covid Case Data", href="https://kingcounty.gov/en/dept/dph/health-safety/disease-illness/covid-19/data/download"),
   ),
   fluidRow(
     url <- a("Income Data", href="https://datausa.io/profile/geo/king-county-wa#housing", allign = "left")
   )
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
    selected = "High"
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

viz_3_sidebar <- sidebarPanel(
  h2("Options"),
  selectInput("selectYearViz3", "Select Year", choices = c("All", unique(df$Year))),
  selectInput("selectIncomeLevelViz3", "Select Income Level", choices = c("All",unique(df$Income_Level))),
  selectInput("selectMeasure", "Select Measure", choices = c("case_count", "hosp_count", "death_count"))
)

viz_3_main_panel <- mainPanel(
  h2("Income vs. Selected Measure"),
  plotlyOutput(outputId = "income_measure_count", height = "800px") 
)

viz_3_tab <- tabPanel("Income vs. Measure",
                      sidebarLayout(
                        viz_3_sidebar,
                        viz_3_main_panel
                      )
)

## CONCLUSIONS TAB INFO

conclusion_tab <- tabPanel("Conclusion",
                           h2("Limitation of Our Dataset"),
                           p("To start the conclusion, it is necessary to acknowledge that our data set lacks cases that fit into our low-income level (less than $20000 yearly) —— there is only one row that fulfills the standard. Hence, we will mainly look at the medium and high-income level data for analysis. 
In addition, the data visualization shows that most cases are detected in 2019 while 2020 is often known to be the peak of COVID cases number. So we also want to recognize this limitation of our data set that it could be lacking data for 2020 and 2021. We strived to address this, but we lack the capital resources to do the survey ourselves, and the data set we are using is the best data we can obtain.  

"),
                      
                           h2("First Takeaway"),
                           p("In our first visualization, the x-axis is the yearly household income, and the y-axis is the total COVID case count in that selected year. The input that users can select to change is income level (low = < $20000, medium = $20000 ~ $50000, high = > $50000). The three graphs compare the total case count over years for each income level selected. These graphs (medium and high income) show a decreasing trend and both have a significant drop in cases from 2019 to 2020. Another thing to note is the difference in the scales of the y-axis —— total case count over years for high income has a y-axis(case count) range from 350000 to 500000, while the one for medium income ranges from 15000 to 30000. This suggests that the number of COVID cases is much higher in groups classified as high income level. Again, this could be a limitation of our data source. 
This visualization tab could serve as background information for our analysis, as it mainly suggests that total case count is decreasing for all income levels during the year 2019-2021.
"),
                           
                           h2("Second Takeaway"),
                           p("In our second visualization, the x-axis is the yearly household income, and the y-axis is the total COVID case count in that selected year. Each data point represents an individual row of data by region. The overall trend of this visualization shows a right-skewed distribution, meaning that there are more COVID cases in the relatively lower-income end. And this observation applies to all three years (2019-2021). Since lower income is associated with fewer cases and higher income is associated with more cases, an inverse relationship can be found between income levels and negative health outcomes.
"),
                           
                           
                           h2("Third Takeaway"),
                           p("In our third visualization, the x-axis is the yearly income in dollars, and the y-axis is the {case_count, hosp_count, and/or death_count}. This tab integrates our data into one visualization and lets us compare the different measurements of health outcomes. To explain in detail: case_count is the number of cases; hosp_count is the number of people who are hospitalized; death_count is the number of people who die due to COVID, all corresponding to the chosen income level. Each data point represents a region’s data. 
Higher income is associated with lower case count/other measures. This trend is accurately represented in all years (2019, 2020, 2021) and all measurements (death_count, hosp_count, and case_count). The measures (case/hosp/death) count is more spread out for lower income levels and has less correlation with income; they are more concentrated for the high income level and show a clearer negative correlation. It suggests that not only do case count and income have an inverse relationship, but other measures like hospitalization count and death count also have an inverse relationship with income. Worse health outcomes demonstrate that lower-income populations are more negatively affected by the pandemic.
"),
                           
                           
                           
                           
                           
                           h2("Conclusion"),
                           p("The research questions derived from the analysis aim to investigate the correlations between income levels and COVID-19 case rates, hospitalization counts, and death rates, underscoring the need for public health measures that address these socioeconomic inequalities. 
It can be concluded that there appears to be a socioeconomic disparity in the health outcomes wherein the lower income groups suggest a higher number of COVID-19 cases, hospitalizations, and deaths. The data shows a right-skewed income distribution, indicating that more people earn at the relatively lower end of the income spectrum, and these individuals are disproportionately affected by the pandemic. Visual data analysis suggests an inverse relationship between income levels and negative health outcomes, with lower-income individuals experiencing more severe effects of the pandemic.
"), 
                           column(4,
                                  img(src = "https://r2.easyimg.io/x1748r3e8/coronavirus-black-background.png", 
                                      height = "500px", 
                                      width = "500px", 
                                      style = "position: absolute; top: 50px; right: 10px;")
                           )
)





# overall UI navbar
ui <- navbarPage("COVID-19 Data and Income Levels in King County",
  overview_tab,
  viz_1_tab,
  viz_2_tab,
  viz_3_tab,
  conclusion_tab
)
