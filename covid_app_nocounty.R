# See my app live at https://scout-oatman-gaitan.shinyapps.io/COVID_App/

library(plotly)
library(shiny)
library(tidyverse)
library(DT)
library(shinythemes)
library(data.table)

# Load in data
confirmed <- fread('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv')
confirmed <- confirmed %>% gather(Date, Confirmed, 12:dim(confirmed)[2])
confirmed <- confirmed[,-c(1:5,8:11)]
names(confirmed)[names(confirmed) == "Admin2"] <- "County"
names(confirmed)[names(confirmed) == "Province_State"] <- "State"

deaths <- fread('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv')
deaths <- deaths %>% gather(Date, Deaths, 13:dim(deaths)[2])
deaths <- deaths[,-c(1:5,8:12)]
names(deaths)[names(deaths) == "Admin2"] <- "County"
names(deaths)[names(deaths) == "Province_State"] <- "State"

# Merge and clean up data
df <- merge(confirmed, deaths)
df <- df[!df$County=="Unassigned",]
df$Date <- as.Date(df$Date, "%m/%d/%y")
df$State <- as.factor(df$State)
df$County <- as.factor(df$County)
# Alphabetical order
df <- df %>% arrange(State)

# Creating UI
ui <- fluidPage(
  #shinythemes::themeSelector(),
  theme=shinytheme("sandstone"),
  titlePanel(h2("COVID-19 Confirmed Cases and Deaths in the US",
                h4("Created by Scout Oatman-Gaitan")),
             windowTitle = "COVID-19 Confirmed Cases and Deaths in the US"),
  
  sidebarLayout(
    sidebarPanel(
      # Inputs
      dateRangeInput("date", "Select a Date Range:", start="2020-01-22",
                     end=(Sys.Date()-1)),
      selectInput("state", "Select a State/Province:", unique(df$State), selected="New York"),
      p("The visualizations are interactive. Zoom, click, and explore to get more information from the graph"),
      br(), p(em("* Click a county in the legend once to remove it and the graph will rescale")),
      p(em("* Double click a county in the legend to focus on that county's data alone")),
      br(),
      a("Data Source: CSSE at Johns Hopkins University (updated daily)", 
        href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series"),
      br(), br(), a("Check out my GitHub here", href="https://github.com/scoutog/COVID_App"),
      br(),br()
    ),
    
    mainPanel(
      # Tabbed outputs
      tabsetPanel(
        tabPanel("Confirmed Count",
                 plotly::plotlyOutput("confirmed"),
                 br(),br(),
                 plotly::plotlyOutput("confirmed_pie")),
        tabPanel("Death Count",
                 plotly::plotlyOutput("deaths"),
                 br(), br(),
                 plotly::plotlyOutput("deaths_pie"))),
      br(), br()
    )
  )
)

# Server
server <- function(input, output, session) {
 
  # Confirmed plot with if/else for empty county choice
  output$confirmed <- plotly::renderPlotly({
  
    df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
        filter(State == input$state) %>% 
        plot_ly(x = ~Date, y = ~Confirmed, type = 'scatter', mode = 'lines', color=~County)
  })
  
  # Death count plot with same if/else
  output$deaths <- plotly::renderPlotly({
      df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
        filter(State == input$state) %>% 
        plot_ly(x = ~Date, y = ~Deaths, type = 'scatter', mode = 'lines', color=~County)

  })
  
  #Confirmed cases pie chart
  output$confirmed_pie <- plotly::renderPlotly({
    df1 <- df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
      filter(State==input$state) %>% group_by(County)
    fig <- plot_ly(df1, labels = ~County, values = ~Confirmed, type = 'pie')
#    fig <- fig %>% layout(title = 'Confirmed Cases by County',
#                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
  })
  
  #Confirmed cases pie chart
  output$deaths_pie <- plotly::renderPlotly({
    df1 <- df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
      filter(State==input$state) %>% group_by(County)
    fig <- plot_ly(df1, labels = ~County, values = ~Deaths, type = 'pie')
    #    fig <- fig %>% layout(title = 'Confirmed Cases by County',
    #                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
  })
}

shinyApp(ui = ui, server = server)
