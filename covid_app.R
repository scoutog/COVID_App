# See my app live at https://scout-oatman-gaitan.shinyapps.io/COVID_App/

library(plotly)
library(shiny)
library(tidyverse)
library(DT)
library(shinythemes)
library(data.table)

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

df <- merge(confirmed, deaths)
df <- df[!df$County=="Unassigned",]
df$Date <- as.Date(df$Date, "%m/%d/%y")

ui <- fluidPage(
  #shinythemes::themeSelector(),
  theme=shinytheme("sandstone"),
  titlePanel("COVID-19 Confirmed Cases and Deaths in the US"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date", "Date Range:", start="2020-01-22",
                     end=(Sys.Date()-1)),
      selectInput("state", "State/Province:", unique(df$State), selected="New York"),
      selectInput("county", "County:", unique(df$County)),
      a("Data Source: CSSE at Johns Hopkins University (updated daily)", 
        href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Confirmed Count",
                 plotly::plotlyOutput("confirmed")),
        tabPanel("Death Count",
                 plotly::plotlyOutput("deaths"))),
      br(), br(),
      p(em("* Click a county in the legend once to remove it and the graph will rescale")),
      p(em("* Double click a county in the legend to focus on that county's data alone")),
      br(), br(),
      a("Check out my GitHub here", href="https://github.com/scoutog/COVID_App")
    )
  )
)
server <- function(input, output, session) {
  output$confirmed <- plotly::renderPlotly({
    if (input$county == ""){
      df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
        filter(State == input$state) %>% 
        plot_ly(x = ~Date, y = ~Confirmed, type = 'scatter', mode = 'lines', color=~County)
    }
    else{
      df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
        filter(State == input$state) %>% filter(County == input$county) %>% 
        plot_ly(x = ~Date, y = ~Confirmed, type = 'scatter', mode = 'lines', color=~County)
    }
    
    #      ggplot(aes(Date, Confirmed, color=State)) +
    #      geom_point()+theme(legend.position = "none")
  })
  output$deaths <- plotly::renderPlotly({
    if (input$county == ""){
      df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
        filter(State == input$state) %>% 
        plot_ly(x = ~Date, y = ~Deaths, type = 'scatter', mode = 'lines', color=~County)
    }
    else{
      df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
        filter(State == input$state) %>% filter(County == input$county) %>% 
        plot_ly(x = ~Date, y = ~Deaths, type = 'scatter', mode = 'lines', color=~County)
    }
    #      ggplot(aes(Date, Deaths, color=State)) +
    #      geom_point()+theme(legend.position = "none")
  })
}
shinyApp(ui = ui, server = server)