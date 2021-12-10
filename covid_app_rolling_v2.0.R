# See my app live at https://scout-oatman-gaitan.shinyapps.io/COVID_App/

library(plotly)
library(shiny)
library(tidyverse)
library(DT)
library(shinythemes)
library(data.table)
library(zoo)

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
df <- merge(data.table(confirmed), data.table(deaths))
rm(confirmed, deaths)
df <- df[!df$County=="Unassigned",]
df$Date <- as.Date(df$Date, "%m/%d/%y")
df$State <- as.factor(df$State)
df$County <- as.factor(df$County)

df <- df[with(df, order(County, State, Date)),]

df$NewCases <- df$Confirmed - shift(df$Confirmed, n=1, fill=0, type="shift")
df$NewCases[df$NewCases < 0 ] <- 0

df$NewDeaths <- df$Deaths - shift(df$Deaths, n=1, fill=0, type="shift")
df$NewDeaths[df$NewDeaths < 0 ] <- 0

# Add 7 day rolling averages
df <- df %>%
  dplyr::arrange(desc(State, County, Date)) %>% 
  dplyr::group_by(State, County) %>% 
  dplyr::mutate(ConfirmedRolling = zoo::rollmean(Confirmed, k = 7, fill = NA),
                DeathsRolling = zoo::rollmean(Deaths, k = 7, fill = NA),
                NewCasesRolling = zoo::rollmean(NewCases, k = 7, fill = NA),
                NewDeathsRolling = zoo::rollmean(NewDeaths, k = 7, fill = NA)) %>% 
  dplyr::ungroup()

require(RcppRoll)
df$rollNewCase <- roll_mean(df$NewCases, n = 7, align = "right", fill = NA)
######################################################
covid_deaths_2021 <- df %>% 
  filter(Date > "2021-01-01",Date < "2021-12-31") %>% 
  summarise(Deaths = sum(NewDeaths))

covid_flu <- data.frame (titles  = c("2021 COVID Deaths", "Average Yearly Flu Deaths"),
                  death_counts = c(covid_deaths_2021[[1]], 36000))
######################################################


#####################################################################3
library(rjson)
library(plotly)

url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)
url2<- "https://raw.githubusercontent.com/plotly/datasets/master/fips-unemp-16.csv"
url2_df <- read.csv(url2, colClasses=c(fips="character"))
#fig <- plot_ly() 
#fig <- fig %>% add_trace(
#  type="choroplethmapbox",
#  geojson=counties,
#  locations=url2_df$fips,
#  z=url2_df$unemp,
#  colorscale="Viridis",
#  zmin=0,
#  zmax=12,
#  marker=list(line=list(
#    width=0),
#    opacity=0.5
#  )
#)
#fig <- fig %>% layout(
#  mapbox=list(
#    style="carto-positron",
#    zoom =2,
#    center=list(lon= -95.71, lat=37.09))
#)
#fig

df_today <- df %>% filter(Date == Sys.Date()-1)
fips_url <- "https://raw.githubusercontent.com/scoutog/COVID_App/master/county%20to%20fips.csv"
fips_mapping <- read_csv(fips_url)

df_map <- left_join(df_today, fips_mapping, 
                    by = c("County" = "County", "State" = "State"))

#fig <- plot_ly() 
#fig <- fig %>% add_trace(
#  type="choroplethmapbox",
#  geojson=counties,
#  locations=df_map$FIPS,
#  z=df_map$rollNewCase,
#  colorscale="Bluered",
#  autocolorscale=TRUE,
#  zmin=0,
#  zmax=12,
#  marker=list(line=list(
#    width=0),
#    opacity=0.5
#  )
#)
#fig <- fig %>% layout(
#  mapbox=list(
#    style="carto-positron",
#    zoom =2,
#    center=list(lon= -95.71, lat=37.09))
#)
#fig


########################################################################
# Creating UI
ui <- fluidPage(
#  shinythemes::themeSelector(),
  theme=shinytheme("lumen"),
  titlePanel(h2("COVID-19 Confirmed Cases and Deaths in the US by State and County",
                h4("Created by Scout Oatman-Gaitan")),
             windowTitle = "COVID-19 Confirmed Cases and Deaths in the US"),
  
  sidebarLayout(
    sidebarPanel(
      # Inputs
      dateRangeInput("date", "Select a Date Range:", start="2020-01-22",
                     end=max(df$Date)),
      selectInput("state", "Select a State/Province:", unique(df$State), selected="New York"),
      selectInput("county", "Select a County:", choices=NULL, selected=""),
      checkboxInput("yn", "Check here to see all the counties of a state graphed together or uncheck to focus on only the county you select above", value=TRUE),
      br(),
      p("The visualizations are interactive. Zoom, click, and explore to get more information from the graph"),
      br(),
      a("Data Source: CSSE at Johns Hopkins University (updated daily)", 
        href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series"),
      br(), br(), a("Check out my GitHub here", href="https://github.com/scoutog/COVID_App"),
      br(), br(),
      a("Note that this is using a 7 day rolling average for all calculations. To see visualizations with raw numbers, click here",
        href="https://scout-oatman-gaitan.shinyapps.io/COVID_App_Raw/")
    ),
    
    mainPanel(
      # Tabbed outputs
      tabsetPanel(
        tabPanel(em("TOTAL CASES"),
                 plotly::plotlyOutput("confirmed"),
                 br(),br(),
                 plotly::plotlyOutput("confirmed_pie")),
        tabPanel(em("NEW CASES"),
                 plotly::plotlyOutput("newCases"),
                 br(),br(),
                 plotly::plotlyOutput("newCases_pie")),
        tabPanel(em("TOTAL DEATHS"),
                 plotly::plotlyOutput("deaths"),
                 br(), br(),
                 plotly::plotlyOutput("deaths_pie")),
        tabPanel(em("NEW DEATHS"),
                 plotly::plotlyOutput("newDeaths"),
                 br(),br(),
                 plotly::plotlyOutput("newDeaths_pie")),
        tabPanel(em("HEATMAP"),
                 plotly::plotlyOutput("map"),
                 br(),br(),
                 plotly::plotlyOutput("flu"))),
      br(), br()
    )
  )
)


# Server
server <- function(input, output, session) {
  # Update county choice based on selected state
  observeEvent(input$state, {
    df1 <- df %>% filter(State==input$state)
    updateSelectInput(session, "county", choices = unique(df1$County), selected="" )
  })  
  
  # Confirmed plot with if/else for empty county choice
  output$confirmed <- plotly::renderPlotly({
    if (input$yn == TRUE){
      df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
        filter(State == input$state) %>% 
        plot_ly(x = ~Date, y = ~ConfirmedRolling, type = 'scatter', mode = 'lines', color=~County)
    }
    else {
      df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
        filter(State == input$state) %>% filter(County == input$county) %>% 
        plot_ly(x = ~Date, y = ~ConfirmedRolling, type = 'scatter', mode = 'lines', color=~County)
    }
  })
  
  # newCases
  output$newCases <- plotly::renderPlotly({
    if (input$yn == TRUE){
      df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
        filter(State == input$state) %>% 
        plot_ly(x = ~Date, y = ~NewCasesRolling, type = 'scatter', mode = 'lines', color=~County)
    }
    else {
      df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
        filter(State == input$state) %>% filter(County == input$county) %>% 
        plot_ly(x = ~Date, y = ~NewCasesRolling, type = 'scatter', mode = 'lines', color=~County)
    }
  })
  
  # Death count plot with same if/else
  output$deaths <- plotly::renderPlotly({
    if (input$yn == TRUE){
      df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
        filter(State == input$state) %>% 
        plot_ly(x = ~Date, y = ~DeathsRolling, type = 'scatter', mode = 'lines', color=~County)
    }
    else{
      df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
        filter(State == input$state) %>% filter(County == input$county) %>% 
        plot_ly(x = ~Date, y = ~DeathsRolling, type = 'scatter', mode = 'lines', color=~County)
    }
  })
  
  # newDeaths
  output$newDeaths <- plotly::renderPlotly({
    if (input$yn == TRUE){
      df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
        filter(State == input$state) %>% 
        plot_ly(x = ~Date, y = ~NewDeathsRolling, type = 'scatter', mode = 'lines', color=~County)
    }
    else {
      df %>% filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
        filter(State == input$state) %>% filter(County == input$county) %>% 
        plot_ly(x = ~Date, y = ~NewDeathsRolling, type = 'scatter', mode = 'lines', color=~County)
    }
  })
  
  #Confirmed cases pie chart
  output$confirmed_pie <- plotly::renderPlotly({
    df1 <- df %>% filter(Date == max(Date)) %>% 
      filter(State==input$state) %>% group_by(County)
    fig <- plot_ly(df1, labels = ~County, values = ~Confirmed, type = 'pie')
    fig
  })
  
  
    #Flu comparison chart
  output$flu <- plotly::renderPlotly({
    df1 <- df %>% filter(Date == max(Date)) %>% 
      filter(State==input$state) %>% group_by(County)
    fig <- plot_ly(covid_flu, labels = ~titles, values = ~death_counts, type = 'pie')
    fig
  })
  
  #newCases_pie
  output$newCases_pie <- plotly::renderPlotly({
    df1 <- df %>% filter(Date == max(Date)) %>% 
      filter(State==input$state) %>% group_by(County)
    fig <- plot_ly(df1, labels = ~County, values = ~NewCases, type = 'pie')
    fig
  })
  
  #Death cases pie chart
  output$deaths_pie <- plotly::renderPlotly({
    df1 <- df %>% filter(Date == max(Date)) %>% 
      filter(State==input$state) %>% group_by(County)
    fig <- plot_ly(df1, labels = ~County, values = ~Deaths, type = 'pie')
    fig
  })
  
  #newDeaths_pie
  output$newDeaths_pie <- plotly::renderPlotly({
    df1 <- df %>% filter(Date == max(Date)) %>% 
      filter(State==input$state) %>% group_by(County)
    fig <- plot_ly(df1, labels = ~County, values = ~NewDeaths, type = 'pie')
    fig
  })

  # MAP
  output$map <- plotly::renderPlotly({
    fig <- plot_ly() 
    fig <- fig %>% add_trace(
      type="choroplethmapbox",
      geojson=counties,
      locations=df_map$FIPS,
      z=df_map$rollNewCase,
      #  colorscale="Bluered",
      autocolorscale=TRUE,
      zmin=0,
      zmax=40,
      marker=list(line=list(
        width=0),
        opacity=0.5
      )
    )
    fig <- fig %>% layout(
      mapbox=list(
        style="carto-positron",
        zoom =2,
        center=list(lon= -95.71, lat=37.09))
    )
    fig

  })
}

shinyApp(ui = ui, server = server)
