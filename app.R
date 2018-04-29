#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(knitr)
library(readstata13)
library(scales)
library(readr)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Map of Major League Baseball teams and Total Championships"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
       column(2, checkboxGroupInput(inputId = 'MLB Divisions',
                                    label = 'Please select 1 or more MLB Divisions',
                                    choices = c('AL East' = 'AL East',
                                                'AL Central' = 'AL Central',
                                                'AL West' = 'AL West',
                                                'NL East' = 'NL East',
                                                'NL Central' = 'NL Central',
                                                'NL West' = 'NL West'),
                                    selected = c('AL East','AL Central','AL West','NL East','NL Central','NL West'))),
       column(1,submitButton(text = "Submit")),
       column(6,leafletOutput(outputId = 'mlbMap'))
       
   ),
   fluidRow(
       column(3,plotOutput(outputId = 'empty')),
       column(6,plotOutput(outputId = 'championships'))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
   output$mlbMap <- renderLeaflet({
       
       #setwd(paste0(personal,'Coursera/Developing Data Products/Week 4/'))
      
       mlbDivision <- function(x){
           
           if(x %in% c('Baltimore Orioles','Boston Red Sox','New York Yankees','Tampa Bay Rays','Toronto Blue Jays')){
               y <- 'AL East'
           } else if(x %in% c('Minnesota Twins','Cleveland Indians','Chicago White Sox','Detroit Tigers','Kansas City Royals')){
               y <- 'AL Central'
           } else if(x %in% c('Los Angeles Angels','Houston Astros','Seattle Mariners','Oakland Athletics','Texas Rangers')){
               y <- 'AL West'
           } else if(x %in% c('New York Mets','Philadelphia Phillies','Atlanta Braves','Washington Nationals','Miami Marlins')){
               y <- 'NL East'
           } else if(x %in% c('Pittsburgh Pirates','St. Louis Cardinals','Atlanta Braves','Chicago Cubs','Milwaukee Brewers','Cincinnati Reds')){
               y <- 'NL Central'
           } else {
               y <- 'NL West'
           }
           
           return(y)
           
       }
       
       # Determine the color of the icon
       mlbColor <- function(x){
           
           if(x == 'AL East'){
               y <- 'blue'
           } else if(x == 'AL Central'){
               y <- 'green'
           } else if(x == 'AL West'){
               y <- 'yellow'
           } else if(x == 'NL East'){
               y <- 'red'
           } else if(x == 'NL Central'){
               y <- 'purple'
           } else {
               y <- 'black'
           }
           
           return(y)
           
       }
       
       
       ## Baseball stadium data
       mlb <- read_csv('baseball data.csv') %>%
           mutate(Longitude = -Longitude,
                  Division = factor(sapply(X = Team, FUN = mlbDivision),levels = c('AL East','AL Central','AL West','NL East','NL Central','NL West')),
                  DivisionColor = sapply(X = Division, FUN = mlbColor),
                  teamLabs = paste0('Team: ',Team,'\nField: ',Name,'\nDistance to Center Field: ',`Distance to center field`)) %>%
           filter(Division %in% input$`MLB Divisions`)
       
       icons <- makeAwesomeIcon(
           icon = 'circle',
           iconColor = 'black',
           library = 'glyphicon',
           markerColor = ifelse(mlb$Team %in% c('Baltimore Orioles','Boston Red Sox','New York Yankees','Tampa Bay Rays','Toronto Blue Jays'),
                                'red',
                         ifelse(mlb$Team %in% c('Minnesota Twins','Cleveland Indians','Chicago White Sox','Detroit Tigers','Kansas City Royals'),
                                'blue',
                         ifelse(mlb$Team %in% c('Los Angeles Angels','Houston Astros','Seattle Mariners','Oakland Athletics','Texas Rangers'),
                                'orange',
                         ifelse(mlb$Team %in% c('New York Mets','Philadelphia Phillies','Atlanta Braves','Washington Nationals','Miami Marlins'),
                                'green',
                         ifelse(mlb$Team %in% c('Pittsburgh Pirates','St. Louis Cardinals','Atlanta Braves','Chicago Cubs','Milwaukee Brewers','Cincinnati Reds'),
                                'darkpurple','black')))))
           
       )
       
       makeColorsandNames <- data.frame(divisions = c('AL East','AL Central','AL West','NL East','NL Central','NL West'),
                                        division.cent = c('#CC0000','#3399FF','#FFA500','#9ACD32','#483D8B','#000000'))
       
       
       
       leaflet(data = mlb) %>%
           setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>% 
           addTiles() %>% 
           addAwesomeMarkers(~Longitude, ~Latitude, icon = icons, label = ~as.character(paste0(Team,", Championships: ",Championships))) %>%
           addLegend(position = 'bottomleft', colors = makeColorsandNames[,2],labels = makeColorsandNames[,1],opacity = 1,title = 'Divisions')
       
       
       
   })
   
   output$championships <- renderPlot({
       
       #setwd(paste0(personal,'Coursera/Developing Data Products/Week 4/'))
       
       mlbDivision <- function(x){
           
           if(x %in% c('Baltimore Orioles','Boston Red Sox','New York Yankees','Tampa Bay Rays','Toronto Blue Jays')){
               y <- 'AL East'
           } else if(x %in% c('Minnesota Twins','Cleveland Indians','Chicago White Sox','Detroit Tigers','Kansas City Royals')){
               y <- 'AL Central'
           } else if(x %in% c('Los Angeles Angels','Houston Astros','Seattle Mariners','Oakland Athletics','Texas Rangers')){
               y <- 'AL West'
           } else if(x %in% c('New York Mets','Philadelphia Phillies','Atlanta Braves','Washington Nationals','Miami Marlins')){
               y <- 'NL East'
           } else if(x %in% c('Pittsburgh Pirates','St. Louis Cardinals','Atlanta Braves','Chicago Cubs','Milwaukee Brewers','Cincinnati Reds')){
               y <- 'NL Central'
           } else {
               y <- 'NL West'
           }
           
           return(y)
           
       }
       
       # Determine the color of the icon
       mlbColor <- function(x){
           
           if(x == 'AL East'){
               y <- 'blue'
           } else if(x == 'AL Central'){
               y <- 'green'
           } else if(x == 'AL West'){
               y <- 'yellow'
           } else if(x == 'NL East'){
               y <- 'red'
           } else if(x == 'NL Central'){
               y <- 'purple'
           } else {
               y <- 'black'
           }
           
           return(y)
           
       }
       
       mlb <- read_csv('baseball data.csv') %>%
               mutate(Longitude = -Longitude,
                      Division = factor(sapply(X = Team, FUN = mlbDivision),levels = c('AL East','AL Central','AL West','NL East','NL Central','NL West')), 
                      DivisionColor = sapply(X = Division, FUN = mlbColor),
                      teamLabs = paste0('Team: ',Team,'\nField: ',Name,'\nDistance to Center Field: ',`Distance to center field`)) %>%
               filter(Division %in% input$`MLB Divisions`) %>%
               group_by(Division) %>%
               summarise(Championships = sum(Championships))
       
       barColors <- data.frame(division = c('AL East','AL Central','AL West','NL East','NL Centrl','NL West'),
                               divColor = c('#CC0000','#3399FF','#FFA500','#9ACD32','#483D8B','#000000')) 
           
       a <- ggplot(data = mlb, aes(x = Division, y = Championships))
       a + geom_bar(stat = 'identity') +
           annotate("text",x = Inf, y = Inf, hjust = 1, vjust = 1, 
                    label=paste0("Total Championships: ", prettyNum(sum(mlb$Championships), big.make = ",",scientific = FALSE)), size = 10) +
           theme(legend.position = 'none')
       
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

