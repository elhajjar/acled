                                 #######  I. Loading libraries ######


library(shiny) 
library(shinydashboard)
library(tidyverse)
library(ggplot2) #for the map
library(plotly)
library(viridis) #for the map



                                #######  II. Reading the data ######

           
# Cleaned data is already stored as data.csv in the same folder. Should you like to see the cleaning
# process, you can refer to R&C Data_ACLED.R file.
# setwd("C:/Users/Omar El-Hajjar/Desktop/ACCLED/")
data<-read.csv("data_updated.csv",header=T)



                                  #######  III. Creating the dashboard ######



dashboardPage(
  skin='black',
  dashboardHeader(title  =  "ACLED"),
  dashboardSidebar( 
    sidebarMenu(
      valueBox("","1. Please select the view below.",color="blue",width="87%"),
      menuItem("GRAPHIC VIEW", tabName = "dashboard", icon = icon("chart-bar")), #takes you to the graphic menu
      menuItem("MAP VIEW", tabName = "dashboard2", icon = icon("map-marked-alt")), #takes you to the map menu
      valueBox("","2. Please select the filters below.",color="blue",width="87%"),
      sliderInput("years_range",                                                  #Slider input for years
                  "Range of years:",
                  min <- min(data$year),
                  max <- max(data$year),
                  value <- c(min(data$year),max(data$year)),
                  step = 1
      ),
      
      menuItem("EVENT TYPE", tabName = "eventTypeTab",icon=icon("bullhorn"),      #Event Type filter
               
               checkboxInput(inputId="eventTypeAllCheckBox",                      #Select All/None check box
                             label = "Select All/None",
                             value = TRUE),
               
               checkboxGroupInput(                                                #Event Type check boxes
                 inputId = "eventTypeCheckBoxes",label = "Event Type",
                 choiceNames = levels(as.factor(data$event_type)),
                 choiceValues = levels(as.factor(data$event_type)))
      ),
      
      menuItem("EVENT REGION", tabName = "regionTab", icon = icon("globe-africa"),   #Event Region filter
               
               checkboxInput(inputId="regionAllCheckBox",                            #Select All/None check box
                             label = "Select All/None",
                             value = TRUE),
               
               checkboxGroupInput(                                                  #Region check boxes
                 inputId = "regionCheckBoxes",label = "Region",
                 choiceNames = levels(as.factor(data$event_type)),
                 choiceValues = levels(as.factor(data$event_type)))
      ),
      
      menuItem("ACTOR TYPE", tabName = "actorTypeTab", icon = icon("fist-raised"),    #Actor Type filter
               
               checkboxInput(inputId="actorTypeAllCheckBox",                          #Select All/None check box
                             label = "Select All/None",
                             value = TRUE),
               
               checkboxGroupInput(                                                   #Actor Type check boxes
                 inputId = "actorTypeCheckBoxes",label = "Actor Type",
                 choiceNames = levels(as.factor(data$actor_type)),
                 choiceValues = levels(as.factor(data$actor_type)))
               
      )
    )
  ),
  dashboardBody(
    
    valueBox("Dataset from the Armed Conflict Location & Event Data", "All conflicts from 1997 are displayed in this dataset. In the Side Menu, you can select the View and filter by Date, Event Type, Region Type and Actor Type.",color="blue",width="200%"),
    tabItems(
      tabItem(tabName = "dashboard",               #Tab showing graphs
              
              fluidRow(
                tabBox(
                  title = "Fatalities/Event Count vs. Years",width = "12",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", 
                  tabPanel("Fatalities", plotlyOutput("distPlot")), #Total Fatalities per event type throughout the years
                  tabPanel("Event Count", plotlyOutput("distPlot3")) #Event Count per event type throughout the years
                )
              ),
              
              fluidRow(
                box(numericInput(inputId = "topN",
                                 label = "Select the number of top regions by total events/fatalities:",
                                 min = 2,
                                 width="30%",
                                 max = min(10,length(data$region)),
                                 value = min(3,length(data$region))),
                    width = "12",

                    tabBox(id = "tabset2",
                           title = "Fatalities/Event Count vs. Region",
                           width = "12",                                     #Both Tabs show top N regions based on either Total Fatalities or Event Count
                           tabPanel("Fatalities",plotlyOutput("distPlot4")), #Stacked Total Fatalities per Event Type vs. Region
                           tabPanel("Event Count",plotlyOutput("distPlot2")) #Stacked Event Count per Event Type vs. Region
                    )
                )
              ),
      ),
      
      tabItem(
        tabName = "dashboard2",                        #Tab showing a map
        fluidRow(
          plotlyOutput("map1")
        ),

        fluidRow(
          valueBoxOutput("event_count_this_year"),   #Shows Event count this year (2020)
          valueBoxOutput("fatalities_this_year"),    #Shows Total fatalities this year (2020)
          valueBoxOutput("country_top_fatalities")   #Shows the country with most total fatalities
        ),
        fluidRow(
          column(width = 1,offset = 2),
          valueBoxOutput("event_with_most_fatalities",width = "6") #Shows details (Country, exact date, number of fatalities) of event with most fatalities
        )
      )
      
    )
  )
)


                             #############################################
                            ############# HOPE YOU ENJOYED IT!  ############
                             #############################################
