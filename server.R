library(shiny) 
library(shinydashboard)
library(tidyverse)
library(ggplot2) #for the map
library(plotly)
library(viridis) #for the map

function(input, output,session) {
  
  l <- list(color = toRGB("grey"), width = 0.5)
  
  g <- list(
    fitbounds = "locations",
    visible = TRUE
  )

  observe({
    updateCheckboxGroupInput(
      session, "eventTypeCheckBoxes",
      choices = levels(as.factor(data$event_type)),
      selected = if(input$eventTypeAllCheckBox) levels(as.factor(data$event_type))
    )
  })

  observe({
    updateCheckboxGroupInput(
      session, "regionCheckBoxes",
      choices = levels(as.factor(data$region)),
      selected = if(input$regionAllCheckBox) levels(as.factor(data$region))
    )
  })

  observe({
    updateCheckboxGroupInput(
      session, "actorTypeCheckBoxes",
      choices = levels(as.factor(data$actor_type)),
      selected = if(input$actorTypeAllCheckBox) levels(as.factor(data$actor_type))
    )
  })
  data<-read.csv("data_updated.csv",header=T)
  
  df<- reactive({
    x <- data[,c("year","region","fatalities","event_type","actor_type","country","iso3")]

    if (length(input$eventTypeCheckBoxes)>0 ){   #to apply event type filter
      x <- x[x$event_type %in% input$eventTypeCheckBoxes,]
    }

    if (length(input$regionCheckBoxes)>0){      #to apply region filter
      x <- x[x$region %in% input$regionCheckBoxes,]
    }

    if (length(input$actorTypeCheckBoxes)>0){    #to apply actor type filter
      x <- x[x$actor_type %in% input$actorTypeCheckBoxes,]
    }
    x<-x[x$year %in% c((input$years_range)[1]:(input$years_range)[2]),]
    return(x)
  })
  
  
  output$distPlot <- renderPlotly({                                  #plots top graph first tab
    
    y<-df()
    w<-y %>% 
      group_by(event_type,year)%>%
      summarise(total_fatalities=sum(fatalities)) %>% ungroup()
    
    w %>% ggplot(aes(year,total_fatalities,colour=event_type)) + geom_line(size=1) + labs(x="Year", y="Total Fatalities")
    
  })
  
  output$distPlot2 <- renderPlotly({        #plots graph below first tab
    top_N_regions <- data %>%
      group_by(region,event_type) %>%
      summarise(n=n()) %>% arrange(desc(n))

    selected.regions<-top_N_regions[,c("region","n")] %>%
      group_by(region) %>%
      summarise(sum=sum(n)) %>% arrange(desc(sum)) %>% head(input$topN)  #gets top N region based on event count for each region

    data.selected <- top_N_regions %>% filter(region %in% selected.regions$region) #apply region filter

    data.selected %>% ggplot(aes(x = region, y = n))+ geom_col(aes(fill = event_type), width = 0.7) + labs(x="Region",y="Event Count")
  })

  output$distPlot3 <- renderPlotly({ #plots top graph second tab
    y<-df()
    w<-y %>%
      group_by(event_type,year)%>%
      summarize(event_counts=n())
    w %>% ggplot(aes(year,event_counts,colour=event_type)) + geom_line(size=1) + labs(x="Year", y=" Event Count")

  })

  output$distPlot4 <- renderPlotly({   #plots below graphs second tab

    top_N_regions <- data %>%
      group_by(region,event_type) %>%
      summarise(sum1=sum(fatalities)) %>%
      ungroup() %>%
      arrange(desc(sum1)) #arrange regions in descending order of total fatalities

    selected.regions<-top_N_regions[,c("region","sum1")] %>%
      group_by(region) %>%
      summarise(sum=sum(sum1)) %>%
      arrange(desc(sum)) %>%
      head(input$topN) %>%
      ungroup()          #gets top N region based on total fatalities for each region

    data.selected <- top_N_regions %>% filter(region %in% selected.regions$region) #apply region filter
    data.selected %>%
      ggplot(aes(x = region, y = sum1))+ geom_col(aes(fill = event_type) , width = 0.7) + labs(x="Region", y="Total Fatalities")
  })


  output$map1 <- renderPlotly({  #code to show the map

    y<-df()
    w<-y %>%
      group_by(country,iso3)%>%
      summarise(total_fatalities=sum(fatalities),event_count=n()) %>% ungroup()


    plot_geo(w) %>% add_trace(
      z = ~total_fatalities, color = ~total_fatalities,
      text = ~paste(country,"| Total Fatalities:",total_fatalities,"| Event Count:",event_count,sep=" "), locations=~iso3,marker = list(line = l),colorscale="Viridis"
    ) %>% colorbar(title = 'Total Fatalities')

  })


  output$event_count_this_year <- renderValueBox({
    my.year= "2020" #format(Sys.Date(), "%Y") but wont work if we are in year 2021+ because data is only for 2020 and below
    valueBox(color = "yellow",
             value = data %>%
               filter( year == my.year  ) %>%
               summarise(n()),
             subtitle = "Total events this year",
             icon = icon("calendar-times"),
    )
  })

  output$fatalities_this_year <- renderValueBox({
    my.year= "2020" #format(Sys.Date(), "%Y") but wont work if we are in year 2021+ because data is only for 2020 and below
    valueBox(color = "orange",
             value = data %>%
               filter( year == my.year  ) %>%
               summarise(total_fatalities=sum(fatalities)),
             subtitle = "Total fatalities this year",
             icon = icon("cross")
    )
  })

  output$country_top_fatalities <- renderValueBox({
    country_fatalities <- data[,c("country","fatalities")]  %>%
      group_by(country)  %>%
      summarise(total_fatalities=sum(fatalities)) %>%
      arrange(desc(total_fatalities)) %>%
      head(1)

    valueBox(color = "red",
             subtitle = paste(country_fatalities[,"country"],", the country with the most fatalities",sep = ""),
             value = country_fatalities[,2],
             icon = icon("exclamation-circle")
    )
  })


  output$event_with_most_fatalities <- renderValueBox({
    val <- data[which.max(data$fatalities),c("country","fatalities","event_date")]
    valueBox(color = "black",
             "Event with most fatalities:" ,
             subtitle = paste("Fatalities: ",val[,2]," | Location: ",val[,1]," | Date: ",val[,3],sep = ""),
             icon = icon("")
    )
  })


}
