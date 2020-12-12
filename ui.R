#importing required libraries
library(ggplot2)
library(leaflet)
library(rgdal)
library(dplyr)
library(shiny)
library(zoo)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(highcharter)
library(plotly)
library(shinyWidgets)


#Dashboard Function
dashboardPage(
  #Defining dashboard header
  dashboardHeader(#link created for search new homes
                  title = "                                                                                           Melbourne Housing Market Analysis",titleWidth = 1020,
                  tags$li(
                    a(
                      icon("search"),
                      "Search new homes",
                      height = 30,
                      href = "https://www.raywhite.com/real-estate/melbourne/",
                      title = "",
                      target = "_blank"
  
                    ),class="dropdown")
                  
                      
                        
                    ),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      #Creating 2 tabs using tabBox
      tabBox(
        title = NULL, width = 12,
        id = "tabset1", height = "1400",
        #Tab 1
        tabPanel("Trends of Council", 
          box(
          #Map output
          leafletOutput("melb", height = "400"),title="Melbourne City (Select the desired council in the map to view the market trends)",height = 470,width = 12,solidHeader = T),
          
          box(
          #Printing selected Council Area
          title = textOutput("txt"),width = 12,height=37,status = "primary"),
          box(
            box(
              #Taking year input from user using radio button
              radioButtons("yr_select", label = "Select the year",
                                                 choices = list("2016" = "2016", "2017" = "2017","2018"="2018"),
                                                 selected = "2017"),width = 3,status = "primary"),
          box(
            #Market trend graphs output
            highchartOutput("plot1", height = "300"),width = 9,status = "primary"),width=12,title="Monthly sales of houses",status = "primary",solidHeader = T),
          box(
            highchartOutput("plot2", height = "300"),title = "Price Distributuion of Top 5 Suburbs",width = 6,status = "primary",solidHeader = T),
          box(
            plotlyOutput("plot3", height = "300"),title = "Trends of average prices over the time",width = 6,status = "primary",solidHeader = T)),
        
        #Tab 2
        tabPanel("Average Price Analysis", box(
          
          #Bubble chart output
          highchartOutput("plot4", height = "280"),
          
          #Time slider input
          sliderTextInput(
          inputId = "year",
          label = "Quarter range slider:",
          choices = c("2016 Q1","2016 Q2","2016 Q3","2016 Q4","2017 Q1","2017 Q2","2017 Q3","2017 Q4","2018 Q1"),
          selected = "2017 Q1",
          animate = animateOptions(duration=200),
          grid = TRUE,
          
        ),
        p(paste("Note: Select the year quarter from the slider or click on the play button to animate the visualisation")),
          width = 12,title="Average Price with respect to Distance from CBD"))
        
        
                   
                 ))
        
        
      )
    )
      
      
  
    

