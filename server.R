#importing the required libraries
library(ggplot2)
library(leaflet)
library(rgdal)
library(dplyr)
library(shiny)
library(zoo)
library(shinydashboard)
library(shinydashboardPlus)
library(highcharter)
library(plotly)
library(shinyWidgets)


function(input,output,session){
  #Set the working directory to the location where files are saved
  #setwd("C:/New folder (2)")
  
  #reading csv data
  df<-read.csv("Vizdata3.csv")
  
  #reading map shape file
  melb<- readOGR("Australia_admin_6.shp")
  
  #Calculation of number of houses sold
  mel<-subset(melb,name=="City of Yarra"|name=="City of Banyule"|name=="City of Bayside"|name=="City of Boroondara"|name=="City of Brimbank"|name=="City of CardiniaShireCouncil"|name=="City of Casey"|name=="City of Darebin"|name=="City of Frankston"|name=="City of Glen Eira"|name=="City of Greater Dandenong"|name=="City of Hobsons Bay"|name=="City of Hume"|name=="City of Kingston"|name=="City of Knox"|name=="City of MacedonRangesShireCouncil"|name=="City of Manningham"|name=="City of Maribyrnong"|name=="City of Maroondah"|name=="	City of Melbourne"|name=="City of Melton"|name=="City of MitchellShireCouncil"|name=="City of Monash"|name=="City of Moonee Valley"|name=="City of MooraboolShireCouncil"|name=="City of Moreland"|name=="Shire of Nillumbik"|name=="City of Port Phillip"|name=="City of Stonnington"|name=="City of Whitehorse"|name=="City of Whittlesea"|name=="City of Wyndham"|name=="City of YarraRangesShireCouncil")
  avg_price<-aggregate(Price~CouncilArea,df,length)
  prices <- avg_price$Price[match(mel$name,avg_price$CouncilArea)]
  
  
  ids<-c(1:27)
  n<-(mel$name)
  output$text1<-renderPrint({"Melbourne City"})
  #Function for plotting leaflet map
  output$melb <- renderLeaflet({
    
    
    
    cpal <- colorNumeric("YlOrRd", prices)
    labels <- sprintf(
      "<strong>%s</strong>",
      mel$name
    ) %>% lapply(htmltools::HTML)
    m<-leaflet(mel) %>% # create a blank canvas
      addTiles() %>%
      addProviderTiles(providers$OpenMapSurfer.AdminBounds) %>%
      addPolygons( 
        stroke = TRUE, color="red",
        smoothFactor = 0.2, 
        fillOpacity = 0.85,
        fillColor = ~cpal(prices),
        weight = 2,
        layerId=ids,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        )%>%
      addLegend("bottomright",pal=cpal,values = ~prices,title = "Count of houses sold (2016-2018)")
    })
  
  #Function to capture mouse clicks
  observeEvent(input$melb_shape_click, {
    click <- input$melb_shape_click
    if(is.null(input$melb_shape_click$id)){
      input$melb_shape_click$id=1}
    else{
      print(input$melb_shape_click)
    }
    
    })
    
    #FUnction to plot highchart pie-chart
    output$plot1 <- renderHighchart({
      
      #Calculation for computing percentage of monthly sales data
      df$year<-format(as.Date(df$Date, format="%d/%m/%Y"),"%Y")
      df$month<-format(as.Date(df$Date, format="%d/%m/%Y"),"%b")
      df$month1<-format(as.Date(df$Date, format="%d/%m/%Y"),"%m")
      df<-subset(df, CouncilArea == as.character(n[input$melb_shape_click$id]) & year == input$yr_select)
      ris<-aggregate(Price~month,FUN=length,data=df)
      ris$month1<-df$month1[match(ris$month,df$month)]
      ris$prcnt<-round(ris$Price/sum(ris$Price)*100,2)
      ris<-ris[order(ris$month1),]
      
      #plotting pie chart
      p<-highchart() %>% 
        hc_chart(type = "pie") %>% 
        hc_add_series_labels_values(labels = ris$month, values = ris$prcnt) %>%
        hc_tooltip(valueDecimals = 0,
                   pointFormat = "Percentage: {point.y}")
      print(p)
     
    })
    #Function to plot bar graph
    output$plot2 <- renderHighchart({
      
      
      x=aggregate(Price~Suburb,data=subset(df,CouncilArea == as.character(n[input$melb_shape_click$id])),FUN=mean)
      x=head(x[order(-x$Price),],5)
      a <- c("Average Price: ")
      b <- sprintf("{point.%s:.2f}", c("Price"))
      tltip <- tooltip_table(a, b)
      p2<-x %>%
        hchart('bar',hcaes(x = Suburb ,y = Price), colorByPoint = TRUE)%>%
        hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip)
      p2
    })
    
    #Function to plot line graph
    output$plot3 <- renderPlotly({
      df$Quarter=as.yearqtr(df$Date, format = "%d/%m/%Y")
      
      x1=aggregate(Price~Quarter,data=subset(df,CouncilArea == as.character(n[input$melb_shape_click$id])),FUN=mean)
      x1
      p3<-ggplot(x1, aes(y=Price,x=Quarter)) +
        geom_line(color="orange")
      p3
    })
    output$txt <- renderText({
      paste("Selected Council Area: ", as.character(n[input$melb_shape_click$id]))
    })
    
    #Function to plot bubble chart
    output$plot4 <- renderHighchart({
     
      #Calculation using aggregate
      df$quarter=as.yearqtr(df$Date, format = "%d/%m/%Y")
      x1=aggregate(x=df$Distance,by=list(df$CouncilArea,df$quarter),FUN=mean,data=df)
      x1
      names(x1)[names(x1)=="Group.1"]<-"CouncilArea"
      names(x1)[names(x1)=="Group.2"]<-"quarter"
      names(x1)[names(x1)=="x"]<-"Distance"
      y1=aggregate(x=df$Price,by=list(df$CouncilArea,df$quarter),FUN=mean,data=df)
      names(y1)[names(y1)=="Group.1"]<-"CouncilArea"
      names(y1)[names(y1)=="Group.2"]<-"quarter"
      names(y1)[names(y1)=="x"]<-"Price"
      z1=aggregate(Price~CouncilArea,FUN=length,data=df)
      
      new<-x1
      new$Price<-y1$Price
      new$Count<-z1$Price[match(new$CouncilArea,z1$CouncilArea)]
      new$Region <- df$Regionname[match(new$CouncilArea,df$CouncilArea)]
      nw<-subset(new,quarter==input$year)
      a <- c("Council Area: ", "Number of houses sold: ")
      b <- sprintf("{point.%s}", c("CouncilArea", "Count"))
      tltip <- tooltip_table(a, b)
        
      #Plotting Bubble chart using input from slider
      pp<-hchart(nw, "scatter", hcaes(Distance, Price, size = Count, color = Region))%>%
          hc_xAxis(title = list(text = "Distance from CBD"))%>%
          hc_yAxis(title = list(text = "Average Price"))%>%
          hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip)
      print(pp)
    })
    
    
    
}

    
  
    

  
  
  
