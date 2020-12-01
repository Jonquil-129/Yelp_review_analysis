<<<<<<< HEAD
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(DT)
library(magrittr)
library(plotly)
library(leafletCN)
library(rsconnect)
set.seed(100)
business.df=read.csv("finalsteak.csv",header = T)

f <- function(x) { 
  aaa=1.79176+3.01248*x[1]+2.17228*x[2]+2.97643*x[3]+3.00886*x[4]-0.52453*x[5]
  if (aaa>5){
    return(5)
  }else if (aaa<0){
    return(0)
  }else{
    return(aaa)
  }
}
cleantable <- business.df %>%
  select(
    Name= name,
    City = city,
    State = state,
    Zipcode = postal_code,
    Stars = stars,
    Lat = latitude,
    Long = longitude
  )
analysis_table=business.df[,c(2,3,4,11,12,18,19,20,21,24)]
analysis_table$relative_star=analysis_table$stars-mean(analysis_table$stars)
analysis_table$relative_v1=analysis_table$v1-mean(analysis_table$v1)
analysis_table$relative_v2=analysis_table$v2-mean(analysis_table$v2)
analysis_table$relative_v3=analysis_table$v3-mean(analysis_table$v3)
analysis_table$relative_v4=analysis_table$v4-mean(analysis_table$v4)
analysis_table$relative_v7=analysis_table$v7-mean(analysis_table$v7)




shinyServer(function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(data=business.df) %>%
      addProviderTiles(providers$OpenStreetMap) %>% 
      #addTiles(
      #  urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #  attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      #) %>%
      addMarkers(~longitude, ~latitude, popup = ~name, label = ~name)  %>% 
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(business_data[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(business.df,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  output$value=reactive({
    f(c(input$topic1,input$topic2,input$topic3,input$topic4,input$topic5))
  })

  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    
    output$Click_text<-renderText({text2})
  })
  
  ## Data Explorer ###########################################
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
                      selected = stillSelected)
  })
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
                      selected = stillSelected)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$ziptable <- DT::renderDataTable(cleantable[,1:5],filter='top',selection='single',server = FALSE)

  output$able = renderTable({
    cleantable[input$ziptable_rows_selected,1:5]
    })

  output$plot=renderPlot({
    plot(density(analysis_table$stars),main="The rating distribution for 'steakhouses' ",xlab="rating/stars",ylab = "density")
    abline(v=cleantable[input$ziptable_rows_selected,5],col="red",cex=3)
    text(cleantable[input$ziptable_rows_selected+0.5,5],0.6,"You are here!",col = "red")
  })
  
  output$relative_star=reactive({analysis_table$relative_star[input$ziptable_rows_selected]})
  output$relative_v1=reactive({analysis_table$relative_v1[input$ziptable_rows_selected]})
  output$relative_v2=reactive({analysis_table$relative_v2[input$ziptable_rows_selected]})
  output$relative_v3=reactive({analysis_table$relative_v3[input$ziptable_rows_selected]})
  output$relative_v4=reactive({analysis_table$relative_v4[input$ziptable_rows_selected]})
  output$relative_v7=reactive({analysis_table$relative_v7[input$ziptable_rows_selected]})
  
  })
=======
font_home <- function(path = '') file.path('~', '.fonts', path)
if (Sys.info()[['sysname']] == 'Linux') {
  dir.create(font_home())
  file.copy('wqy-zenhei.ttc', font_home())
  system2('fc-cache', paste('-f', font_home()))
}

library(shiny)
require(DT)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(magrittr)
library(plotly)
library(leafletCN)
library(rsconnect)


set.seed(100)
business.df=read.csv("finalsteak.csv",header = T)

f <- function(x) { 
  aaa=1.79176+3.01248*x[1]+2.17228*x[2]+2.97643*x[3]+3.00886*x[4]-0.52453*x[5]
  if (aaa>5){
    return(5)
  }else if (aaa<0){
    return(0)
  }else{
    return(aaa)
  }
}
cleantable <- business.df %>%
  select(
    Name= name,
    City = city,
    State = state,
    Zipcode = postal_code,
    Stars = stars,
    Lat = latitude,
    Long = longitude
  )
analysis_table=business.df[,c(2,3,4,11,12,18,19,20,21,24)]
analysis_table$relative_star=analysis_table$stars-mean(analysis_table$stars)
analysis_table$relative_v1=analysis_table$v1-mean(analysis_table$v1)
analysis_table$relative_v2=analysis_table$v2-mean(analysis_table$v2)
analysis_table$relative_v3=analysis_table$v3-mean(analysis_table$v3)
analysis_table$relative_v4=analysis_table$v4-mean(analysis_table$v4)
analysis_table$relative_v7=analysis_table$v7-mean(analysis_table$v7)




shinyServer(function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(data=business.df) %>%
      addProviderTiles(providers$OpenStreetMap) %>% 
      #addTiles(
      #  urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #  attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      #) %>%
      addMarkers(~longitude, ~latitude, popup = ~name, label = ~name)  %>% 
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(business_data[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(business.df,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  output$value=reactive({
    f(c(input$topic1,input$topic2,input$topic3,input$topic4,input$topic5))
  })

  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    
    output$Click_text<-renderText({text2})
  })
  
  ## Data Explorer ###########################################
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
                      selected = stillSelected)
  })
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
                      selected = stillSelected)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$ziptable <- DT::renderDataTable(cleantable[,1:5],filter='top',selection='single',server = FALSE)

  output$able = renderTable({
    cleantable[input$ziptable_rows_selected,1:5]
    })

  output$plot=renderPlot({
    plot(density(analysis_table$stars),main="The rating distribution for 'steakhouses' ",xlab="rating/stars",ylab = "density")
    abline(v=cleantable[input$ziptable_rows_selected,5],col="red",cex=3)
    text(cleantable[input$ziptable_rows_selected+0.5,5],0.6,"You are here!",col = "red")
  })
  
  output$relative_star=reactive({analysis_table$relative_star[input$ziptable_rows_selected]})
  output$relative_v1=reactive({analysis_table$relative_v1[input$ziptable_rows_selected]})
  output$relative_v2=reactive({analysis_table$relative_v2[input$ziptable_rows_selected]})
  output$relative_v3=reactive({analysis_table$relative_v3[input$ziptable_rows_selected]})
  output$relative_v4=reactive({analysis_table$relative_v4[input$ziptable_rows_selected]})
  output$relative_v7=reactive({analysis_table$relative_v7[input$ziptable_rows_selected]})
  
  })
>>>>>>> 5fe0187ca6e915ff798e2e87abb947d9503b16fd
