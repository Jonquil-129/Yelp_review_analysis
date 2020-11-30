library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(magrittr)
library(plotly)
library(leafletCN)
library(sf)

set.seed(100)
#business.df=read.csv("business_steakhouse.csv")
business.df=read.csv("finalsteak.csv")
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

#shape <- st_read("cb_2018_us_state_500k.shp") %>% 
#  st_transform(shape, crs = 4326)

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
#      addMarkers(~longitude, ~latitude, popup = paste("good topics:",business.df$v1,business.df$v2), label = ~name)  %>% 
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

  # 
  # output$histRanking <- renderPlotly({
  #   colorBy <- input$color
  #   colorData <- zipsIFiltered()[[colorBy]]
  #   
  #   if (nrow(zipsIFiltered()) == 0)
  #     return(NULL)
  #   
  #   pal<-colorPalette(colorData,colorBy)
  # 
  #   labelx <- paste(input$business_category, "business", input$color , "(visible business)", sep=" ")
  #   ax <- list(
  #     title = labelx,
  #     showticklabels = TRUE
  #   )
  #   
  #   ay <- list(
  #     title = input$size,
  #     showticklabels = TRUE
  #   )
  #   
  #   p<-plot_ly(zipsIFiltered(),type = "bar", x = zipsIFiltered()[[input$color]], y = zipsIFiltered()[[input$size]], 
  #          marker= list(color=pal(colorData)), size = zipsIFiltered()[[input$size]],showscale = FALSE)%>%
  #          layout(xaxis = ax, yaxis = ay)
  #   
  # })
  
  # output$scatterRanking <- renderPlotly({
  #   colorBy <- input$color
  #   colorData <- zipsIFiltered()[[colorBy]]
  #   
  #   if (nrow(zipsIFiltered()) == 0)
  #     return(NULL)
  #   
  #   pal<-colorPalette(colorData,colorBy)
  #   
  #   labelx <- paste(input$business_category, "business", input$color , "(visible business)", sep=" ")
  #   ax <- list(
  #     title = labelx,
  #     showticklabels = TRUE
  #   )
  #   
  #   ay <- list(
  #     title = input$size,
  #     showticklabels = TRUE
  #   )
  #   
  #   p<-plot_ly(zipsIFiltered(), x = zipsIFiltered()[[input$color]], y = zipsIFiltered()[[input$size]],
  #          mode = "markers", color= colorData,colors = "Spectral" , size = zipsIFiltered()[[input$size]],showscale = FALSE)%>%
  #   layout(xaxis = ax, yaxis = ay)
  # })
  
  # output$scatterRanking2 <- renderPlotly({
  #   if (nrow(zipsIFiltered()) == 0)
  #     return(NULL)
  #   
  #   # Get total review count for the states
  #   if(input$size=="Stars mark"){
  #     review_by_state <- aggregate( stars ~ state, data = zipsIFiltered(), FUN = sum)
  #     labelx <- paste(input$business_category, "business", input$color , "(visible business)", sep=" ")
  #     ax <- list(
  #       title = labelx,
  #       showticklabels = TRUE
  #     )
  #     
  #     ay <- list(
  #       title = input$size,
  #       showticklabels = TRUE
  #     )
  #     p<-plot_ly(review_by_state,type = "bar", x =state, y = stars,
  #                size = stars)%>%
  #    layout(xaxis = ax, yaxis = ay)
  #  }
  #   else{
  #     review_by_state <- aggregate( review_count ~ state, data = zipsIFiltered(), FUN = sum)
  #     labelx <- paste(input$business_category, "business", input$color , "(visible business)", sep=" ")
  #     ax <- list(
  #       title = labelx,
  #       showticklabels = TRUE
  #     )
  #     
  #     ay <- list(
  #       title = input$size,
  #       showticklabels = TRUE
  #     )
  #     p<-plot_ly(review_by_state,type = "bar", x = state, y = review_count,
  #                size = review_count)%>%
  #       layout(xaxis = ax, yaxis = ay)
  #   }
  #   
  # })
  # 
  # # This observer is responsible for maintaining the circles and legend,
  # # according to the variables the user has chosen to map to color and size.
  # observe({
  #   sizeBy <- input$size
  #   sizeRange <- input$size_scale
  #   colorBy <- input$color
  #   
  #   colorData <- zipsIFiltered()[[colorBy]]
  # 
  #   pal<-colorPalette(colorData,colorBy)
  #   radius <- zipsIFiltered()[[sizeBy]] * sizeRange 
  #   
  #   leafletProxy("map", data = zipsIFiltered()) %>%
  #     clearShapes() %>%
  #     addCircles(~longitude, ~latitude, radius=radius, layerId=~postal_code,
  #                stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
  #     addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
  #               layerId="colorLegend")
  # })
  # 
  # 
  # When map is clicked, show a popup with city info
  # observe({
  #   leafletProxy("map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   if (is.null(event))
  #     return()
  #   
  #   isolate({
  #     showZipcodePopup(event$id, event$lat, event$lng)
  #   })
  # })
  
  # Show a popup at the given location
  # showZipcodePopup <- function(zipcode, lat, lng) {
  #   selectedZip <- business.df[business.df$postal_code == zipcode,]
  #   content <- as.character(tagList(
  #     tags$h4(as.character(selectedZip$name)),
  #     tags$h5("Categories:", as.character(selectedZip$categories)),
  #     sprintf("Number of reviews: %s", as.integer(selectedZip$review_count)), tags$br(),
  #     sprintf("Stars score: %s", as.integer(selectedZip$stars))
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }
  
  # Calculate the color palette
  # colorPalette <- function(colorData, colorBy) {
  #   if(colorBy=="stars" || colorBy=="review_count"){
  #     pal <- colorBin("Spectral", colorData, 4, pretty=TRUE,alpha = FALSE)
  #   }else{
  #     pal <- colorBin("Spectral", colorData, 4, pretty=TRUE,alpha = FALSE)
  #   }
  #   return (pal)
  # }
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
   # text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
   # text2 <- paste("YOUVE CLICKED THE MAP!!!",click$lat, click$lng)
    
    # proxy <- leafletProxy("map")
    # proxy %>% clearPopups() %>%
    #   addPopups(click$lng, click$lat, text)
    
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
  
  output$ziptable <- DT::renderDataTable({
    df <- cleantable[,1:5] %>%
      filter(
        #Stars >= input$minScore,
        #Stars <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  #output$ziptable = DT::renderDataTable(, server = FALSE, selection = 'single')
  output$advice = renderPrint(input$ziptable_rows_selected)
})
