library(shiny)
library(leaflet)
library(plotly)
library(DT)

shinyUI(navbarPage("Yelp Reviews Analysis", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("libraries/styles.css"),
        includeScript("libraries/gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%" ),


      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 400, left = 20, right = 20, bottom = "auto",
                                   width = 690, height =  "auto",
                    h4("The Steakhouse Analysis: "),
                    br(),
                    h4("click on the steakhouse you are interested in, or go to 'Steakhouses Finder' to search for it."),
                    conditionalPanel("input.map_marker_click",
                                     h4("what the hell"),
                                     top=20, left=60, height=400, width=200,
                                     style="padding-left: 10px; padding-right: 8px; padding-top: 8px; padding-bottom: 8px")
      ),

    )
  ),

  tabPanel("Steakhouses Finder",
    sidebarLayout(
    sidebarPanel(
    DT::dataTableOutput("ziptable")
  ),
  mainPanel(
    
    p(h4("Use this panel to check how can the improvement help with your rating ",align="center")),
    tags$head(
      tags$style("label{font-family: BentonSans Book;}")
    ),
    
    fluidRow(
      
      column(4,
             sliderInput('topic1',  h3("topic1"), min=0, max=0.3, value=0.2, round=0)
      ),
      column(4,
             sliderInput('topic2', label = h3("topic2"), min=0, max=0.3, value=0.2, round=0)
      ),
      column(4,
             sliderInput('topic3', label = h3("topic3"), min=0, max=0.3, value=0.2, round=0)
      ),
      column(4, 
             sliderInput('topic4', label = h3("topic4"), min=0, max=0.3, value=0.2, round=0)
      ),                  
      column(4, 
             sliderInput('topic5', label = h3("topic5"), min=0, max=0.3, value=0.2, round=0)
      ),
    ),
    #submitButton("Check"),
    helpText(h4("Your estimated rating is:"),align="center"),
    span(textOutput("value"), style="color:black;font-size: 25px",align="center"),
    hr(),
    fluidRow(
      h4("Search and select your business at the left-hand side panel, and the advice will be given below.",align="center")
    ),
    
    h4("You have selected:",align="center"),
    fluidRow(
      column(6,
             plotOutput('plot'),align="center"
      ),
      column(6,
             tableOutput('able'),align="center",
             h4("Here are some advice for you:",align="center"),
             conditionalPanel(
               condition = "output.relative_star < 0",
               h3("1. Your business's overall rating is a bit below average in steakhouses category")
             ),
             conditionalPanel(
               condition = "output.relative_star >= 0",
               h3("1. Your business's overall rating is better than average in steakhouses category!")
             ),
             conditionalPanel(
               condition = "output.relative_v1 < 0",
               h3("2. The bad reviews of topic 1 influence your business's rating largely.")
             ),
             conditionalPanel(
               condition = "output.relative_v1 >= 0",
               h3("2. You have many good reviews on topic 1.")
             ),
             conditionalPanel(
               condition = "output.relative_v2 < 0",
               h3("3. Your business's overall rating is better than average in steakhouses category!")
             ),
             conditionalPanel(
               condition = "output.relative_v2 >= 0",
               h3("3. Your business's overall rating is better than average in steakhouses category!")
             ),
             conditionalPanel(
               condition = "output.relative_v3 < 0",
               h3("4. Your business's overall rating is better than average in steakhouses category!")
             ),
             conditionalPanel(
               condition = "output.relative_v3 >= 0",
               h3("4. Your business's overall rating is better than average in steakhouses category!")
             ),
             conditionalPanel(
               condition = "output.relative_v4 < 0",
               h3("5. Your business's overall rating is better than average in steakhouses category!")
             ),
             conditionalPanel(
               condition = "output.relative_v4 >= 0",
               h3("5. Your business's overall rating is better than average in steakhouses category!")
             ),
             conditionalPanel(
               condition = "output.relative_v5 < 0",
               h3("6. Your business's overall rating is better than average in steakhouses category!")
             ),
             conditionalPanel(
               condition = "output.relative_v5 >= 0",
               h3("6. Your business's overall rating is better than average in steakhouses category!")
             ),
             
             
             
             span(textOutput("relative_star"), style="color:black;font-size: 25px",align="center"),
             
             
      )

    )


    
    )
  
))))
