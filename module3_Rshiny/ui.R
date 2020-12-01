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
             sliderInput('topic1',  h3("Bar"), min=0, max=0.3, value=0.2, round=0)
      ),
      column(4,
             sliderInput('topic2', label = h3("Cheese curds"), min=0, max=0.3, value=0.2, round=0)
      ),
      column(4,
             sliderInput('topic3', label = h3("Service and atmosphere"), min=0, max=0.3, value=0.2, round=0)
      ),
      column(4, 
             sliderInput('topic4', label = h3("Quality of Steaks"), min=0, max=0.3, value=0.2, round=0)
      ),                  
      column(4, 
             sliderInput('topic5', label = h3("Service time"), min=0, max=0.3, value=0.2, round=0)
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
             plotOutput('plot'),align="center",
             p("Compared with other steakhouses, your relative rating and ratings on the above each topics are:"),
             textOutput("relative_star"),
             textOutput("relative_v1"),
             textOutput("relative_v2"),
             textOutput("relative_v3"),
             textOutput("relative_v4"),
             textOutput("relative_v7"),
      ),
      column(6,
             tableOutput('able'),align="center",
             h4("Here are some advice for you:",align="center"),
             conditionalPanel(
               condition = "output.relative_star < 0",
               p(h4(" Your business's overall rating is a bit below average in steakhouses category"))),
             conditionalPanel(
               condition = "output.relative_star >= 0",
               h4(" Your business's overall rating is better than average in steakhouses category!")),
             conditionalPanel(
               condition = "output.relative_v1 < 0",
               h4(" You have a bar for your steakhouse, which is quite impressive! But if you can do more on your bar's menu, your rating might be better. Check how it will change your rating in the upper slider panel.")),
             conditionalPanel(
               condition = "output.relative_v2 < 0",
               h4(" You have good reviews on chees curds, but it seems you did not do as good as other steakhouses do in this aspect. Improve the taste of your cheese may lead to better rating! Check how it will change your rating in the upper slider panel.")),
             conditionalPanel(
               condition = "output.relative_v4 < 0",
               h4(" Nothing is more important than the quality of the steaks for a steakhouse! But it seems there is still room for improvement in this aspects for you. Check how it will change your rating in the upper slider panel.")),
             conditionalPanel(
               condition = "output.relative_v3 >= 0",
               h4(" Atmosphere of your steakhouse is quite good. Some reviews describe your steakhouse's Atmosphere as excellent.") ),
             conditionalPanel(
               condition = "output.relative_v5 < 0",
               h4(" There happens to have some bad reviews regarding the service time. Try not to keep cunstomers waiting for too long, or they might give low rating to your steakhouse.")),
             conditionalPanel(
               condition = "output.relative_v5 >= 0",
               h4(" It is very urgent that you have to improve your steakhouse's service, especially serving time. Customers are complaining about the service time, which largely affected you business's overall rating! Check how it will change your rating in the upper slider panel.")
             ),
             
             
             
      )

    )


    
    )
  
))))
