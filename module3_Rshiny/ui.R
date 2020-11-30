library(shiny)
library(leaflet)
library(plotly)
library(DT)

# Choices for drop-downs
# color_vars <- c(
#   "Stars mark" = "stars",
#   "Review count" = "review_count",
#   "Starts difference" = "stars_diff"
# )
# 
# size_vars <- c(
#   "Review count" = "review_count",
#   "Stars mark" = "stars"
# )

# category_vars <- c(
#    "All" = "All",
#    "Restaurant" = "Restaurant",
#    "Shopping" = "Shopping",
#    "Health & Medical" = "Health & Medical",
#    "Hotels" = "Hotels",
#    "Home Services" = "Home Services",
#    "Food"="Food"
# )

shinyUI(navbarPage("Yelp Reviews Analysis", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("libraries/styles.css"),
        includeScript("libraries/gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%" ),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
      #   draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
      #   width = 330, height = "auto",
      #   h4("Steakhouses finder")
      # ),

        #selectInput("color", "Color", color_vars),
        #selectInput("size", "Size", size_vars, selected = "adultpop"),
        #sliderInput("size_scale", "Size scale:",
        #            min = 10, max = 200, value = 50, step = 5),
        #br(),
        #h4("Filter options"),
        #checkboxInput("open_checkbox", label = "Currently Open", value = TRUE),
      #   selectInput("business_category", "Category", category_vars, selected = "adultpop"),
      # 
      #   sliderInput("reviews", "Minimum number of reviews:",
      #               min = 0, max = 500, value = 10, step = 5), 
      #   sliderInput("stars", "Minimum number of stars:",
      #                min = 0, max = 5, value = 0, step = 1)
      # ),

      # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
      #               draggable = TRUE, top = 400, left = 20, right = 20, bottom = "auto",
      #               width = 690, height =  "auto",
      #               tabsetPanel(type = "tabs",
      #                           tabPanel("Bar chart",plotlyOutput("histRanking", height = 200)),
      #                           tabPanel("Scatterplot", plotlyOutput("scatterRanking", height = 250)),
      #                           tabPanel("States summary",plotlyOutput("scatterRanking2", height = 250))
      #               )
      # ),
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
    fluidRow(
      column(3,
          selectInput("states", "States", c("WI","OH","PA","IL"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),

    hr(),
    DT::dataTableOutput("ziptable")
  ),
  mainPanel(
    
    p(h4("Use this panel to check how can the improvement help with your rating ",align="center")),
    tags$head(
      tags$style("label{font-family: BentonSans Book;}")
    ),
    
    fluidRow(
      
      column(4,
             sliderInput('topic1',  h3("topic1"), min=0, max=0.5, value=0.2, round=0),
      ),
      column(4,
             sliderInput('topic2', label = h3("topic2"), min=0, max=0.5, value=0.2, round=0),
      ),
      column(4,
             sliderInput('topic3', label = h3("topic3"), min=0, max=0.5, value=0.2, round=0),
      ),
      column(4, 
             sliderInput('topic4', label = h3("topic4"), min=0, max=0.5, value=0.2, round=0),
      ),                  
      column(4, 
             sliderInput('topic5', label = h3("topic5"), min=0, max=0.5, value=0.2, round=0),
      ),
    ),
    submitButton("Check"),
    helpText(h4("Your estimated rating is:"),align="center"),
    span(textOutput("value"), style="color:black;font-size: 25px",align="center"),
    
    fluidRow(
      h4("Search and select your business at the left-hand side panel, and the advice will be given below.",align="center"),
      span(textOutput("advice"), style="color:black;font-size: 25px",align="center")
    )
  )

    )
  
  #conditionalPanel("false", icon("crosshair"))
)))
