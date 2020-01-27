#
# This is a Shiny dashboard web application. You can run the application by 
# clicking the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    shiny: http://shiny.rstudio.com/
#    shinydashboard: https://rstudio.github.io/shinydashboard/index.html


# Instructions: Search for "TODO" in the code below and follow the in-line 
#               instructions

library(shinydashboard)
source("wildfires.R")

ui <- dashboardPage(skin = "purple", # see more skins at https://rstudio.github.io/shinydashboard/appearance.html
                    
  dashboardHeader(title = "Climate: New South Wales, Australia", 
                  titleWidth = 400),
  
  dashboardSidebar(
  
    #TODO: add the sliderInput here,
    
    checkboxGroupInput("city", "City:", 
                       choices = c("Canberra" = "CANBERRA", 
                                   "Melbourne" = "MELBOURNE", 
                                   "Sydney" = "SYDNEY"),
                       selected = c("CANBERRA", "MELBOURNE", "SYDNEY"),
                       inline = FALSE),
    
    p("Description: A focused look at historic temperatures, rainfall, and current wildfires in 
        New South Wales, Australia")),
        
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(#TODO: Add the templot here,
          #TODO: Add the rain plot here
        ),
      box(
        headerPanel("Current Wildfires"),
        #TODO: add the mapPlot here,
        HTML("<p align='right'><font size='1'>Source: NSW Rural Fire Service</font></p>")
      )
    )
    
  
  )
)


server <- function(input, output) {
  #TODO: Use the same code here that we used in the shiny app server function
}

# Run the application 
shinyApp(ui, server)
