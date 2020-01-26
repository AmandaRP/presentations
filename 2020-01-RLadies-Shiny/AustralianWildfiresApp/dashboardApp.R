#
# This is a Shiny dashboard web application. You can run the application by 
# clicking the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    shiny: http://shiny.rstudio.com/
#    shinydashboard: https://rstudio.github.io/shinydashboard/index.html


library(shinydashboard)
source("wildfires.R")

ui <- dashboardPage(skin = "purple", # see more skins at https://rstudio.github.io/shinydashboard/appearance.html
                    
  dashboardHeader(title = "Climate: New South Wales, Australia", 
                  titleWidth = 400),
  
  dashboardSidebar(
  
    sliderInput("year", "Years:",
                min = 1930, max = 2018,
                value = c(1930,2018)),
    
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
      box(plotOutput("tempPlot", height = "250px"),
          plotOutput("rainPlot", height = "250px")),
      box(
        headerPanel("Current Wildfires"),
        mapviewOutput("mapPlot"),
        HTML("<p align='right'><font size='1'>Source: NSW Rural Fire Service</font></p>")
      )
    )
    
  
  )
)


server <- function(input, output) {
  
  # Plot Fires courtesy of Dean Marchiori
  output$mapPlot <- renderMapview({
    mapview(fire_poly, 
            zcol = "title", 
            color = "orange", 
            col.regions = "red", 
            legend = FALSE)
  })
  
  
  #temperature plot
  output$tempPlot <- renderPlot({
    
    # Compute averages (for selected cities and years)
    temp_avgs <- temperature %>% 
      filter(year >= input$year[1], year <= input$year[2], city_name %in% input$city) %>%
      group_by(year) %>%
      summarize(avg_temp = mean(temperature, na.rm = TRUE)) 
    
    # Compute mean across all (selected) cities and years
    overall_avg_temp <- temp_avgs %>% 
      summarize(overall_avg_temp = mean(avg_temp)) %>% 
      unlist()
    
    # Create plot using function defined in wildfires.R (to simplify this script)
    p <- create_temperature_plot(temp_avgs, overall_avg_temp)
    p
    
  })
  
  output$rainPlot <- renderPlot({
    p + xlim(input$year[1], input$year[2]) +
      gghighlight(tolower(city_name) %in% tolower(input$city), 
                  use_group_by = FALSE,
                  use_direct_label = FALSE) 
    
  })
  
}

# Run the application 
shinyApp(ui, server)
