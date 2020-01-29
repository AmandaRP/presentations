#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("wildfires.R")


# Define UI for application
ui <- fluidPage(theme = shinytheme("cerulean"), 
                #See more themes at https://rstudio.github.io/shinythemes/ 
   
  # Plots and controls 
  splitLayout(
    verticalLayout(
      titlePanel("Climate: South East Australia"),
      p("A focused look at historic temperatures, rainfall, and current wildfires in 
        the south east part of Australia"),
      titlePanel("Current Wildfires in New South Wales"),  
      mapviewOutput("mapPlot"),
      HTML("<p align='right'><font size='1'>Source: NSW Rural Fire Service</font></p>")
    ),
    verticalLayout(
      fluidRow(
        column(width = 2),
        column(width = 5,
          sliderInput("year", "Years:",
                    min = 1930, max = 2018,
                    value = c(1930,2018))
          ),
        column(width = 5,
          checkboxGroupInput("city", "City:", 
                             choices = c("Brisbane" = "BRISBANE",
                                         "Canberra" = "CANBERRA", 
                                         "Melbourne" = "MELBOURNE", 
                                         "Sydney" = "SYDNEY"),
                             selected = c("BRISBANE", "CANBERRA", "MELBOURNE", "SYDNEY"),
                                  inline = FALSE
                             )
        )
          
      ),
      plotOutput("tempPlot", height = "250px"),
      plotOutput("rainPlot", height = "250px")
    )
    
      
  )
)

# Define server logic 
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
   
   # Rain plot
   output$rainPlot <- renderPlot({
     p + xlim(input$year[1], input$year[2]) +
       gghighlight(tolower(city_name) %in% tolower(input$city), 
                   use_group_by = FALSE,
                   use_direct_label = FALSE) 
 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

