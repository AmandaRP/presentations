#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Instructions:
# 1. Try different themes (line 21)
# 2. The rainfall plot (rainPlot) is defined in the server function, but not 
#    showing in the UI. Add it to the UI, below the temperature plot.
# 3. The year slider and city checkbox selection are not having an effect 
#    on the plots. Fix this in the server function.

library(shiny)
source("wildfires.R")

# Define UI for application
ui <- fluidPage(theme = shinytheme("cerulean"), 
                #See more themes at https://rstudio.github.io/shinythemes/ 
   
  # Plots and controls 
  splitLayout(
    verticalLayout(
      titlePanel("Climate: South East Australia"),
      p("A focused look at historic temperatures, rainfall, and current NSW wildfires"),
      titlePanel("Current New South Wales Wildfires"),  
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
      plotOutput("tempPlot", height = "250px")
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
       filter(year >= 1930, year <= 2018, city_name %in% c("CANBERRA", "MELBOURNE")) %>%
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
     p + xlim(1930, 2018) +
       gghighlight(tolower(city_name) %in% tolower(c("CANBERRA", "MELBOURNE")), 
                   use_group_by = FALSE,
                   use_direct_label = FALSE) 
 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

