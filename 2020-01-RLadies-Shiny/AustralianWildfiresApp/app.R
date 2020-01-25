#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("wildfires.R")


# Define UI for application
ui <- fluidPage(
   
  # Application title
  
  titlePanel("Climate: New South Wales, Australia"),
  p("A focused look at historic temperatures and current wildfires in 
    New South Wales, Australia"),
   
  # Sidebar with a slider input for number of bins 
  splitLayout(
    verticalLayout(
      titlePanel("Temperature"),
      fluidRow(
        column(width = 6,
          sliderInput("year", "Years:",
                    min = 1930, max = 2018,
                    value = c(1930,2018))
          ),
        column(width = 6,
          checkboxGroupInput("city", "City:", 
                             choices = c("Canberra" = "CANBERRA", 
                                         "Melbourne" = "MELBOURNE", 
                                         "Sydney" = "SYDNEY"),
                                  inline = FALSE
                             )
        )
          
      ),
      plotOutput("tempPlot")
    ),
    
    verticalLayout(
      titlePanel("Wildfires"),  
      mapviewOutput("mapPlot")   
    )  
  )
)

# Define server logic required to draw a histogram
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
       group_by(year) %>%
       summarize(avg_temp = mean(temperature, na.rm = TRUE)) 
       
       # Compute mean across all (selected) cities and years
     overall_avg_temp <- temp_avgs %>% 
       summarize(overall_avg_temp = mean(avg_temp)) %>% 
       unlist()
       
     temp_avgs %>% 
       mutate(temp_minus_mean = avg_temp - overall_avg_temp) %>%
       ggplot(aes(year, temp_minus_mean, fill = temp_minus_mean<0)) + 
       geom_col() +
       labs(x = element_blank(), 
            y = element_blank(),
            caption = "Source: Australian Government Bureau of Meteorology") +
       theme_minimal() +
       scale_y_continuous(breaks = c(-1, -0.5, 0.5, 1),
                          labels = c(parse(text = TeX('$-1.0^o$')), 
                                     parse(text = TeX('$-0.5^o$')), 
                                     parse(text = TeX('$+0.5^o$')), 
                                     parse(text = TeX('$+1.0^o$')))) +
       theme(plot.background = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.major.x = element_line(linetype = "dashed", color = "grey"),
         panel.grid.minor.x = element_blank(),
         plot.title = element_text(face = "bold"),
         legend.position = "none",
         plot.caption = element_text(color = "darkgrey")) +
       annotate("text", 
                x = 1962, 
                y = 0.7, 
                color = "darkgrey",
                label = "Annual temperature (degrees Celcius)\nabove or below the 1950-2018 average") 
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

