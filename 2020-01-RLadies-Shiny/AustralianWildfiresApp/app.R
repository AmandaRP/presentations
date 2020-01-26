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
      titlePanel("Climate: New South Wales, Australia"),
      p("A focused look at historic temperatures, rainfall, and current wildfires in 
        New South Wales, Australia"),
      titlePanel("Current Wildfires"),  
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
                             choices = c("Canberra" = "CANBERRA", 
                                         "Melbourne" = "MELBOURNE", 
                                         "Sydney" = "SYDNEY"),
                             selected = c("CANBERRA", "MELBOURNE", "SYDNEY"),
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
       
     #plot
     temp_avgs %>% 
       mutate(temp_minus_mean = avg_temp - overall_avg_temp) %>%
       ggplot(aes(year, temp_minus_mean, fill = temp_minus_mean<0)) + 
       geom_col() +
       labs(title = "Annual Temperature Above or Below the Average",
            x = element_blank(), 
            y = "Degrees Celcius",
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
         axis.title.y = element_text(color = "darkgrey"),
         legend.position = "none",
         plot.caption = element_text(color = "darkgrey")) 
     
   })
   
   output$rainPlot <- renderPlot({
     p + xlim(input$year[1], input$year[2]) +
       gghighlight(tolower(city_name) %in% tolower(input$city), 
                   use_group_by = FALSE,
                   use_direct_label = FALSE) 
 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

