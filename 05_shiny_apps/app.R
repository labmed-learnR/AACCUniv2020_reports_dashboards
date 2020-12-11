#
# You should notice there is no YAML metadata for a shiny app
#

library(shiny)
library(tidyverse)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Basic QC Chart"),
  

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateInput(inputId = "start", label = "Start Date:",
                        min = lubridate::today()-365,
                        max = lubridate::today(),
                        value = lubridate::today()-45),
            dateInput(inputId = "end", label = "End Date:",
                      min = lubridate::today()-365,
                      max = lubridate::today(),
                      value = lubridate::today()-15)
            ),

        # Show a plot of the LVchart
        mainPanel(
            plotOutput(outputId = "LVPlot"),
            
        )
    )
)


server <- function(input, output) {

    output$LVPlot <- renderPlot({
        # read in the data
        active_data <- read.csv("ShinyData.csv")
        active_data$date <- as.Date(active_data$date)
        #User input of the dates 
        active_data <- active_data %>%
            dplyr::filter(between(date, as.Date(input$start), as.Date(input$end)))
        
       active_data %>% 
            group_by(qc_code) %>%
            mutate(value_scaled = (value-mean(value))/sd(value)) %>%
            ggplot( aes(x = date, y = value_scaled, color = qc_code, ymin = -1, ymax = 1)) + 
            geom_ribbon( alpha = 0.3) +
            geom_ribbon(aes(ymin =-2, ymax = 2), alpha =0.2) +
            geom_ribbon(aes(ymin =-3, ymax = 3), alpha =0.1) +
            geom_line() + 
            geom_point() +
            ggtitle("Levey-Jennings Chart") +
            xlab("Date") +
            ylab("SD Scaled Values")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
