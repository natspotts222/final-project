# Quest 2 (Correlation App)

library(shiny)
library(tidyverse)

# Define UI for application that draws a scatterplot
ui <- fluidPage(
   
   # Application title
   titlePanel("Correlation Demo"),
   
   # Sidebar
   # sidebarLayout contains information for both the sidebar and main panels
   sidebarLayout(
      sidebarPanel(
         # sidebarPanel includes the interactive parameter components
         sliderInput(inputId = "points", # creates input$points
                     label = "Number of points:",
                     min = 10,
                     max = 200,
                     value = 50),
         sliderInput(inputId = "slope", # creates input$slope
                     label = "Slope:",
                     min = -10,
                     max = 10,
                     value = 1),
         sliderInput(inputId = "error", # creates input$error
                     label = "Error:",
                     min = .001,
                     max = 5,
                     value = 0.5)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput(outputId = "scatterPlot") # creates output$scatterPlot
      )
   )
)

# Define server logic 
server <- function(input, output) {
   # We want the output to be a plot that takes in our input parameters
   output$scatterPlot <- renderPlot({

    x = runif(input$points) # makes x a uniform distribution of however many points you set it to
    y = rep(input$slope, input$points) * as.vector(x) + 
      rnorm(input$points, sd = input$error) # y = b1*x + e

    # Create dataframe with x and y
    df <- as.data.frame(cbind(x, y))
    
    # ggplot
    ggplot(df, aes(x, y)) +
       geom_point() +
       geom_smooth(method = "lm")
    
    # Base R plot -- keeping around for posterity
    # plot(x = x, y = y, xlab = "x", ylab = "y", main = "Scatterplot") 
   })
}

# Run the application, using the above defined ui and server parameters
shinyApp(ui = ui, server = server)