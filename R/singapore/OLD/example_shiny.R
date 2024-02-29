# Source: https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html

library(shiny)
runExample("01_hello")

#Shiny apps are contained in a single script called app.R. 
# The script app.R lives in a directory (for example, newdir/) and the app can be run with runApp("newdir").

#app.R has three components:
  
# 1. a user interface object

# 2. a server function

# 3. a call to the shinyApp function


# 1. The user interface (ui) object controls the layout and appearance of your app. 

ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# 2. The server function contains the instructions that your computer needs to build your app. 

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}


# 3. Finally the shinyApp function creates Shiny app objects from an explicit UI/server pair.

shinyApp(ui = ui, server = server)

# Run
# verify it is not working
runApp("example_shiny.R")
