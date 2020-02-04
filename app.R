#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Iris Dataset (Sample for Niedner Mitacs)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        #implementing radio buttons
        radioButtons("p", 
                     "Select column of iris dataset:",
                     list("Sepal.Length"='a', 
                          "Sepal.Width"='b', 
                          "Petal.Length"='c', 
                          "Petal.Width"='d')
                     ),
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
       #referring input p in ui.r as input$p
       if(input$p=='a'){
         i<-1
       }
       if(input$p=='b'){
         i<-2
       }
       if(input$p=='c'){
         i<-3
       }
       if(input$p=='d'){
         i<-4
       }
       x    <- iris[, i] 
      # generate bins based on input$bins from ui.R
      # x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

