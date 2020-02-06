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
                     value = 30),
        sliderInput("n",
                    "Number of image:",
                    min = 1,
                    max = 10,
                    value = 1),
        fileInput("file1", "Choose JPG or PNG File",
                  multiple = FALSE,
                  accept = c("image/jpeg", 
                             "image/png",
                             "image/svg+xml"))
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput("distPlot")), #, textOutput("selected_var")
          tabPanel("Images", imageOutput("myImage")), 
          tabPanel("Browsed Images", imageOutput("browsedImage")) 
          # tabPanel("Table", tableOutput("table"))
        )
         # plotOutput("distPlot"),
         # Use imageOutput to place the image on the page
         # imageOutput("myImage")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
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
   
   output$myImage <- renderImage({
     # width  <- session$clientData$output_myImage_width
     # height <- session$clientData$output_myImage_height
     
     width  <- 500
     height <- 400
     
     # When input$n is 3, filename is ./images/image3.png
     filename <- normalizePath(file.path('./images',
                                         paste('image', input$n, '.jpg', sep='')))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = width,
          height = height,
          alt = paste("Image number", input$n))
     
   }, deleteFile = FALSE)
   
   output$browsedImage <- renderImage({
     # width  <- session$clientData$output_myImage_width
     # height <- session$clientData$output_myImage_height
     
     width  <- 500
     height <- 400
     
     # Return a list containing the filename and alt text
     list(src = input$file1$datapath,
          width = width,
          height = height,
          alt = paste("Random Image"))
     
   }, deleteFile = FALSE)
   
   output$selected_var <- renderText({ 
     input$file1$datapath
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

