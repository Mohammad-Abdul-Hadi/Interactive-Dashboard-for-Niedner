library(shiny)
library(shinyWidgets)
library(shinyBS)
library(DT)
library(sqldf)

elements <- read.csv(file.path("./Snaking/Snaking_tres_0.2.csv"), stringsAsFactors=FALSE)
str(elements)
snakingE1Vals <- unique(elements[,2])
snakingE2Vals <- unique(elements[,3])
snakingAlVals <- unique(elements[,4])
snakingFcVals <- unique(elements[,5])
snakingCountVals <- unique(elements[,6])
snakingLenVals <- unique(elements[,7])
snakingAmpVals <- unique(elements[,8])

Assembly_elems <- read.csv(file.path("./Assembly/Assembly.csv"), stringsAsFactors=FALSE)
str(Assembly_elems)
assemblyMEI1Vals <- unique(Assembly_elems[,2])
assemblyMEI2Vals <- unique(Assembly_elems[,3])
assemblySMIVals <- unique(Assembly_elems[,4])
assemblyMEO1Vals <- unique(Assembly_elems[,5])
assemblyMEO2Vals <- unique(Assembly_elems[,6])
assemblySMOVals <- unique(Assembly_elems[,7])
assemblyFCVals <- unique(Assembly_elems[,8])
assemblyELVals <- unique(Assembly_elems[,9])
assemblyImageVals <- unique(Assembly_elems[,10])
assemblyImage2Vals <- unique(Assembly_elems[,11])

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
    tags$head(tags$style(
      HTML('
           #sidebar {
              background-color: #ffffff;
          }
  
          body, label, input, button, select { 
            font-family: "Arial";
          }')
    )),
  
   # Application title
   #titlePanel("Visualization Tool (Niedner)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(id="sidebar",
        img(src = "niedner.jpg", width = "100%"),
        br(),
        br(),
        hr(),
        #implementing radio buttons

        
        bsCollapse(id = "sidebarCollapse", multiple = FALSE, open = NULL,
                   bsCollapsePanel("Snaking", value = 1,
                           selectInput("p", 
                                       "Output Variable:",
                                       c("Snaking Count"='o1', #Sepal.Length
                                         "Snaking Length (m)"='o2', #Sepal.Width
                                         "Snaking Amplitude (m)"='o3')
                           ),
                           selectInput("q", 
                                       "Input Variable:",
                                       c("Modulus (E0)- GPa"='i1', #Sepal.Length
                                         "Modulus (E1)- GPa"='i2', #Sepal.Width
                                         "Friction"='i3', #Petal.Length
                                         "Load (N/mm^2)"='i4' #Petal.Width
                                         )
                           ),
                           # radioButtons("p", 
                           #              "Output Variables:",
                           #              list("Snaking Count"='o1', #Sepal.Length
                           #                    "Snaking Length (m)"='o2', #Sepal.Width
                           #                    "Snaking Amplitude (m)"='o3')
                           #              ),
                           # radioButtons("q", 
                           #              "Input Variables:",
                           #              list("Modulus (E0)- GPa"='i1', #Sepal.Length
                           #                   "Modulus (E1)- GPa"='i2', #Sepal.Width
                           #                   "Friction"='i3', #Petal.Length
                           #                   "Load (N/mm^2)"='i4' #Petal.Width 
                           #              )),
                           # hr(),
                           sliderInput("bins",
                                       "Number of Divisions:",
                                       min = 1,
                                       max = 10,
                                       value = 7),
                           hr(),
                           hr(),

                           shinyWidgets::sliderTextInput(inputId = "n1", 
                                                         label = "Demo Image Slider", 
                                                         choices = c(1,2,3,4,5,6,7,8,9,10),
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                           hr(),
                           shinyWidgets::sliderTextInput(inputId = "n1_1", 
                                                         label = "Modulus of Elasticity - inner: (GPa)", 
                                                         choices = snakingE1Vals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                           # sliderInput("n1_1",
                           #            "Modulus of Elasticity - inner: (GPa)",
                           #            min = 0.08,
                           #            max = 0.22,
                           #            value = 0.08), 
                           
                           shinyWidgets::sliderTextInput(inputId = "n1_2", 
                                                         label = "Modulus of Elasticity - outer: (GPa)", 
                                                         choices = snakingE2Vals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                           # sliderInput("n1_2",
                           #             "Modulus of Elasticity - outer: (GPa)",
                           #             min = 0.03,
                           #             max = 0.15,
                           #             value = 0.03), 
                           
                           shinyWidgets::sliderTextInput(inputId = "n1_3", 
                                                         label = "Axial Load (Water Pressure Drop): (N/mm^2)", 
                                                         choices = snakingAlVals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                           # sliderInput("n1_3",
                           #             "Axial Load (Water Pressure Drop): (N/mm^2)",
                           #             min = 0.000033,
                           #             max = 0.00053,
                           #             value = 0.000033),
                           
                           shinyWidgets::sliderTextInput(inputId = "n1_4", 
                                                         label = "Friction Coefficient", 
                                                         choices = snakingFcVals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                           # sliderInput("n1_4",
                           #             "Friction Coefficient",
                           #             min = 0.000033,
                           #             max = 0.00053,
                           #             value = 0.000033), 
                           style = "info"),
                   bsCollapsePanel("Bunching",  value = 2,
                                   sliderInput("n2",
                                               "Demo image:",
                                               min = 1,
                                               max = 10,
                                               value = 1),
                                   hr(), 
                                   sliderInput("n2_1",
                                               "Modulus of Elasticity - inner - warp (GPa):",
                                               min = 0.08,
                                               max = 0.22,
                                               value = 0.08), 
                                   sliderInput("n2_2",
                                               "Modulus of Elasticity - inner - weft (GPa):",
                                               min = 0.08,
                                               max = 0.22,
                                               value = 0.08),
                                   sliderInput("n2_3",
                                               "Modulus of Elasticity - outer - warp (GPa):",
                                               min = 0.5,
                                               max = 1.5,
                                               value = 0.5),
                                   sliderInput("n2_4",
                                               "Modulus of Elasticity - outer - weft (GPa):",
                                               min = 0.5,
                                               max = 1.5,
                                               value = 0.5),
                                   sliderInput("n2_5",
                                               "Shear Modulus - inner (MPa):",
                                               min = 0.6,
                                               max = 1.4,
                                               value = 0.6),
                                   sliderInput("n2_6",
                                               "Shear Modulus - outer (MPa):",
                                               min = 0.6,
                                               max = 1.4,
                                               value = 0.6),
                                   sliderInput("n2_7",
                                               "Friction Coefficient:",
                                               min = 0.08,
                                               max = 0.2,
                                               value = 0.08),
                                   sliderInput("n2_8",
                                               "Axial Load (Water Pressure Drop): (N/mm^2):",
                                               min = 0.000033,
                                               max = 0.00053,
                                               value = 0.000033), style = "info"),
                   bsCollapsePanel("Assembly",  value = 3,
                                   sliderInput("n3",
                                               "Demo image:",
                                               min = 1,
                                               max = 10,
                                               value = 1),
                                   hr(), 
                                   shinyWidgets::sliderTextInput(inputId = "n3_1",
                                                         label = "Modulus of Elasticity - inner - warp (GPa):",
                                                         choices = assemblyMEI1Vals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE), 
                                   shinyWidgets::sliderTextInput(inputId = "n3_2",
                                                         label = "Modulus of Elasticity - inner - weft (GPa):",
                                                         choices = assemblyMEI2Vals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                                   shinyWidgets::sliderTextInput(inputId = "n3_3",
                                                         label = "Modulus of Elasticity - outer - warp (GPa):",
                                                         choices = assemblyMEO1Vals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                                   shinyWidgets::sliderTextInput(inputId = "n3_4",
                                                         label = "Modulus of Elasticity - outer - weft (GPa):",
                                                         choices = assemblyMEO2Vals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                                   shinyWidgets::sliderTextInput(inputId = "n3_5",
                                                         label = "Shear Modulus - inner (MPa):",
                                                         choices = assemblySMIVals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                                   shinyWidgets::sliderTextInput(inputId = "n3_6",
                                                         label = "Shear Modulus - outer (MPa):",
                                                         choices = assemblySMOVals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                                   shinyWidgets::sliderTextInput(inputId = "n3_7",
                                                         label = "Friction Coefficient:",
                                                         choices = assemblyFCVals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE), 
                                   style = "info")
        ),
        br(),
        hr(),
        selectInput("dataset", "Download a dataset:",
                    choices = c("Snaking", "Assembly")),

        # Button
        downloadButton("downloadData", "Download"),
        
        br(),
        hr(),
        fileInput("file1", "Choose JPG or PNG File",
                  multiple = FALSE,
                  accept = c("image/jpeg", 
                             "image/png",
                             "image/svg+xml")),
                  placeholder = "No file selected",
                  #verbatimTextOutput("clientdataText"),
                  width = 4
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id = "tabs",
          # tabPanel("Plot", plotOutput("distPlot")), #, textOutput("selected_var")
          tabPanel("Snaking", value = 1,
                   bsCollapse(id = "SnakingCollapse", multiple = TRUE, open = c(1,3),
                   bsCollapsePanel("Plot",  value = 1, # "This is a panel demo data plot",
                                   fluidPage( 
                                     fluidRow(
                                       column(6, plotOutput("distPlot")),
                                       column(6, plotOutput("distPlot2"))
                                     )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Demonstration", value = 2, # "This is a panel with just demo image ",
                                   fluidPage( 
                                     fluidRow(
                                       column(6, offset = 3, 
                                              imageOutput("myImage1", width="100%", height="100%", inline = FALSE))
                                     )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Snaking Images", value = 3,  # "This panel one image and one table",
                                   fluidPage( 
                                     fluidRow(
                                       column(12, #offset = 3, 
                                              imageOutput("myImage1_actual", width="100%", height="100%", inline = FALSE))#,
                                       # column(6, DT::dataTableOutput("mytable_snaking"))
                                     )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Snaking Data Table", value = 4, # "This panel contains one table.",
                                   fluidPage(
                                     fluidRow(
                                       column(6, offset = 3, DT::dataTableOutput("mytable_snaking"))
                                     )
                                   ),
                                   style = "primary"))
                   ), 
          tabPanel("Bunching",  value = 2,
                   bsCollapse(id = "SnakingCollapse", multiple = TRUE, open = c(2,3),
                   bsCollapsePanel("Demonstration",  value = 1,  "This is a panel with just demo image ",
                                   fluidPage( 
                                      fluidRow(
                                         column(6, offset = 3, imageOutput("myImage2"))
                                       )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Bunching Panel", value = 2, "This panel has three images. ",
                                   fluidPage(
                                       fluidRow(
                                         column(6, div(style = "width:session$clientData$output_myImage_2_1_width;
                                                       text-align:center;", 
                                                       imageOutput("myImage2_1"))),
                                         column(6, div(style = "width:session$clientData$output_myImage_2_2_width;
                                                       text-align:center;", 
                                                       imageOutput("myImage2_2")))
                                       ),
                                       fluidRow(
                                         column(6, offset = 3, div(style = "width:session$clientData$output_myImage_2_3_width;
                                                                   text-align:center;", 
                                                                   imageOutput("myImage2_3")))
                                       )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Table",  value = 3, "This panel contains one table.",
                                   fluidPage(
                                     fluidRow(
                                       column(6, offset = 3, DT::dataTableOutput("mytable_bunching"))
                                     )
                                   ),
                                   style = "primary"))
                  ),
          tabPanel("Assembly",  value = 3,
                   bsCollapse(id = "SnakingCollapse", multiple = TRUE, open = c(2,3),
                   bsCollapsePanel("Demonstration", value = 1, "This is a panel with just demo image ",
                                   fluidPage( 
                                     fluidRow(
                                       column(6, offset = 3, imageOutput("myImage3"))
                                     )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Assembly Image", value = 2, "This panel loads the assembly image ",
                                   fluidPage( 
                                     fluidRow(
                                       column(6, offset = 3, imageOutput("myImage3_actual"))
                                     )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Table", value = 3, "This panel contains one table.",
                                   fluidPage(
                                     fluidRow(
                                       column(6, offset = 3, DT::dataTableOutput("mytable_assembly"))
                                     )
                                   ),
                                   style = "primary"))
          ),
          tabPanel("Analytical Visualization",  value = 4,
                   bsCollapse(id = "SnakingCollapse", multiple = TRUE, open = 1,
                   bsCollapsePanel("Plot", value = 1, "This is a panel with one Statistical Analysis Plot",
                                   fluidPage( 
                                     fluidRow(
                                       column(6, offset = 3, imageOutput("myImageStat"))
                                     )
                                   ),
                                   style = "primary"))
          ),
          tabPanel("Browsed Images",  value = 5, imageOutput("browsedImage"))
          # tabPanel("Table", tableOutput("table"))
        )#,
        # width = 8
         # plotOutput("distPlot"),
         # Use imageOutput to place the image on the page
         # imageOutput("myImage")
      )
   )
))



# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
    cdata <- session$clientData
    
    observeEvent(input$tabs, ({
      updateCollapse(session, "sidebarCollapse", open = input$tabs)
    }))
    
    observeEvent(input$sidebarCollapse, ({
      choice <- input$sidebarCollapse
      updateTabsetPanel(session, "tabs", selected = choice)
    }))
    
    output$clientdataText <- renderText({
      paste(      session$clientData$output_myImage2_1_width,
                  session$clientData$output_myImage2_1_height,
                  session$clientData$output_myImage2_2_width,
                  session$clientData$output_myImage2_2_height,
                  session$clientData$output_myImage2_3_width,
                  session$clientData$output_myImage2_3_height,
                  input$sidebarCollapse ,sep = ' , ')
    })
   
    output$distPlot <- renderPlot({
       #referring input p in ui.r as input$p
       if(input$p=='o1'){
         i<-6
         xLabel<- "Snaking Count"
       }
       if(input$p=='o2'){
         i<-7
         xLabel<- "Global Snaking Length (m)"
       }
       if(input$p=='o3'){
         i<-8
         xLabel<- "Maximum Snaking Amplitude (m)"
       }
       x    <- elements[, i] 
      # generate bins based on input$bins from ui.R
      # x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x,
           main="Histogram for Niedner Data", 
           xlab=xLabel, 
           breaks = bins, 
           col = 'RoyalBlue', 
           border = 'white')
    })
    
    output$distPlot2 <- renderPlot({
     #referring input p in ui.r as input$p
     if(input$q=='i1'){
       j<-2
       xLabel<- "Modulus (E0)- GPa"
     }
     if(input$q=='i2'){
       j<-3
       xLabel<- "Modulus (E1)- GPa"
     }
     if(input$q=='i3'){
       j<-4
       xLabel<- "Friction"
     }
     if(input$q=='i4'){
       j<-5
       xLabel<-"Load (N/mm^2)"
     }
     # generate bins based on input$bins from ui.R
     # x    <- faithful[, 2] 
      if(input$p=='o1'){
        elements$out <- elements$count
        yLabel <- "Sanking Count"
      }
      if(input$p=='o2'){
        elements$out <- elements$len
        yLabel <- "Global Sanking Length (m)"
      }
      if(input$p=='o3'){
        elements$out <- elements$amp
        yLabel <- "Maximum Sanking Amplitude (m)"
      }
     # dfc <- cut(elements$out, breaks=c(min(elements$out),
     #                                    (max(elements$out)-min(elements$out))/3,
     #                                    ((max(elements$out)-min(elements$out))/3)*2,
     #                                    max(elements$out)), 
     #                                    #labels=paste("Label", 1:3, sep="")
     #            )
     # elements$out <- dfc
      

     boxplot(elements$out~elements[, j],
          data = elements,
          main="BoxPlot for Niedner Data", 
          # names=c("Label 1","Label 2","Label 3"),
          col = "RoyalBlue",
          xlab = xLabel,
          ylab = yLabel,
          boxwex = 0.3,
          notch = T,
          frame=F)
    })
    
    output$myImage1 <- renderImage({
     #width  <- session$clientData$output_myImage1_width
     #height <- session$clientData$output_myImage1_height
     
     # When input$n is 3, filename is ./images/image3.png
     filename <- normalizePath(file.path('./images',
                                         paste('image', input$n1, '.jpg', sep='')))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = "100%",
          #height = "100%",
          alt = paste("Image number", input$n))
    }, deleteFile = FALSE)
    
    output$myImage1_actual <- renderImage({
     #width  <- session$clientData$output_myImage1_actual_width
     #height <- session$clientData$output_myImage1_actual_height
     
     capture <- subset(elements, (e1 == input$n1_1 & e2 == input$n1_2 & load == input$n1_3 & fc == input$n1_4), select=c(image))
     
     filename <- normalizePath(file.path('./Snaking/Pictures', capture[1,1]))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = "100%",
          #height = "100%",
          alt = paste("Image name:",
                      capture[1,1],
                      'could not be found', sep =' '))
     
    }, deleteFile = FALSE)

    output$myImage3_actual <- renderImage({
     #width  <- session$clientData$output_myImage3_actual_width
     #height <- session$clientData$output_myImage3_actual_height

     capture <- subset(Assembly_elems, (mei1 == input$n3_1 & mei2 == input$n3_2 & meo1 == input$n3_3 & meo2 == input$n3_4 & smi == input$n3_5 & smo == input$n3_6 & 
                                        fc == input$n3_7), select=c(SI))
     
     filename <- normalizePath(file.path('./Assembly/Images', paste(capture[1,1], "_image.jpg", sep="")))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = "100%",
          #height = "100%",
          alt = paste("Image name:",
                      paste(capture[1,1], "_image.jpg", sep=""),
                      'could not be found', sep =' '))
     
    }, deleteFile = FALSE)
    
    output$mytable_snaking = DT::renderDataTable({
     count_out <- subset(elements, (e1 == input$n1_1 & e2 == input$n1_2 & load == input$n1_3 & fc == input$n1_4), select=c(count))
     len_out <- subset(elements, (e1 == input$n1_1 & e2 == input$n1_2 & load == input$n1_3 & fc == input$n1_4), select=c(len))
     amp_out <- subset(elements, (e1 == input$n1_1 & e2 == input$n1_2 & load == input$n1_3 & fc == input$n1_4), select=c(amp))
     my_data <- data.frame(
       Variables =c("Modulus of elasticity - inner (GPa)", "Modulus of elasticity - outer(GPa)", "Axial Load (N/mm^2)", "Friction Coefficient", "Snaking (count)", "Global Snaking Length (m)", "Maximum Snaking Amplitude (m)"),
       Values = c(input$n1_1, input$n1_2, input$n1_3, input$n1_4, count_out[1,1], len_out[1,1], amp_out[1,1])
     )
     
     datatable(my_data, options = list(dom = 't')) %>% formatStyle(
       'Variables',
       target = 'row',
       backgroundColor = styleEqual(c("Snaking (count)", "Global Snaking Length (m)", "Maximum Snaking Amplitude (m)"), c('lightblue', 'lightblue', 'lightblue'))
     )
    })

    # Reactive value for selected dataset ----
    datasetInput <- reactive({
        switch(input$dataset,
              "Sanking" = elements,
              "Assembly" = Assembly_elems)
      })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )

    output$mytable_assembly = DT::renderDataTable({
     el_out <- subset(Assembly_elems, (mei1 == input$n3_1 & mei2 == input$n3_2 & meo1 == input$n3_3 & meo2 == input$n3_4 & smi == input$n3_5 & smo == input$n3_6 & 
                                        fc == input$n3_7), select=c(el))
     my_data <- data.frame(
       Variables =c("Modulus of elasticity – inner – warp", "Modulus of elasticity – inner – weft", "Modulus of elasticity – outer – warp", "Modulus of elasticity – outer – weft", "Shear modulus - inner", "Shear modulus - outer", "Friction coefficient", "Excessive Length (Output)"),
       Values = c(input$n3_1, input$n3_2, input$n3_3, input$n3_4, input$n3_5, input$n3_6, input$n3_7, el_out[1,1])
     )
     
     datatable(my_data, options = list(dom = 't')) %>% formatStyle(
       'Variables',
       target = 'row',
       backgroundColor = styleEqual(c("Excessive Length (Output)"), c('lightblue'))
     )
    })
    
    output$mytable_bunching = DT::renderDataTable({
     my_data <- data.frame(
       Variables =c("Modulus of elasticity – inner – warp", "Modulus of elasticity – inner – weft", "Modulus of elasticity – outer – warp", "Modulus of elasticity – outer – weft", "Shear modulus - inner", "Shear modulus - outer", "Friction coefficient", "Axial load (water pressure drop)", "Location of maximum bunching (Output)"),
       Values = c(input$n2_1, input$n2_2, input$n2_3, input$n2_4, input$n2_5, input$n2_6, input$n2_7, input$n2_8, "ND")
     )
     
     datatable(my_data) %>% formatStyle(
       'Variables',
       target = 'row',
       backgroundColor = styleEqual(c("Location of maximum bunching (Output)"), c('lightblue'))
     )
    })
    
    
    output$myImage2 <- renderImage({
     width  <- session$clientData$output_myImage2_width
     height <- session$clientData$output_myImage2_height
     
     # When input$n is 3, filename is ./images/image3.png
     filename <- normalizePath(file.path('./images',
                                         paste('bunching', input$n2, '.jpg', sep='')))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = width,
          height = height,
          alt = paste("Image number", input$n2))
     
    }, deleteFile = FALSE)
    
    output$myImage2_1 <- renderImage({
     width  <- session$clientData$output_myImage2_1_width
     height <- session$clientData$output_myImage2_1_height
     
     filename <- normalizePath(file.path('./images',
                                         paste('bunching_e0_', input$n2_1, 
                                               '_e1_', input$n2_2,
                                               '_e2_', input$n2_3,
                                               '_e3_', input$n2_4,
                                               '_s0_', input$n2_5,
                                               '_s1_', input$n2_6,
                                               '_fric_', input$n2_7,
                                               '_load_', input$n2_8,
                                               '_ss1',
                                               '.jpg', sep='')))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = width/1.3,
          height = height/1.3,
          alt = paste("Image name: ",
                      'bunching_e0_', input$n2_1, 
                      '_e1_', input$n2_2,
                      '_e2_', input$n2_3,
                      '_e3_', input$n2_4,
                      '_s0_', input$n2_5,
                      '_s1_', input$n2_6,
                      '_fric_', input$n2_7,
                      '_load_', input$n2_8,
                      '_ss1',
                      '.jpg Not generated', sep=''))
     
    }, deleteFile = FALSE)
    
    
    output$myImage2_2 <- renderImage({
     width  <- session$clientData$output_myImage2_2_width
     height <- session$clientData$output_myImage2_2_height
     
     # When input$n is 3, filename is ./images/image3.png
     filename <- normalizePath(file.path('./images',
                                         paste('bunching_e0_', input$n2_1, 
                                               '_e1_', input$n2_2,
                                               '_e2_', input$n2_3,
                                               '_e3_', input$n2_4,
                                               '_s0_', input$n2_5,
                                               '_s1_', input$n2_6,
                                               '_fric_', input$n2_7,
                                               '_load_', input$n2_8,
                                               '_ss2',
                                               '.jpg', sep='')))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = width/1.3,
          height = height/1.3,
          alt = paste("Image name: ",
                      'bunching_e0_', input$n2_1, 
                      '_e1_', input$n2_2,
                      '_e2_', input$n2_3,
                      '_e3_', input$n2_4,
                      '_s0_', input$n2_5,
                      '_s1_', input$n2_6,
                      '_fric_', input$n2_7,
                      '_load_', input$n2_8,
                      '_ss2',
                      '.jpg Not generated', sep=''))
     
    }, deleteFile = FALSE)
    
    output$myImage2_3 <- renderImage({
     width  <- session$clientData$output_myImage2_3_width
     height <- session$clientData$output_myImage2_3_height
     
     # When input$n is 3, filename is ./images/image3.png
     filename <- normalizePath(file.path('./images',
                                         paste('bunching_e0_', input$n2_1, 
                                               '_e1_', input$n2_2,
                                               '_e2_', input$n2_3,
                                               '_e3_', input$n2_4,
                                               '_s0_', input$n2_5,
                                               '_s1_', input$n2_6,
                                               '_fric_', input$n2_7,
                                               '_load_', input$n2_8,
                                               '_ss3',
                                               '.jpg', sep='')))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = width/1.3,
          height = height/1.3,
          alt = paste("Image name: ",
                      'bunching_e0_', input$n2_1, 
                      '_e1_', input$n2_2,
                      '_e2_', input$n2_3,
                      '_e3_', input$n2_4,
                      '_s0_', input$n2_5,
                      '_s1_', input$n2_6,
                      '_fric_', input$n2_7,
                      '_load_', input$n2_8,
                      '_ss3',
                      '.jpg Not generated', sep=''))
     
    }, deleteFile = FALSE)
    
    
    output$myImage3 <- renderImage({
      width  <- session$clientData$output_myImage3_width
      height <- session$clientData$output_myImage3_height
     
     # When input$n is 3, filename is ./images/image3.png
     filename <- normalizePath(file.path('./images',
                                         paste('assembly', input$n3, '.jpg', sep='')))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = width,
          height = height,
          alt = paste("Image number", input$n))
     
    }, deleteFile = FALSE)
    
    
    output$browsedImage <- renderImage({
     # width  <- session$clientData$output_myImage_width
     # height <- session$clientData$output_myImage_height
     
     inFile <- input$file1
     capture <- subset(elements, (e1 == input$n1_1 & e2 == input$n1_2 & load == input$n1_3 & fc == input$n1_4), select=c(image))
     filename <- normalizePath(file.path('./Snaking/Pictures', capture[1,1]))
     
     if(is.null(inFile))
       inFile$datapath <- filename
     
     # Return a list containing the filename and alt text
     list(src = inFile$datapath,
          alt = paste("Random Image"))
     
    }, deleteFile = FALSE)
    
    output$myImageStat <- renderImage({
     width  <- session$clientData$output_myImageStat_width
     height <- session$clientData$output_myImageStat_height
     
     # When input$n is 3, filename is ./images/image3.png
     filename <- normalizePath(file.path('./images/stat_analysis',
                                         paste('bunching.png')))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = width,
          height = height,
          alt = paste("Image name: bunching.png Not generated"))
     
    }, deleteFile = FALSE)
    
    output$selected_var <- renderText({ 
     input$file1$datapath
    })
})

# Run the application 
shinyApp(ui = ui, server = server)

