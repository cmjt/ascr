library(shinycssloaders)

shinyUI(fluidPage(
    
  # App title ----
    titlePanel("acoustic spatial capture-recapture (ascr)"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
        h3(tags$b("Read in data")),
        checkboxInput("example", "Load single trap example data"), # example
                                        # Input: Select a csv file of trap locations
        fileInput("file1", "Choose CSV file of trap locations",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        fileInput("file2", "Choose CSV file of detections",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
                                        # Input: Checkbox if file1 has header ----
        checkboxInput("header", "Header", TRUE),

                                        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ",",inline = TRUE),

                                        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"',inline = TRUE),


                                        # Input: Select number of rows to display ----
        radioButtons("disp", "Display",
                     choices = c( All = "all",
                                 Head = "head"),
                     selected = "all",inline = TRUE),
                                        
        h3(tags$b("Build mask")),
         # Input: integer of mask buffer in meters (this is updated based on trap info when file is loaded)
        sliderInput("buffer", "Choose mask buffer (m):",
                    min = 1, max = 10000,
                    value = 1000),
         # Input: integer of mask spacing in meters (this is updated based on trap info when file is loaded)
        sliderInput("spacing", "Choose mask spacing (m):",
                    min = 1, max = 1000,
                    value = 250),
        downloadButton('downloadMask', 'Mask Plot'),
                                        # horizontal lines before model options,
        
        h3(tags$b("Modelling")),
                                        # select box for detetion functions
        selectInput("select", label = "Chose a detection function", 
                    choices = list("halfnormal" = 'hn', "hazard rate" = 'hr', "threshold" = 'th'), 
                    selected = "hn"),
                                        # check box conditional on value of detfn chosen
        uiOutput("fixedParamSelection"),
                                        # fix g0 to what value
        uiOutput("fixedg0"),
                                        # fix sigma to what value
        uiOutput("fixedsigma"),
                                        # fix z to what value
        uiOutput("fixedz"),
                                        # fix shape to what value
        uiOutput("fixedshape"),
                                        # fix scale to what value
        uiOutput("fixedscale"),
                                        # fix shape.1 to what value
        uiOutput("fixedshape.1"),
                                        # fix shape.2 to what value
        uiOutput("fixedshape.2"),
                                        # horizontal lines before choosing call number for estimated group location
        
        
        actionButton("fit", "Fit model"),
        hr(),
        numericInput("call.num", "Choose call number to display in estimated location plot:",
                     min = 1, max = 1000,step = 1,
                     value = 1),
        downloadButton('downloadSurfPlot', 'Detection surface plot'),
        downloadButton('downloadContPlot', 'Detection contour plot'),
        downloadButton('downloadDetPlot', 'Detection function plot'),                               
        h3(tags$b("Other")),
        checkboxInput("advanced", "Show advanced options"),
        conditionalPanel(
            condition = "input.advanced == true",
            checkboxGroupInput("advancedOptions", "Advanced options",
                               choices = list("bearings in degrees (default radians)" = "bd","increase mask buffer"  = "inc",
                                              "chose parameter starting values" = "sv",
                                              "build finer mask for plotting" = "fine"),inline = TRUE),
            conditionalPanel(
                condition = "input.advancedOptions.includes('chose parameter starting values')",
                uiOutput("startParamSelection"),
                uiOutput("svg0"), # chose g0 sv
                uiOutput("svsigma"), # chose sigma sv
                uiOutput("svz"), # chose z sv
                uiOutput("svshape"), # chose shape sv
                uiOutput("svscale"), # chose scale sv
                uiOutput("svshape.1"), # chose shape.1 sv
                uiOutput("svshape.2") # chose shape.2 sv
            ),
            conditionalPanel(
                condition = "input.advancedOptions.includes('increase mask buffer')",
                numericInput("incmaskbuffer","Chose higher bound for the mask buffer",
                             min = 1, max = 10000000,step = 1,
                             value = 1000)
            ),
            conditionalPanel(
                condition = "input.advancedOptions.includes('build finer mask for plotting')",
                numericInput("plotmaskspacing","Chose mask spacing (plotting purposes only)",
                             min = 1, max = 10000000,step = 1,
                             value = 250)
            ),
            downloadButton('downloadModel', 'Save Model .RData file')
        ),
        downloadButton("report", "Generate Basic Report"),
        numericInput("anispeed","Chose speed of animation for report",
                     min = 0.1,max = 5,step = 0.1,
                     value = 1)
        
    ),
                                        # Main panel for displaying outputs ----
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Data",
                             tabsetPanel(
                                 tabPanel("Traps",
                                          fluidRow(
                                              column(width = 4,
                                                     h4("Raw data"),
                                                     tableOutput("traps")),
                                              column(width = 4, 
                                                     h4("Trap locations"),
                                                     plotOutput("trapsPlot"))
                                          )),
                                 tabPanel("Detections",
                                          fluidRow(
                                              column(width = 4,
                                                     h4("Raw data"),
                                                     tableOutput("detections")),
                                              column(width = 4, 
                                                     h4("Capture history matrix"),
                                                     tableOutput("capt.hist")))))),
                    tabPanel("Mask",
                             column(width = 12, align="center",
                                    withSpinner(plotOutput("maskPlot"),type = 5,color = "#D3D3D3"))
                            ),
                    tabPanel("Model",
                             fluidRow(
                                 column(width = 3,
                                        h4("Parameter values"),
                                         withSpinner(tableOutput("coefs"),type = 5,color = "#D3D3D3")),
                                 column(width = 3,
                                        h4("Model info"),
                                         withSpinner(tableOutput("AIClL"),type = 5,color = "#D3D3D3")),
                                 column(width = 6,
                                        h4("Detection function"),
                                         withSpinner(plotOutput("detfn"),type = 5,color = "#D3D3D3"))
                             ),
                             fluidRow(
                                 h4("Detection surface"),
                                  withSpinner(plotOutput("detectionsurf"),type = 5,color = "#D3D3D3")
                             ),
                             fluidRow(
                                 h4("Location estimates"),
                                 column(12, align="center",
                                         withSpinner(plotOutput("locs"),type = 5,color = "#D3D3D3")
                                        )
                             )
                    ))
    ))
))
