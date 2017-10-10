#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


shinyUI(fluidPage(

  # App title ----
  titlePanel("acoustic spatial capture-recapture (ascr)"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a csv file of trap locations
      fileInput("file1", "Choose CSV File of trap locations",
                multiple = TRUE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
      
      # Input: Select a csv file of detections
      fileInput("file2", "Choose CSV File of detections",
                multiple = TRUE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

      # Horizontal line to separate file choice from options
      tags$hr(),

      # Input: Checkbox if file1 has header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),

      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),


      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
    # Two horizontal lines before mask options
    tags$hr(),
    tags$hr(),
    # Input: integer of mask buffer in meters
    sliderInput("buffer", "Choose mask buffer (m):",
                min = 0, max = 10000,
                value = 1000),
    # Input: integer of mask spacing in meters
    sliderInput("spacing", "Choose mask spacing (m):",
                min = 0, max = 1000,
                value = 250),
    # Two horizontal lines before model options
    tags$hr(),
    tags$hr(),
    # select box for detetion functions
    selectInput("select", label = "Chose a detection function", 
                choices = list("halfnormal" = 'hn', "hazard rate" = 'hr', "threshold" = 'th', "log-link threshold" = 'lth'), 
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
     # Two horizontal lines before choosing call number for estimated group location
    tags$hr(),
    tags$hr(),
    # Input: integer of call number to estimate group location 
    numericInput("call.num", "Choose call number:",
                min = 1, max = 1000,step = 1,
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
                                                     h2(""),
                                                     tableOutput("traps")),
                                              column(width = 4,
                                                     h2(""),
                                                     plotOutput("trapsPlot"))
                                          )),
                                 tabPanel("Detections", tableOutput("detections")))),
                    tabPanel("Mask",plotOutput("maskPlot")),
                    tabPanel("Model",
                             fluidRow(
                                 column(width = 3,
                                        "Parameter estimates",
                                        tableOutput("coefs")),
                                 column(width = 4,
                                        "Detection surface",
                                        plotOutput("detectionPlot")),
                                 column(width = 5,
                                        "Estimated location of chosen call",
                                        plotOutput("locationPlot"))
                                 )),
                    tabPanel("Report",print("TODO report"))        
                    )
    )
  )
))
