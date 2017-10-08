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
    selectInput("select", label = h3("Chose a detection function"), 
                choices = list("halfnormal" = 'hn', "hazard rate" = 'hr', "threshold" = 'th', "log-link threshold" = 'lth'), 
                selected = "hn")
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
                    tabPanel("Model",tableOutput("coefs")),
                    tabPanel("Report",print("TODO report"))        
                    )
    )
  )
))
