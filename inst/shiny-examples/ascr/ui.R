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

      # Input: Select a file ----
      fileInput("file1", "Choose CSV File of trap locations",
                multiple = TRUE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
      
      # Input: Select a file ----
      fileInput("file2", "Choose CSV File of detections",
                multiple = TRUE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

      # Horizontal line ----
      tags$hr(),

      # Input: Checkbox if file has header ----
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

      # Horizontal line ----
      tags$hr(),

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      

    ),

    # Main panel for displaying outputs ----
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Data",
                             tabsetPanel(
                                 tabPanel("Traps",tableOutput("traps")),
                                 tabPanel("Detections", tableOutput("detections")))),
                    tabPanel("Mask",print("TODO Mask plot")),
                    tabPanel("Model",print("TODO Model params")),
                    tabPanel("Report",print("TODO report"))        
                    )
                    )
    )
  )
)

      # Output: Data file ----
        ## tableOutput("traps"),
        ## tableOutput("detections")

